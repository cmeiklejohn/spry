-module(spry_backend).
-author("Christopher S. Meiklejohn <christopher.meiklejohn@gmail.com>").

-behaviour(gen_server).

-export([start_link/0,
         declare/2,
         reduce/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type object_id() :: binary().
-type origin() :: node().
-type constraint() :: {latency, non_neg_integer()}.
-type constraints() :: [constraint()].

%% State record
-record(state, {}).

%% Object record.
-record(object, {origin, value}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start and link to calling process.
-spec start_link()-> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Declare a new object identifier with an origin.
-spec declare(object_id(), origin()) -> ok.
declare(ObjectId, Origin) ->
    gen_server:call(?MODULE,
                    {declare, ObjectId, Origin},
                    infinity).

%% @doc Reduce a value from reference to value.
-spec reduce(object_id(), constraints()) -> {ok, term()}.
reduce(ObjectId, Constraint) ->
    gen_server:call(?MODULE,
                    {reduce, ObjectId, Constraint},
                    infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    ?MODULE = ets:new(?MODULE, [set, named_table]),
    {ok, #state{}}.

%% @private
handle_call({declare, ObjectId, Origin}, _From, State) ->
    lager:info("Declaring ~p with origin ~p", [ObjectId, Origin]),
    true = ets:insert(?MODULE, {ObjectId,
                                #object{origin=Origin}}),
    {reply, ok, State};

handle_call({reduce, ObjectId,
             [{latency, Milliseconds}]}, From, State) ->
    lager:info("Reducing ~p with latency bound of ~p ms",
               [ObjectId, Milliseconds]),

    %% Retrieve object origin.
    [{_, #object{origin=Origin}}] = ets:lookup(?MODULE, ObjectId),

    %% Set timer to retrieve the value from the other node.
    timer:send_after(Milliseconds,
                     {reduce_timeout, From, Milliseconds, ObjectId}),

    %% Issue request for the value.
    Self = node(),
    gen_server:cast({?MODULE, Origin}, {refresh, Self, ObjectId}),

    {noreply, State};

handle_call(Msg, _From, State) ->
    _ = lager:warning("Unhandled messages: ~p", [Msg]),
    {reply, ok, State}.

%% @private
handle_cast({refresh, From, ObjectId}, State) ->
    Self = node(),
    lager:info("Received refresh from ~p", [Self]),

    %% Retrieve object origin.
    [{ObjectId, Object}] = ets:lookup(?MODULE, ObjectId),

    %% Reply.
    gen_server:cast({?MODULE, From}, {value, ObjectId, Object}),

    {noreply, State};

handle_cast({value, ObjectId, Object}, State) ->
    lager:info("Refreshed value received: ~p", [ObjectId]),

    %% Store new value.
    true = ets:insert(?MODULE, {ObjectId, Object}),

    {noreply, State};

handle_cast(Msg, State) ->
    _ = lager:warning("Unhandled messages: ~p", [Msg]),
    {noreply, State}.

%% @private
handle_info({reduce_timeout, From, Milliseconds, ObjectId}, State) ->
    lager:info("Latency timeout: ~p for ~p",
               [Milliseconds, ObjectId]),

    %% Retrieve current value.
    [{_, #object{value=Value}}] = ets:lookup(?MODULE, ObjectId),

    lager:info("Returning value is: ~p", [Value]),

    %% Respond back to the waiting clients.
    gen_server:reply(From, {ok, Value}),

    {noreply, State};
handle_info(Msg, State) ->
    _ = lager:warning("Unhandled messages: ~p", [Msg]),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
