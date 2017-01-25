-module(spry_backend).
-author("Christopher S. Meiklejohn <christopher.meiklejohn@gmail.com>").

-behaviour(gen_server).

-export([start_link/0,
         declare/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type object_id() :: binary().
-type origin() :: node().

%% State record
-record(state, {}).

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
    true = ets:insert(?MODULE, {ObjectId, Origin}),
    {reply, ok, State};
handle_call(Msg, _From, State) ->
    _ = lager:warning("Unhandled messages: ~p", [Msg]),
    {reply, ok, State}.

%% @private
handle_cast(Msg, State) ->
    _ = lager:warning("Unhandled messages: ~p", [Msg]),
    {noreply, State}.

%% @private
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
