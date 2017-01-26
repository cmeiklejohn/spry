-module(spry_SUITE).
-author("Christopher S. Meiklejohn <christopher.meiklejohn@gmail.com>").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0,
         groups/0,
         % init_per_suite/1,
         % end_per_suite/1,
         % init_per_group/2,
         % end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2
        ]).

-export([basic/1]).

all() ->
    [basic].

groups() ->
    [].

init_per_testcase(_Name, Config) ->
    {ok, _Apps} = application:ensure_all_started(spry),
    Config.

end_per_testcase(_Name, _Config) ->
    ok.

basic(_Config) ->
    Node = node(),
    Value = <<"this is a value">>,
    ObjectId = <<"123">>,
    Constraints = [{latency, 100}],
    ok = spry_backend:declare(ObjectId, Node),
    ok = spry_backend:bind(ObjectId, Value),
    {ok, Value} = spry_backend:reduce(ObjectId, Constraints).
