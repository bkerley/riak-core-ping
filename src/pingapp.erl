-module(pingapp).
-include("pingapp.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0,
         hello/0
        ]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    sync_spawn_command(ping).

hello() ->
    sync_spawn_command(hello).

%% private

sync_spawn_command(Command) ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, pingapp),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, Command, pingapp_vnode_master).
