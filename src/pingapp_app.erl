-module(pingapp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case pingapp_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, pingapp_vnode}]),
            
            ok = riak_core_ring_events:add_guarded_handler(pingapp_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(pingapp_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(pingapp, self()),

            EntryRoute = {["pingapp", "ping"], pingapp_wm_ping, []},
            webmachine_router:add_route(EntryRoute),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
