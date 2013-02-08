-module(pingapp_wm_ping).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, nostate}.

to_html(ReqData, Context) ->
    Result = io_lib:format("Result: ~p", [pingapp:ping()]),
    {"<html><head><title>pingapp</title></head><body>" ++ Result ++ "</body></html>", ReqData, Context}.
