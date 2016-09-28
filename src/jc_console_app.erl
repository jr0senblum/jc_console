%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011-2017, Jim Rosenblum
%%% @doc Application module for the web-based j_cache console. It assumes 
%%% that it is invoked by j_cache via application:start(jc_console). 
%%% 
%%% Because, this application gets its port configuration from the jc sys.config
%%% file, j_cache must have been started so that that configuration is available
%%% to this module at run time.
%%%
%%% @version {@version}
%%% @end
%%% Created : 16 Sept 2016 by Jim Rosenblum
%%% ----------------------------------------------------------------------------

-module(jc_console_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).



%% ===================================================================
%% Application callbacks
%% ===================================================================


-spec start (normal | {takeover   | failover, atom()}, [{node, atom()}]) -> 
   		      {ok, pid()} | {error, atom()}.

start(_StartType, _StartArgs) ->
    case application:get_env(jc, port, false) of
        false ->
            lager:warning("~p: no port configured for admin", [?MODULE]),
            {error, bad_configuration};
        Port ->
            application:start(cowboy),
            Dispatch = cowboy_router:compile([
		{'_', [% Random handler goes away as soon as UI dev doesn't need it.
                       {"/api/command", command_handler, []},
                       {"/api/summary", summary_handler, []},
                       {"/api/map/:map", map_handler, []},
                       {"/api/eventsource/map/:map", lasse_handler, [eventsource_handler]},
                       {'_', cowboy_static, {priv_file, jc, "index.html"}}]}]),
            {ok, _} = cowboy:start_http(http, 10, [{port, Port}], 
                                        [{env, [{dispatch, Dispatch}]}]),
            lager:info("Admin service up and listening on port ~p", 
                       [Port]),
            jc_console_sup:start_link()
    end.


-spec stop(term()) -> ok.
stop(_State) ->
    ok.
