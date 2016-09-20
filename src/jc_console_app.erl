-module(jc_console_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case application:get_env(jc_console, port, false) of
        false ->
            lager:warning("~p: no port configured for admin", [?MODULE]),
            {error, bad_configuration};
        Port ->
            application:start(cowboy),
            Dispatch = cowboy_router:compile([
		{'_', [{"/summary", summary_handler, []},
                       {"/map/:map", map_handler, []}]}
                                             ]),
            {ok, _} = cowboy:start_http(http, 10, [{port, Port}], 
                                        [{env, [{dispatch, Dispatch}]}]),
            lager:info("Admin service up and listening on port ~p", 
                       [Port]),
            jc_console_sup:start_link()
    end.

stop(_State) ->
    ok.
