%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011 - 2017, Jim Rosenblum
%%% @doc This is the http, RESTFUL handler used by cowboy to provide cache-line
%%% information.
%%%
%%% @version {@version}
%%% @end
%%% Created : 19 September 2016 by Jim Rosenblum
%%% ----------------------------------------------------------------------------

-module(map_handler).


% Handler call-backs.
-export([init/3, 
         rest_init/2, 
         terminate/3]).

% RESTful call-backs.
-export([allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         map_to_json/2]).


%%% ============================================================================
%%$                    handler required call-backs
%%% ============================================================================


init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

terminate(_Reason, _Req, _State) ->
	ok.

rest_init(Req, _Opts) ->
    {ok, Req, {}}.



%%% ============================================================================
%%$                    RESTful call-backs : GET
%%% ============================================================================


allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.


% For GET, HEAD, POST, PUT, PATCH, DELETE, the resource types provided and the
% call-back used.
content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, map_to_json}
     ], Req, State}.


resource_exists(Req, _State) ->
    {Map, Req2} = cowboy_req:binding(map, Req),
    try binary_to_existing_atom(Map, utf8) of
        Atom  ->
            {true, Req2, Atom}
    catch
        _:_ ->
            {false, Req2, {}}
    end.

% Need the map type from the url so we now how to convert the binding to the
% actual jcache map term. Then use that to get the map_size information.
map_to_json(Req, Map) ->
    Body = jsonx:encode(to_prop_list(jc:map_size(Map))),
    {Body, Req, {}}.

to_prop_list({records, R}) ->
    [{records, R}].
    
               


