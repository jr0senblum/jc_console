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


% Need the map type from the url so we now how to convert the binding to the
% actual jcache map term. Then use that to get the map_size information.
map_to_json(Req, State) ->
    {Type, Req2} = cowboy_req:binding(type, Req),
    {Map, Req3} = cowboy_req:binding(map, Req2),
    RealMap = decode(Type, Map),
    Body = jsonx:encode(to_prop_list(jc:map_size(RealMap))),
    {Body, Req3, State}.


% Convert a Map binary to the correct jc term using the type to know what to do.
decode(<<"a">>, Map) ->
    binary_to_existing_atom(Map, utf8);
decode(<<"b">>, Map) ->
    Map;
decode(<<"s">>, Map) ->
    binary_to_list(Map);
decode(<<"i">>, Map) ->
    binary_to_integer(Map).

to_prop_list({records, R}) ->
    [{records, R}].
    
               


