%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011 - 2017, Jim Rosenblum
%%% @doc This is the http, RESTFUL handler used by cowboy to provide summary
%%% information about the cache to the web-based console.
%%%
%%% @version {@version}
%%% @end
%%% Created : 19 September 2016 by Jim Rosenblum
%%% ----------------------------------------------------------------------------

-module(summary_handler).


% Handler call-backs.
-export([init/3, 
         rest_init/2, 
         terminate/3]).

% RESTful call-backs.
-export([allowed_methods/2, 
         content_types_provided/2,
         summary_to_json/2]).



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
      {<<"application/json">>, summary_to_json}
     ], Req, State}.


% Construct the JSON which represents the jcache summary information:
% cache tables and their sizes
% configured and up nodes
% cache-line names and URLs for more infomration.
summary_to_json(Req, State) ->
    Host = construct_host(Req),
    PLists = 
        to_prop_list(jc:cache_size(), Host) 
        ++ to_prop_list(jc:cache_nodes(), Host)
        ++ to_prop_list(jc:maps(), Host),
    Body = jsonx:encode(PLists),
    {Body, Req, State}.


% Construct a proplist that can be turned into JSON. Host is provided in 
% case it is needed for certain values - like uri's.
to_prop_list({nodes, {active, Up}, {configured, Configured}}, _Host) ->
    [{nodes, [{configured, Configured}, 
              {up, Up}]}];

to_prop_list({size, TableInfo}, Host) ->
    [{tables, lists:foldl(fun(T, Acc) -> [to_prop_list(T, Host) | Acc] end, 
                          [], 
                          TableInfo)}];
    
to_prop_list({Table, {records, Rs}, {bytes, Bs}}, _Jost) ->
    [{table_name, Table}, {record_count, Rs}, {byte_count, Bs}];

to_prop_list(Maps, Host) when is_list(Maps)->
    F = fun(M, Acc) ->
                [[{name, map_to_bstring(M)}, {url, url_string(Host, M)}] | Acc] 
        end,
    [{cache_lines, lists:foldl(F, [], Maps)}].


% url needs to indicate the map data-type: string, binary, integer or atom 
% because a map could be any of those.
url_string(HostPort, MapName) when is_atom(MapName) ->
    B = atom_to_binary(MapName, utf8),
    <<HostPort/binary, "/map/a/", B/binary>>;

url_string(HostPort, MapName) when is_binary(MapName) ->
    <<HostPort/binary, "/map/b/", MapName/binary>>;

url_string(HostPort, MapName) when is_list(MapName) ->
    B = list_to_binary(MapName),
    <<HostPort/binary, "/map/s/", B/binary>>;
url_string(HostPort, MapName) when is_integer(MapName) ->
    B = integer_to_binary(MapName),
    <<HostPort/binary, "/map/i/", B/binary>>.


% convert map name to an atom or binary string for JSON construction
map_to_bstring(M) when is_atom(M) ->
    M;
map_to_bstring(M) when is_list(M) ->
    list_to_binary(M);
map_to_bstring(M) when is_integer(M) ->
    integer_to_binary(M);
map_to_bstring(M) -> M.


% construct the host string used to construct the url where a
% map can be found.
construct_host(Req) ->
    {Host, Req2} = cowboy_req:host(Req),
    {Port, _Req3} = cowboy_req:port(Req2),
    list_to_binary(lists:append([binary_to_list(Host), 
                                 ":", 
                                 integer_to_list(Port)])).