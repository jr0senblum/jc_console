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
         content_types_provided/2]).

% Callbacks used by the restful functions.
-export([ summary_to_json/2]).


-type transport() :: {cowboy_rest:transport_name(), cowboy_rest:protocol_name()}.
-type value() :: cowboy_rest:value().
-type state() :: any().
-type req()   :: cowboy_req:req().
-type opts()  :: cowboy_rest:opts().



-ifdef('WIN').
-define(JSON_DECODE(X), jsone:decode(X, [{object_format, proplist}])).
-define(JSON_ENCODE(X), jsone:encode(X)).
-else.
-define(JSON_DECODE(X), jsonx:decode(X, [{format, proplist}])). 
-define(JSON_ENCODE(X), jsonx:encode(X)).
-endif.


%%% ============================================================================
%%%                    http_handler required call-backs
%%% ============================================================================

-spec init(transport(), req(), opts()) -> {upgrade, protocol, cowboy_rest}.
init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.


-spec terminate(cowboy_rest:reason(), req(), state()) -> ok.
terminate(_Reason, _Req, _State) ->
	ok.


-spec rest_init(cowboy_req:req(), opts()) -> {ok, req(), {}}.
rest_init(Req, _Opts) ->
    {ok, Req, {}}.



%%% ============================================================================
%%$                    RESTful call-backs : GET
%%% ============================================================================


-spec allowed_methods(req(), state()) -> {[binary()], req(), state()}.
allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.


% For GET, HEAD, POST, PUT, PATCH, DELETE, the resource types provided and the
% call-back used.
-spec content_types_provided(req(), state()) -> {value(), req(), state()}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, summary_to_json}
     ], Req, State}.



%%% ============================================================================
%%%                    Helper Functions
%%% ============================================================================

% Construct the JSON which represents the jcache summary information:
% cache tables and thier sizes
% configured and up nodes
% cache-line names, references for SSEs and more infomration.
-spec summary_to_json(req(), state()) -> {cowboy_rest:body(), req(), state()}.

summary_to_json(Req, State) -> 
    Sizes = node_reported_sizes(),

    PLists = [to_prop_list(jc:up()),
              to_prop_list(jc:cache_nodes()),
              Sizes,
              to_prop_list(all_mentioned_maps())],
   
    Body = jsonx:encode(PLists),
    {Body, Req, State}.


% For each active node, get its reported cache table statistics.
node_reported_sizes() ->
    {per_node_sizes, lists:foldl(fun add_good_result/2, [], up_nodes())}.

% Get all up nodes.
up_nodes() ->
    {nodes, {active, Active}, {configured, _}} = jc:cache_nodes(),
    Active.

% If the rpc to the given node works, accumulate its answer
add_good_result(N, Acc) ->
    case rpc:call(N, jc, cache_size, []) of
        {size,_} = R ->
            [{N, [to_prop_list(R)]} | Acc];
        _ ->
            Acc
end.
    

% Construct a proplist that can be turned into JSON.
to_prop_list({nodes, {active, Up}, {configured, Configured}}) ->
    {nodes, [{configured, Configured}, {up, Up}]};

to_prop_list({size, TableInfo}) ->
    F = fun(T, Acc) -> [to_prop_list(T) | Acc]  end,
    {tables, lists:foldl(F, [], TableInfo)};

to_prop_list({uptime,[{up_at, Up},{now, Now},{up_time, {D,{H,M,S}}}]}) ->
    {up_time, [{started, list_to_binary(Up)},
                {now, list_to_binary(Now)},
                {up, [{days, D}, {hours, H}, {minutes, M}, {seconds, S}]}]};
    
to_prop_list({Table, {records, Rs}, {bytes, Bs}}) ->
    [{table_name, Table}, {record_count, Rs}, {byte_count, Bs}];

to_prop_list(Maps) when is_list(Maps)->
    F = fun(M, Acc) ->
                [[{cache, M}, 
                  {ref, reference(M)},
                  {sse, events(M)}] | Acc] 
        end,
    {cache_lines, lists:foldl(F, [], Maps)}.


% Construct the reference.
reference(MapName) ->
    B = atom_to_binary(MapName, utf8),
    <<"/api/map/", B/binary>>.


% Construct the sse reference.
events(MapName) ->
    B = atom_to_binary(MapName, utf8),
    <<"/api/eventsource/map/", B/binary>>.


all_mentioned_maps() ->
    Maps = lists:foldl(fun({M, _}, Acc) -> sets:add_element(M, Acc) end,
                       sets:from_list(jc:maps()),
                       jc_eviction_manager:get_max_ttls()), 
    sets:to_list(Maps).
                        
