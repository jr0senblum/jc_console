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
         resource_exists/2]).

% Callbacks used by the restful functions.
-export([map_to_json/2]).


-type transport() :: {cowboy_rest:transport_name(), cowboy_rest:protocol_name()}.
-type value() :: cowboy_rest:value().
-type state() :: any().
-type req()   :: cowboy_req:req().
-type opts()  :: cowboy_rest:opts().



%%% ============================================================================
%%%                    http_handler, required call-backs
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
%%%                    RESTful call-backs : GET
%%% ============================================================================

-spec allowed_methods(req(), state()) -> {[binary()], req(), state()}.
allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.


% For GET, HEAD, POST, PUT, PATCH, DELETE, the resource types provided and the
% call-back used.
-spec content_types_provided(req(), state()) -> {value(), req(), state()}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, map_to_json}
     ], Req, State}.


-spec resource_exists(req(), state()) -> {boolean(), req(), state()}.
resource_exists(Req, _State) ->
    {Map, Req2} = cowboy_req:binding(map, Req),
    try binary_to_existing_atom(Map, utf8) of
        Atom  ->
            {true, Req2, Atom}
    catch
        _:_ ->
            {false, Req2, {}}
    end.



%%% ============================================================================
%%%                    Helper Functions
%%% ============================================================================


% Construct the Json representing informationabout a given map.
-spec map_to_json(req(), atom()) -> {cowboy_rest:body(), req(), state()}.
                                     
map_to_json(Req, Map) ->
    PList = 
        [jc:map_size(Map),
         max_ttl(Map),
         sequence(Map),
         indexes(Map)],
    Body = jsone:encode(PList),
    {Body, Req, {}}.


max_ttl(Map) ->
    case lists:keyfind(Map, 1, jc_eviction_manager:get_max_ttls()) of
        {Map, Secs} ->
            {ttl, Secs};
        false ->
            {ttl, 0}
    end.


sequence(Map) ->
    case jc_s:sequence(Map) of
        {ok, not_exist} ->
            {sequence_no, false};
        {ok, Seq} ->
            {sequence_no, Seq}
    end.


indexes(Map) ->
    F = fun({{_Map, Path}, Pos}, Acc) -> 
                [[{path, tuple_to_list(Path)}, {pos, Pos}] | Acc]
        end,
    {indexes, lists:foldl(F, [], jc_store:indexes(Map))}.
