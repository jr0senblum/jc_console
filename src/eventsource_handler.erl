%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011-2017, Jim Rosenblum
%%% @doc SSE handler courtesy of lasse. 
%%%
%%% The endpoint,  "/eventsource/map/MAP", is used to stream notifications of
%%% writes and evicts on the given MAP to the client.
%%%
%%% @version {@version}
%%% @end
%%% Created : 16 Sept 2016 by Jim Rosenblum
%%% ----------------------------------------------------------------------------

-module(eventsource_handler).


-behaviour(lasse_handler).


% Handler call-backs.
-export([
         init/3,
         handle_notify/2,
         handle_info/2,
         handle_error/3,
         terminate/3
        ]).


-type req()    :: cowboy_req:req().
-type reason() :: lasse_handler:reason().
-type state()  :: lasse_handler:state().
-type msg()    :: lasse_handler:msg().
-type args()   :: lasse_handler:args().
-type result() :: lasse_handler:result().
-type return() :: {ok, cowboy_req:req(), state()} |
                  {shutdown, cowboy:http_status(), 
                   cowboy:http_headers(), 
                   iodata(), 
                   req(), 
                   state()}.






%%% ============================================================================
%%$                    http_handler required call-backs
%%% ============================================================================


% Create the SSE connection and send the initial information about the map.
-spec init(args(), binary()|undefined, state()) -> return().
init(_InitArgs, _LastEventId, Req) -> 
    {Map, Req2} = cowboy_req:binding(map, Req),
    lager:debug("~p: initializing SSE for pid ~p and map ~p", 
			[?MODULE, self(), Map]),
    case subscribe(Map) of
        {ok, MapAsAtom}  -> 
            {ok, Req2, [initial_event(MapAsAtom)], MapAsAtom};
        _ -> 
            lager:debug("~p: not a valid map ~p", [?MODULE, Map]),
            {shutdown, 404, [], [], Req, {}}
    end.


-spec handle_notify(msg(), state()) -> result().
handle_notify(_Msg, State) ->
    {nosend, State}.


% Receive map_event message, send it to the client, else ignore.
-spec handle_info(msg(), state()) -> result().

handle_info({_M, _K, delete} = Op, State) -> 
    Data = to_json(Op, State),
    {send, create_event(Data), State};

handle_info({M, K, write, _V}, State) -> 
    Data = to_json({M, K, write}, State),
    {send, create_event(Data), State};

handle_info(Msg, State) ->
    lager:warning("~p: unrecognized message received: ~p", 
                  [?MODULE, Msg]),
    {nosend, State}.


-spec handle_error(msg(), reason(), state()) -> state().
handle_error(_Msg, _Reason, State) ->
    State.


-spec terminate(reason(), req(), state()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.



%%% ============================================================================
%%%                    Helper Functions
%%% ============================================================================


% Subscribe to jcache events for the given map.
-spec subscribe(binary()) -> {error, badarg} | ok.

subscribe(Map) ->
    try binary_to_existing_atom(Map, utf8) of
        MapAsAtom ->
            jc_psub:map_subscribe(self(), MapAsAtom, any, any),
            {ok, MapAsAtom}
    catch 
        _:_ -> 
        {error, badarg}
    end.


% Create the iniital event used when establishing the SSE connection.
initial_event(Map) ->
    create_event(to_json(no_op, Map)).


% Create the map representing the SSE event.
create_event(Data) ->
    #{data => Data, id => id(), event => <<"map_details">>}.


% Generate a unique message id
id() ->
    Id = erlang:system_time(micro_seconds),
    integer_to_binary(Id, 16).


% Convert the operation from the subscription and the map details into JSON.
to_json(Op, Map) ->
    PList = 
        lists:flatten([jc:map_size(Map),
                       max_ttl(Map),
                       sequence(Map),
                       indexes(Map),
                       operation(Op)]),
    jsone:encode(PList).
    

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


operation({Map, Key, Op}) ->    
    [{opperation, atom_to_binary(Op, utf8)},
     {map, Map},
     {key, list_to_binary_string(Key)}];
operation(_) ->    
    [].


list_to_binary_string(Item) when is_list(Item) ->
    list_to_binary(Item);

list_to_binary_string(Item) -> Item.





          
                              
                 
