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


-spec init(args(), binary()|undefined, state()) -> return().
init(_InitArgs, _LastEventId, Req) -> 
    {Map, Req2} = cowboy_req:binding(map, Req),
    lager:debug("~p: initializing SSE for pid ~p and map ~p", 
			[?MODULE, self(), Map]),
    case subscribe(Map) of
        ok -> 
            {ok, Req2, {}};
        _ -> 
            lager:debug("~p: not a valid map ~p", [?MODULE, Map]),
            {shutdown, 404, [], [], Req, {}}
    end.


-spec handle_notify(msg(), state()) -> result().
handle_notify(_Msg, State) ->
    {nosend, State}.


% Receive map_event message, send it to the client, else ignore.
-spec handle_info(msg(), state()) -> result().

handle_info({_M, _K, delete} = Details, State) -> 
    Data = to_json(Details),
    {send, #{data => Data, id => id()}, State};

handle_info({M, K, write, _V}, State) -> 
    Data = to_json({M, K, write}),
    {send, #{data => Data, id => id()}, State};

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


% Generate a unique message id
id() ->
    Id = erlang:system_time(micro_seconds),
    integer_to_binary(Id, 16).


% RetrieAssumes Plist contains map, key, value where map is a binary string 
% representing a string name of a map. 
-spec subscribe(binary()) -> {error, badarg} | ok.

subscribe(Map) ->
    try binary_to_existing_atom(Map, utf8) of
        MapAsAtom ->
            jc_psub:map_subscribe(self(), MapAsAtom, any, any)
    catch 
        _:_ -> 
        {error, badarg}
    end.

to_json(Details) ->
    try to_json_h(Details) of
        Result ->
            Result
    catch
        _:_ -> 
            E = [{error, <<"Result could not be converted into JSON">>},
                 {id, id()}],
            jsonx:encode(E)
    end.

to_json_h({Map, Key, Op}) ->    
    M = [{opperation, atom_to_binary(Op, utf8)},
         {map, Map},
         {key, list_to_binary_string(Key)}],
    jsonx:encode(M).


list_to_binary_string(Item) when is_list(Item) ->
    list_to_binary(Item);
list_to_binary_string(Item) -> Item.




          
                              
                 
