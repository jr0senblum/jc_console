%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011 - 2017, Jim Rosenblum
%%% @doc This is the RESTFUL handler used by cowboy to provide a command
%%% interface to the web-based console.
%%% Post url encoded JSON body of {command:clear}
%%%
%%% @version {@version}
%%% @end
%%% Created : 28  September 2016 by Jim Rosenblum
%%% ----------------------------------------------------------------------------

-module(command_handler).


% Handler call-backs.
-export([init/3, 
         rest_init/2, 
         terminate/3]).

% RESTful call-backs.
-export([allowed_methods/2, 
         content_types_accepted/2]).

% Callbacks used by the restful functions.
-export([do_command/2]).


-type transport() :: {cowboy_rest:transport_name(), cowboy_rest:protocol_name()}.
-type value() :: cowboy_rest:value().
-type state() :: any().
-type req()   :: cowboy_req:req().
-type opts()  :: cowboy_rest:opts().



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
    {[<<"POST">>], Req, State}.


-spec content_types_accepted(req(), state()) -> {value(), req(), state()}.
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, do_command}],
      Req, 
      State}.



%%% ============================================================================
%%%                    Helper Functions
%%% ============================================================================


% Parse the JSON in the body, execute the command TODO: remove random.
-spec do_command(req(), state()) -> {ok | false, req(), state()}.

do_command(Req, State) ->
    {ok, Data, Req2} = cowboy_req:body(Req, []),
    Result = 
        try proplists:get_value(<<"command">>, 
                                jsone:decode(Data, [{object_format, proplist}])) of
            <<"random">> -> 
                random(),
                true;
            <<"clear">> ->
                jc:flush(),
                true;
            _ ->
                false
        catch 
            _:_ ->
                false
        end,
    {Result, Req2, State}.
            

random()->
    RInt = crypto:rand_uniform(1,1000),
    {ok, {key, RInt}} = jc:put(bed, RInt, RInt).
