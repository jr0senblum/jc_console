%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011 - 2017, Jim Rosenblum
%%% @doc This is a stupid http handler that provides an endpoint to randomly
%%% insert/update a cache element to alow UI development an easy way to change 
%%% something.
%%% TODO: NEEDS TO GO AWAY FOR PRODUCTION
%%% @version {@version}
%%% @end
%%% Created : 27 September 2016 by Jim Rosenblum
%%% ----------------------------------------------------------------------------

-module(random_handler).


-export([init/3]).
-export([handle/2]).
-export([terminate/3]).


-type transport() :: {cowboy_rest:transport_name(), cowboy_rest:protocol_name()}.
-type state() :: any().
-type req()   :: cowboy_req:req().
-type opts()  :: cowboy_rest:opts().


-spec init(transport(), req(), opts()) -> {upgrade, protocol, cowboy_rest}.
init(_Type, Req, []) ->
    {ok, Req, undefined}.


% jc:put(bed, RandomNumberX, RandomNumberX).
-spec handle(req(), state()) -> {ok, req(), state()}.

handle(Req, State) ->
    RInt = crypto:rand_uniform(1,1000),
    {ok, {key, RInt}} = jc:put(bed, RInt, RInt),
    Result = list_to_binary(["Key ", integer_to_list(RInt), " updated."]),
    {ok, Req2} = cowboy_req:reply(200, 
                                  [{<<"content-type">>, <<"text/plain">>}],
                                  Result, Req),
    {ok, Req2, State}.


-spec terminate(any(), req(), state()) -> ok.
terminate(_Reason, _Req, _State) ->
	ok.
