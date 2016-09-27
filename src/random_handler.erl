%% Feel free to use, reuse and abuse the code in this file.

%% @doc Randomly touch a bed key/value... TEMPORARY
-module(random_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    RInt = crypto:rand_uniform(1,1000),
    {ok, {key, RInt}} = jc:put(bed, RInt, RInt),
    Result = list_to_binary(["Key ", integer_to_list(RInt), " updated."]),
    {ok, Req2} = cowboy_req:reply(200, 
                                  [{<<"content-type">>, <<"text/plain">>}],
                                  Result, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
