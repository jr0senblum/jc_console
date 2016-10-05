%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011-2015, Jim Rosenblum
%%% @doc Top-level supervisor for the jc_console application. 
%%% Doesn't really do anything except make this a OTP compliant application.
%%% @version {@version}
%%% @end
%%% Created : 21 September 2016 by Jim Rosenblum
%%% ----------------------------------------------------------------------------

-module(jc_console_sup).


-behaviour(supervisor).


%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


%% ===================================================================
%% API functions
%% ===================================================================


-spec start_link() -> {ok, pid()} | ignore | {error, {already_started, pid()}}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init([]) ->  {'ok', {{'one_for_one', 5, 10}, []}}.
init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

