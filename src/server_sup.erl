%%%-------------------------------------------------------------------
%%% @author  <wildbartty@cornerstone>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 30 Jan 2018 by  <wildbartty@cornerstone>
%%%-------------------------------------------------------------------
-module(server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,start_socket/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, Port} = get_port(),
    %% Set the socket into {active_once} mode.
    %% See sockserv_serv comments for more details.
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, {packet,line}]),
    spawn_link(fun empty_listeners/0),
    {ok, {{simple_one_for_one, 60, 3600},
	  [{socket,
	    {server, start_link, [ListenSocket]}, %Pass the socket!
	    temporary, 1000, worker, [sockserv_serv]}
	  ]}}.


start_socket() ->
    supervisor:start_child(?MODULE, []).

empty_listeners() ->
    [start_socket() || _ <- lists:seq(1,20)].
		      

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_port() ->
    case application:get_env(port) of
	undefined ->
	    {ok,8888};
	Port ->
	    Port
    end.
