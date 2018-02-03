%%%-------------------------------------------------------------------
%%% @author  <wildbartty@cornerstone>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created :  1 Feb 2018 by  <wildbartty@cornerstone>
%%%-------------------------------------------------------------------
-module(echo).

%% API
-export([echo/0]).

-compile(export_all).

%%%===================================================================
%%% API
%%%===================================================================

echo() ->
    Pid = spawn(fun loop/0),
    link(Pid),
    Pid.

loop() ->
    {ok,Socket} = gen_tcp:listen(8888, []),
    loop(Socket).

loop(Socket) ->
    receive
	exit ->
	    ok
    after 0 ->
	    Handler = gen_tcp:accept(Socket),
	    receive
		{tcp, Handler, Packet} ->
		    io:format(Packet, []),
		    loop(Socket)
	    end
    end.

handle(Socket, Tuple) ->
    gen_tcp:send(Socket,Tuple),
    io:format("~p~n", [Tuple]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
