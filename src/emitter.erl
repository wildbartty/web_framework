%%%-------------------------------------------------------------------
%%% @author  <wildbartty@cornerstone>
%%% @copyright (C) 2018, 
%%% @doc
%%% 
%%% @end
%%% Created : 24 Jan 2018 by  <wildbartty@cornerstone>
%%%-------------------------------------------------------------------
-module(emitter).

%% API
-export([html/1, css/1]).

%%%===================================================================
%%% API
%%%===================================================================

-type proplist() :: [{term(), term()}].

-spec css(proplist()) -> iolist().

css([{X, Y} | Rest]) ->
    [block(X,Y), css(Rest)];
css([]) ->
    [].

%% html/1 takes a list and returns an io list

-spec html(undefined | list() | integer() | binary()) ->
    iolist().

html(X) ->
    html_priv(X).

html_priv(undefined)->
    [];
html_priv([X]) ->
    [open_close_tag(X)];
html_priv([X, Y]) ->
    [open_tag(X,Y), close_tag(X)];
html_priv(X) when is_integer(hd(X)); is_binary(X) ->
    X;
html_priv(X) when is_number(X) ->
    to_string(X);
html_priv([X, Y |Z]) ->
    [open_tag(X,Y), lists:map(fun html_priv/1, Z), close_tag(X)];
html_priv([]) ->
    [].


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

block(X,Y)->
    %% this means a space character
    %%             ||
    %%             \/
    [to_string(X), $ , ${, 10, map_vals(Y), $}, 10].


map_vals([]) ->
    [];
map_vals(Y = #{}) ->
    Fun = fun (K,V) ->
		  [9, to_string(K),32 , $:,32, to_string(V), $;, 10]
	  end,
    maps:values(maps:map(Fun,Y)).


quote(X) ->
    %% Quotes xml things
    [$', X, $'].

to_string(X) when is_atom(X) ->
    atom_to_list(X);
to_string(X) when is_number(X) ->
    hd(io_lib:format("~p", [X]));
to_string(X) when is_binary(X) ->
    binary_to_list(X);
to_string(X) ->
    %% If all else fails do nothing
    X.

expand_args([{Arg, Val} | Rest]) ->
    [to_string(Arg), $=,quote(to_string(Val)),
     if Rest =:= [] -> [];
	true -> [" ", expand_args(Rest)]
     end];
expand_args(X) ->
    X.




%% A pre emmited thing is of the form
%% [tag_name, tag_args | tagform] or
%% [tag_name, tag_args]

open_tag(Tag_name, Tag_args) ->
    %%opens the tag
    if Tag_args =:= [] ->
	    ["<", to_string(Tag_name), expand_args(Tag_args), ">", 10];
       true -> ["<", to_string(Tag_name)," ", expand_args(Tag_args), ">", 10]
    end.
%% open_tag(Tag_name) ->
%%     [$<, to_string(Tag_name), $>].

close_tag(Tag_name) ->
    [10,"</", to_string(Tag_name), ">"]. 

open_close_tag(Tag_name) ->
    %%opens the tag
    [$<, to_string(Tag_name), $/, $>].
