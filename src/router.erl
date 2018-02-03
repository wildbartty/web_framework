-module(router).

-import(proplists, [lookup/2]).

-import_lib("stdlib/include/qlc.hrl").

-export([initDB/0]).
-export([get_all_pages/0, get_page/1]).
-export([add_page/1]).
-export([build_page/1, render_page/1]).

-record(my_web_pages, {name,
		       header,
		       style,
		       scripts,
		       title,
		       page}).
initDB()->
    mnesia:create_schema([node()]),
    mnesia:start(),
    {Ret,Rea} = mnesia:create_table(my_web_pages, [{attributes,
						    record_info(fields
							       ,my_web_pages)},
						   {type, set},
						   {disc_copies, [node()]}]),
    if (Ret =:= atomic) -> ok;
       true -> {Ret, Rea}
    end.

lookup(Key, List, Default) ->
    Result = lookup(Key, List),
    if Result =:= none ->
	    {Key, Default};
       true ->
	    Result
    end.

iolist_to_bin(X) when is_binary(X) ->
    X;
iolist_to_bin(X) when is_number(X)->
    erlang:iolist_to_binary(io_lib:format("~p", [X]));
iolist_to_bin(undefined) ->
    undefined;
iolist_to_bin(X) when is_atom(X) ->
    atom_to_binary(X, unicode);
iolist_to_bin(X) when is_function(X); is_tuple(X) ->
    X;
iolist_to_bin(X) ->
    iolist_to_binary(X).





add_page(Args) ->
    {page,Page0} = lookup(page, Args, undefined),
    {header,Header} = lookup(header, Args, undefined),
    {name,Name} = lookup(name, Args, undefined),
    {title,Title} = lookup(title, Args, Name),
    {style, Style} = lookup(style,Args, undefined),
    {scripts, Scripts} = lookup(scripts, Args, undefined),
    Page = #my_web_pages{name =  iolist_to_bin(Name),
    			 header = iolist_to_bin(Header),
    			 style =  iolist_to_bin(Style),
    			 scripts =iolist_to_bin(Scripts),
    			 title =  iolist_to_bin(Title),
    			 page =   iolist_to_bin(Page0)},
    Write = fun () ->
    		    mnesia:write(Page)
    	    end,
    mnesia:transaction(Write).

the_404_page() ->
    #my_web_pages{name = <<"404">>, 
		  title = <<"404">>,
		  page = iolist_to_bin(emitter:html([p, [], "Hello world"]))}.

get_page(Page) ->
    Read = fun () ->
		   mnesia:read({my_web_pages, iolist_to_bin(Page)})
	   end,
    case mnesia:transaction(Read) of
	{atomic, [X]} -> X;
	_ -> the_404_page()
    end.

maybe_html(_,undefined) ->
    [];
maybe_html(nothing,Thing) ->
    Thing;
maybe_html(Name, Thing) ->
    [Name, [], Thing].

build_page(_ = #my_web_pages{title = Title, style = Style, scripts = Scripts,
			     header = Header, name = _Name, page = Page}) ->
    iolist_to_bin(emitter:html([html, [],
				[head, [], 
				 maybe_html(title, Title),
				 maybe_html(style, Style),
				 maybe_html(script, Scripts),
				 maybe_html(nothing, Header)],
				[body, [],
				 Page]])).

get_all_pages() ->
    Read = fun () ->
		   mnesia:match_object(#my_web_pages{_ = '_'})
	   end,
    mnesia:transaction(Read).

render_page(Page) ->
    build_page(get_page(Page)).
