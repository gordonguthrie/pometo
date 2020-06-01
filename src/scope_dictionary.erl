-module(scope_dictionary).

-export([
			puts/1,
			transfers/1,
			gets/0,
			gets/1,
			clear/1,
			clear_all/0,
			put_line_no/1,
			get_line_no/0
		]).

-include_lib("eunit/include/eunit.hrl").

%%
%% This module uses the process dictionary for scope
%%

-record(storage, {
				current = [],
				current_line_no = none,
				kvs     = #{}
			  }).

%% this function appends the term to the PD as a list under a key called `'$POMETO_DATA'`
puts(Term) ->
	case get('$POMETO_DATA') of
		#storage{current = C} = S ->
			put('$POMETO_DATA', S#storage{current = [Term | C]});
	    undefined ->
			put('$POMETO_DATA', #storage{current = [Term]})
		end,
	ok.

transfers(Scope) ->
	#storage{current = C,
	         kvs     = KVs} = S = get('$POMETO_DATA'),
	NewKVs = map:put(Scope, C, KVs),
	put('$POMETO_DATA', S#storage{current = [],
				  kvs     = NewKVs}),
	ok.

gets() ->
	#storage{current = C} = get('$POMETO_DATA'),
	C.

gets(Scope) ->
	#storage{kvs = KVs} = get('$POMETO_DATA'),
	case maps:is_key(Scope, KVs) of
		true  -> {ok,    maps:get(Scope, KVs)};
		false -> {error, io_lib:format("no key called ~p", [Scope])}
	end.

clear_all() ->
	erase(),
	ok.

clear(Scope) ->
	#storage{kvs = KVs} = S = get('$POMETO_DATA'),
	NewKVs = map:remove(Scope, KVs),
	put('$POMETO_DATA', S#storage{kvs = NewKVs}),
	ok.

put_line_no(N) ->
	case get('$POMETO_DATA') of
		#storage{} = S ->
			put('$POMETO_DATA', S#storage{current_line_no = N});
	    undefined ->
			put('$POMETO_DATA', #storage{current_line_no = N})
		end,
	ok.

get_line_no() ->
	#storage{current_line_no = N} = get('$POMETO_DATA'),
	N.
