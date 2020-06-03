-module(scope_dictionary).

-export([
			puts/1,
			are_bindings_valid/0,
			% transfers/1,
			%gets/0,
			%gets/1,
			%get_keys/1,
			clear/1,
			clear_all/0,
			put_line_no/1,
			get_line_no/0
		]).

%% debugging export
-export([
		  'print_DEBUG'/0
		]).

-include_lib("eunit/include/eunit.hrl").

-define(EMPTYDUPS, []).

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

are_bindings_valid() ->
	case get('$POMETO_DATA') of
		#storage{current = C,
				 kvs     = KVs} = S ->
			?debugFmt("current is ~p~n", [C]),
			case get_duplicates(C) of
                []   -> true;
                Dups -> {false, Dups}
			end;
	    undefined ->
            true
		end.

%transfers(Scope) ->
%	#storage{current = C,
%	         kvs     = KVs} = S = get('$POMETO_DATA'),
%	NewKVs = map:put(Scope, C, KVs),
%	put('$POMETO_DATA', S#storage{current = [],
%				                  kvs     = NewKVs}),
%	ok.

%gets() ->
%	case get('$POMETO_DATA') of
%		#storage{current = C} = S -> C;
%	    undefined                 -> [])
%	end.

%get_keys() ->
%	case get('$POMETO_DATA') of
%		#storage{kvs = KVs} = S -> maps:keys(KVs);
%	    undefined               -> []
%	end.

%gets(Scope) ->
%	#storage{kvs = KVs} = get('$POMETO_DATA'),
%	case maps:is_key(Scope, KVs) of
%		true  -> {ok,    maps:get(Scope, KVs)};
%		false -> {error, io_lib:format("no key called ~p", [Scope])}
%	end.

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

get_duplicates(Bindings) -> get_dups(lists:sort(Bindings), ?EMPTYDUPS).

get_dups([],                      Dups) -> Dups;
get_dups([H                | []], Dups) -> Dups;
% three in a row is two errors
get_dups([{K, V1}, {K, V2} | T],  Dups) -> get_dups([{K, V2} | T], [{K, {V1, V2}} | Dups]);
get_dups([_H               | T],  Dups) -> get_dups(T, Dups).

print_DEBUG() ->
	S = get('$POMETO_DATA'),
	case S of
		undefined ->
		?debugFmt("there is nothing in the scope dictionary~n", []);
	#storage{current         = C,
			 current_line_no = N,
			 kvs             = KVs} ->
		?debugFmt("The current accumulator is ~p~n", [C]),
		?debugFmt("The current line no is ~p~n", [N]),
		?debugFmt("The KVs are ~p~n", [KVs])
	end.
