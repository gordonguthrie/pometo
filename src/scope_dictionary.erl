-module(scope_dictionary).

%% normal exports
-export([
      puts/1,
      are_current_bindings_valid/0,
      can_bindings_be_consolidated/0,
      consolidate_bindings/0,
      get_bindings/0,
      clear_all/0,
      put_line_no/1,
      get_line_no/0,
      get_new_scope/0,
      get_current_scope/0
    ]).

% exports for use in the interpreter only
% in the interpreter the global bindings of Rappel
% need to be loaded into the scope dictionary at first
-export([
      persist_bindings/1
    ]).


%% debugging export
-export([
      'print_DEBUG'/0,
      'print_DEBUG'/1
    ]).

-include_lib("eunit/include/eunit.hrl").
-include("parser_records.hrl").
-include("errors.hrl").

-define(EMPTYDUPS, []).

%%
%% This module uses the process dictionary for scope
%%

-record(storage, {
                  current         = [],
                  current_line_no = none,
                  bindings        = #{},
                  errors          = [],
                  scope_index     = 0
                 }).

%% this function appends the term to the Process Dictionary as a list under a key called `'$POMETO_DATA'`
puts(Term) ->
  case get('$POMETO_DATA') of
    #storage{current = C} = S ->
      put('$POMETO_DATA', S#storage{current = [Term | C]});
    undefined ->
      put('$POMETO_DATA',  #storage{current = [Term]})
    end,
  ok.

are_current_bindings_valid() ->
  case get('$POMETO_DATA') of
    #storage{current = C} ->
      case get_duplicates(C) of
                []   -> true;
                Dups -> {false, Dups}
      end;
    undefined ->
            true
    end.

consolidate_bindings() ->
  case get('$POMETO_DATA') of
    [] ->
      true;
    #storage{current  = C,
         bindings = Bindings} = S ->
      ConsolidateFn = fun({K, V}, Bs) ->
        maps:put(K, V, Bs)
      end,
      NewBs =  lists:foldl(ConsolidateFn, Bindings, C),
      put('$POMETO_DATA', S#storage{current  = [],
                                bindings = NewBs})
  end,
  ok.

can_bindings_be_consolidated() ->
  case get('$POMETO_DATA') of
    [] ->
      true;
    #storage{current  = C,
         bindings = Bindings} ->
      TestKeysFn = fun({K, CurrentBinding}, Acc) ->
        case maps:is_key(K, Bindings) of
          false -> Acc;
          true  -> [{K, {maps:get(K, Bindings), CurrentBinding}} | Acc]
        end
      end,
      Test = lists:foldl(TestKeysFn, ?EMPTYDUPS, C),
      case Test of
        [] -> true;
        _  -> {false, Test}
      end
  end.

get_bindings() ->
  case get('$POMETO_DATA') of
    #storage{bindings = Bindings} -> Bindings;
    undefined                     -> #{}
  end.

clear_all() ->
  erase(),
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

persist_bindings(Bs) ->
  case get('$POMETO_DATA') of
    #storage{bindings = OldBs} = S ->
      MergedBs = maps:merge(OldBs, Bs),
      put('$POMETO_DATA', S#storage{bindings = MergedBs});
    undefined ->
      put('$POMETO_DATA', #storage{bindings = Bs})
  end,
  ok.

get_new_scope() ->
  NewRec = case get('$POMETO_DATA') of
    #storage{scope_index = ScopeNo} = S -> S#storage{scope_index = ScopeNo + 1};
    undefined                           -> #storage{}
  end,
  put('$POMETO_DATA', NewRec),
  #storage{scope_index = NewS} = NewRec,
  io_lib:format("~p", [NewS]).

get_current_scope() ->
  case get('$POMETO_DATA') of
    #storage{scope_index = ScopeNo} -> io_lib:format("~p", [ScopeNo]);
    undefined                       -> NewRec = #storage{},
                                       put('$POMETO_DATA', NewRec),
                                       #storage{scope_index = NewS} = NewRec,
                                       io_lib:format("~p", [NewS])
  end.

%%
%% private functions
%%

get_duplicates(Bindings) -> get_dups(lists:sort(Bindings), ?EMPTYDUPS).

get_dups([],                      Dups) -> Dups;
get_dups([_H               | []], Dups) -> Dups;
% three in a row is two errors
get_dups([{K, V1}, {K, V2} | T],  Dups) -> get_dups([{K, V2} | T], [{K, {V1, V2}} | Dups]);
get_dups([_H               | T],  Dups) -> get_dups(T, Dups).

print_DEBUG() -> print_DEBUG("Scope Dictionary").

print_DEBUG(Label) ->
  ?debugFmt("~n*************************************************~n~n", []),
  ?debugFmt(Label ++ "~n", []),
  S = get('$POMETO_DATA'),
  case S of
    undefined ->
    ?debugFmt("there is nothing in the scope dictionary~n", []);
  #storage{current         = C,
       current_line_no = N,
       bindings        = Bindings} ->
    ?debugFmt("The current accumulator is ~p~n", [C]),
    ?debugFmt("The current line no is ~p~n", [N]),
    ?debugFmt("The Bindings are ~p~n", [Bindings])
  end,
  ?debugFmt("~n*************************************************~n", []).
