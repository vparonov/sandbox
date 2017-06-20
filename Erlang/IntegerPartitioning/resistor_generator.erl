
%cd("D:/Projects/public_github_projects/Erlang/IntegerPartitioning").
%c("resistor_generator.erl").
%resistore_generator:test_g(3).
-module(resistor_generator).
-export([test_g/2,test_r/2, takeN/2,dump_values/2,fillUpTo/3]).
-include_lib("stdlib/include/ms_transform.hrl").

-record(fraction, {nominator, denominator}).

fraction(A,B) -> #fraction{nominator=A,denominator=B}.

toFp(F) ->
	F#fraction.nominator / F#fraction.denominator.	

mp(N) ->
    lists:foreach(fun (X) -> put(X, undefined) end,
          lists:seq(1, N)), % clean up process dictionary for sure
    mp(N, N).

mp(N, Max) when N > 0 ->
    case get(N) of
      undefined -> R = mp(N, 1, Max, []), put(N, R), R;
      [[Max | _] | _] = L -> L;
      [[X | _] | _] = L ->
          R = mp(N, X + 1, Max, L), put(N, R), R
    end;
mp(0, _) -> [[]];
mp(_, _) -> [].

mp(_, X, Max, R) when X > Max -> R;
mp(N, X, Max, R) ->
    mp(N, X + 1, Max, prepend(X, mp(N - X, X), R)).

prepend(_, [], R) -> R;
prepend(X, [H | T], R) -> prepend(X, T, [[X | H] | R]).


toFractions(L) ->
	lists:map(fun(LI) -> fraction(1, LI) end, L).

tranformF(_)->
	% function that transforms
	% list [a, b, c, d] into tuple {1/(1/a + 1/b + 1/c + 1/d), lenghOfl, [a, b, c, d]}
	%Coef = Decimals * 10, 
	fun(LI) -> 
		{
			%round(Coef *
				(1.0 / lists:foldl(fun(Item, Agg) -> Agg + toFp(Item) end, 0, toFractions(LI)))
				%) / Coef
			,{lchunks(LI), LI}
		} 
	end.

lchunks(L) ->
		lists:foldl(fun(I, Agg) -> Agg + I end, 0, L).

generateResistorNetworks(N, Decimals) ->
	%generate all partitions from 1 to N
	L = lists:map(fun(I) -> mp(I) end,lists:seq(1, N)),
	
	%transform every parition into a circuit from parallel connected resistors
	All = lists:flatten(lists:map(fun(LI) -> lists:map(tranformF(Decimals), LI) end, L)),

	%sort all circuits by value and number of chunks
	Sorted = lists:sort(
		fun({A,{Chunks1, _}}, {B,{Chunks2,_}}) -> 
			((A > B) orelse (A =:= B andalso Chunks1 > Chunks2)) 
		end, All),
	maps:to_list(maps:from_list(Sorted)).

saveGeneratedResistorNetworks(FileName, ResistorNetworks) ->
	{ok,Ref} = dets:open_file(FileName,[]),
	lists:foreach(fun(Item) -> dets:insert(Ref, Item) end, ResistorNetworks),
	dets:close(Ref).

openRNF(FileName) ->
	{ok,Ref} = dets:open_file(FileName,[]),
	Ref.

closeRNF(Ref) ->
	dets:close(Ref).

takeN(N, L) -> takeN(N, L, []).
takeN(_, [], R)  -> 
	lists:reverse(R);
takeN(0, _,  R)  -> 
	lists:reverse(R);
takeN(N, [H|T],  R)  -> 
	takeN(N-1, T, [H|R]).
	

fillUpTo(N,Filler,List) -> fillUpTo(N,Filler,List,[]).

fillUpTo(0,_,_,Result)          -> lists:reverse(Result) ;
fillUpTo(N,Filler,[],Result)    -> fillUpTo(N-1,Filler,[],[Filler|Result]);
fillUpTo(N,Filler,[H|T],Result) -> fillUpTo(N-1,Filler,T ,[H|Result]).




test_g(N, Decimals) ->
	saveGeneratedResistorNetworks("data.dat", generateResistorNetworks(N, Decimals)).

test_r(Inputs, Tolerance) ->
	Table = openRNF("data.dat"),
	TolCoef = Tolerance / 100.0, 
	RetVal = lists:map(
		fun(D) ->
			Tol = D * TolCoef, % search within 10% tolerance 	
			Selection = 
				dets:select
				(
					Table, 
					ets:fun2ms(
							fun({Value, {Chunks, L}}) 
								when abs(Value - D) =< Tol -> 
									{round(1000000 * abs(Value - D)), Value, Chunks, L} 
							end)
				),
			SortedSelection = lists:sort(fun({A,_,Ca,_}, {B,_,Cb,_}) -> A < B orelse (A =:= B andalso Ca < Cb) end, Selection),
			takeN(10, SortedSelection)
		end, Inputs),
	closeRNF(Table),
	RetVal.

dump_values(N, OutFileName) ->
	Table = openRNF("data.dat"),
	All = dets:traverse(Table, fun(X) -> {continue, X} end),
	Sorted = lists:sort(fun({A,{_, _}}, {B,{_,_}}) -> A < B end, All), 
	Result = lists:map(
		fun({FloatValue,{Chunks, List}}) ->
			io_lib:format("~w~c~w~c~w~n", [FloatValue, 9,Chunks, 9,fillUpTo(N,0,List)])
		end, Sorted),
	closeRNF(Table),
	file:write_file(OutFileName,Result).


		


