%cd("D:/Projects/public_github_projects/Erlang/IntegerPartitioning").
%c("resistor_generator.erl").
%resistore_generator:test_g(3).
-module(resistor_generator).
-export([test/0,test_g/1,toString/1]).


-record(fraction, {nominator, denominator}).

fraction(A,B) -> #fraction{nominator=A,denominator=B}.

toString(Fraction) ->
	io_lib:format("~w/~w", [trunc(Fraction#fraction.nominator), trunc(Fraction#fraction.denominator)]).

gcd(A, B) when A =:= B -> A;
gcd(A, B) when A > B ->
	gcd(A-B, B);
gcd(A, B)  -> 
	gcd(A, B-A).	
	

lcd(A, B) -> 
	(A  / gcd(A, B)) * B.	


add(FA, FB) ->
	D = lcd(FA#fraction.nominator, FB#fraction.denominator),
	N = FA#fraction.nominator * (D / FA#fraction.denominator) + FB#fraction.nominator * (D / FB#fraction.denominator),
	fraction(N, D).

reduce(F) ->
	G = gcd(F#fraction.nominator, F#fraction.denominator), 
	fraction(F#fraction.nominator / G, F#fraction.denominator / G).

chunks(F) ->
	F#fraction.nominator + F#fraction.denominator.

invert(F) ->
	fraction(F#fraction.denominator, F#fraction.nominator).

toFp(F) ->
	F#fraction.nominator / F#fraction.denominator.	

%function g(a, n, s, r, N) {
%	if (s >= N)
%	{
%		if (s == N)
%		{
%			r.push([].concat(a)) ;
%		}
%		return ;
%	}
%	for (var i = n ; i <= N ; i++)
%	{
%		a.push(i) ;
%		g(a, i, s+i, r, N) ;
%		a.pop() ;
%	}
%}

g(S,_,A,NN) when S >= NN -> 
	A;
g(S,N,A,NN) when N =< NN ->
	I = N + 1,
	g(S+I,I,[I|A], NN).

%
% tests
%
test_fraction() ->
	F = fraction(1,2),
	io:fwrite("~s~n", [toString(F)]).

test_toString() ->
	A = #fraction{nominator=1, denominator=2},	
	io:fwrite("fraction as string ~s~n",[toString(A)]).
	

test_gcd() ->
	G = gcd(54,24),
	io:fwrite("~p~n", [G]).

test_lcd() ->
	L = lcd(2, 4), 
	io:fwrite("~p~n", [L]).

test_add() ->
	A = fraction(1,3),
	B = fraction(1,3),
	C = reduce(add(A, B)), 
	D = reduce(add(C, C)), 
	io:fwrite("~s + ~s = ~s, ~s ~n",[toString(A),toString(B),toString(C), toString(D)]).

test_chunks() ->
	A = fraction(3,4),
	io:fwrite("~s = ~p chunks~n", [toString(A), chunks(A)]).

test_invert() ->
	A = fraction(3,4), 
	io:fwrite("~s => inverted = ~s ~n", [toString(A), toString(invert(A))]).	

test_toFp() ->
	A = fraction(3,4), 
	io:fwrite("~s => float = ~p ~n", [toString(A), toFp(A)]).	

test() ->
	test_toString(),
	test_gcd(),
	test_lcd(),
	test_fraction(),
	test_add(),
	test_chunks(),
	test_invert(),
	test_toFp(),
	g(0,-1,[],5).
	

for(I,N,F,R) when I =:= N -> [F(I)|R] ;
for(I,N,F,R) ->
	[F(I)|for(I+1,N,F,R)]. 


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

tranformF()->
	% function that transforms
	% list [a, b, c, d] into tuple {[a, b, c, d], 1/(1/a + 1/b + 1/c + 1/d)}
	fun(LI) -> 
		{1.0 / lists:foldl(fun(Item, Agg) -> Agg + toFp(Item) end, 0, toFractions(LI)), LI} 
	end.


generateResistorNetworks(N) ->
	%generate all partitions from 1 to N
	L = lists:map(fun(I) -> mp(I) end,lists:seq(1, N)),
	%transform every parition into a circuit from parallel connected resistors
	All = lists:flatten(lists:map(fun(LI) -> lists:map(tranformF(), LI) end, L)),
	Sorted = lists:sort(fun({A,L1}, {B,L2}) -> ((A =:= B andalso length(L1) > length(L2)) orelse A > B) end, All),
	maps:from_list(Sorted).

test_g(N) ->
	generateResistorNetworks(N).