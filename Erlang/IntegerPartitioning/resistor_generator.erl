-module(resistor_generator).
-export([test/0,toString/1]).


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
	