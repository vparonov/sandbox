   
function add(a, b) {
	var d = lcd(a.d, b.d) ;

	var n = a.n * (d / a.d) + b.n * (d / b.d)  ;

	return {n : n, d: d};
}

function reduce(a) {
	var g = gcd(a.n, a.d) ;
	return {n: a.n / g, d: a.d / g} ;
}

function chunks(a) {
	return a.n + a.d ;
}

function invert(a) {
	return {n : a.d, d : a.n};
}

function toFp(a) {
	return a.n / a.d ;
}

function lcd(a, b) {
	return (a / gcd(a, b)) * b ;
}

function gcd(a, b) {
	while(a != b) {
		if (a > b) {
			a -= b ;
		}
		else {
			b -= a ;
		}
	}

	return a; 
}

function toString(a) {
	return "" + a.n + "/" + a.d ;
}

function g(a, n, s, r, N) {
	if (s >= N)
	{
		if (s == N)
		{
			r.push([].concat(a)) ;
		}
		return ;
	}
	for (var i = n ; i <= N ; i++)
	{
		a.push(i) ;
		g(a, i, s+i, r, N) ;
		a.pop() ;
	}
}

var r = [] ;
var MAXN = 55 ;
for (var i = 2 ; i <= MAXN ; i++)
{
	g([], 1, 0, r, i) ;
}

var tr = r.map(function (s) {
	var x = {n: 0, d:1} ;
	s.forEach(function (el) {
		x = add(x, {n:1, d:el}) ;
	});
	x = invert(reduce(x)) ;
	var sch = s.join("/");
	var chunks = s.reduce(function(a, c) {
		return a + 1.0 * c ;
	}, 0) ;

	return {s:sch, v:x, l:chunks} ; 
	//console.log( + "->" + toString(x)) ;
	//console.log() ;
});

tr.sort(function(a, b) {
	return toFp(a.v) - toFp(b.v) ;
})
.forEach(function (s) {
	console.log(toFp(s.v) + "\t" + toString(s.v) +"\t"+s.s + "\t" + s.l) ;
});

