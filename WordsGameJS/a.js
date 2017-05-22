var fs = require('fs');

var charCodeOfA = "a".charCodeAt(0) ;

function addWord(hash, bitMap, a) 
{

	var hashValue = a.toLowerCase()
		.split("")
		.map(function(a) { 
			return a.charCodeAt(0) - charCodeOfA ; 
		}) 
		.reduce(function(acc, a) {
			var mask = 1 << a ;
			return (acc |= mask) ;
		}, 0) ;

	hash[hashValue] = hash[hashValue] || [] ;
	hash[hashValue].push(a) ;

	for (var i = 0 ; i < 26; i++)
	{
		if (hashValue & (1 << i))
		{
			bitMap[i] = bitMap[i] || [] ;
			bitMap[i][hashValue] = 1 ;
		}
	}
}

function intersectN(ars)
{
	var result = [] ;
	var head = ars[0] ;
	var b  ; 
	head.forEach(function(v, ix, ar)
	{

		b = true ;
		for(var i = 1 ; i < ars.length ; i++)
		{
			if (typeof(ars[i]) == "undefined" || ars[i][ix] != 1)
			{
				b = false ;
				break ;
			}
		}
		if (b)
		{
			result[ix] = 1;
		}
	}) ;

	return result ;
}

function search(letters) 
{

	var hlp = letters
		.toLowerCase()
		.split("") 
		.map(function(a) {			
			return bitMap[a.charCodeAt(0) - charCodeOfA] ; 
		}) ;

	return intersectN(hlp).reduce(function(acc, v, ix, array) 
			{
				return acc.concat(hash[ix]) ;
			}, []) ;

}

var bitMap = [] ;
var hash = [] ;

var words  = fs.readFileSync('words.txt').toString().split("\n") ;

words.forEach(function(w) 
{
	addWord(hash, bitMap, w);
});

console.log("----------------- start time: " + new Date() ) ;
console.log(search("angel")
	.filter(function(v) 
	{
		return v.length <= 6 ;
	})
	.join(";")) ;
console.log("----------------- end time: " + new Date() ) ;
