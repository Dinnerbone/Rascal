// Variables
var a = 10;
var b = "10";
var c = 5;

trace("a=" + a + ", b=" + b + ", c=" + c);
trace("----");

// Loose equality (type coercion)
trace("a == b = " + (a == b));   // true, because '10' coerces to number
trace("a != b = " + (a != b));   // false
trace("---");

// Strict equality (no coercion)
trace("a === b = " + (a === b)); // false
trace("a !== b = " + (a !== b)); // true
trace("---");

// Left-to-right evaluation
trace("a == b == c = " + (a == b == c)); // false
trace("((a == b) == c) = " + ((a == b) == c)); // false

trace("a !== b !== c = " + (a !== b !== c)); // true
trace("((a !== b) !== c) = " + ((a !== b) !== c)); // true
trace("---");

// Mixing with comparison operators
trace("a == b > c = " + (a == b > c)); // false
trace("(a == (b > c)) = " + (a == (b > c))); // false
