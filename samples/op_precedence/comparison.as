// Variables
var a = 10;
var b = 5;
var c = 7;

trace("a=" + a + ", b=" + b + ", c=" + c);
trace("----");

// Basic comparisons
trace("a > b = " + (a > b));   // true
trace("a < b = " + (a < b));   // false
trace("a >= b = " + (a >= b)); // true
trace("a <= b = " + (a <= b)); // false
trace("---");

// Left-to-right evaluation
trace("a > b >= c = " + (a > b >= c)); // false
trace("((a > b) >= c) = " + ((a > b) >= c)); // false
trace("---");

trace("a < b <= c = " + (a < b <= c)); // true
trace("((a < b) <= c) = " + ((a < b) <= c)); // true
trace("---");

// Mixing with arithmetic
trace("a + b > c = " + (a + b > c)); // true
trace("((a + b) > c) = " + ((a + b) > c)); // true
trace("---");

trace("a * b <= c + 5 = " + (a * b <= c + 5)); // false
trace("((a * b) <= (c + 5)) = " + ((a * b) <= (c + 5))); // false
