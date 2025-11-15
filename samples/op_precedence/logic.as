// Variables
var a = true;
var b = false;
var c = true;

trace("a=" + a + ", b=" + b + ", c=" + c);
trace("----");

// Basic logical AND
trace("a && b = " + (a && b)); // false
trace("b && c = " + (b && c)); // false
trace("---");

// Basic logical OR
trace("a || b = " + (a || b)); // true
trace("b || c = " + (b || c)); // true
trace("---");

// Left-to-right evaluation with same operator
trace("a && c && b = " + (a && c && b));  // false
trace("((a && c) && b) = " + ((a && c) && b)); // false
trace("---");

trace("a || b || c = " + (a || b || c)); // true
trace("((a || b) || c) = " + ((a || b) || c)); // true
trace("---");

// Mixing && and || to test relative precedence
trace("a || b && c = " + (a || b && c));  // true
trace("(a || (b && c)) = " + (a || (b && c))); // true
trace("---");

// More complex mix
var x = false;
var y = true;
var z = false;
trace("x && y || z = " + (x && y || z)); // false
trace("((x && y) || z) = " + ((x && y) || z)); // false
