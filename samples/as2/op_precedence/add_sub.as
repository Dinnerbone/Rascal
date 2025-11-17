var a = 10;
var b = 5;
var c = 2;

trace("a=" + a + ", b=" + b + ", c=" + c);
trace("----");

// Basic left-to-right evaluation
trace("a + b - c = " + (a + b - c)); // 13
trace("(a + b) - c = " + ((a + b) - c)); // 13
trace("---");

trace("a - b + c = " + (a - b + c)); // 7
trace("(a - b) + c = " + ((a - b) + c)); // 7
trace("---");

// Nested expressions
trace("a + b - c + b = " + (a + b - c + b)); // 18
trace("(((a + b) - c) + b) = " + (((a + b) - c) + b)); // 18
trace("---");

// Mixing unary minus
trace("a + -b = " + (a + -b)); // 5
trace("a - -b = " + (a - -b)); // 15
