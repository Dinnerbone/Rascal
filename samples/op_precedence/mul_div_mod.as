// Use variables instead of literals so compiler won't fold constants.
var a = 20;
var b = 6;
var c = 4;

// Display values we use
trace("a=" + a + ", b=" + b + ", c=" + c);
trace("----");

// Core tests for left-to-right evaluation of *, /, %
trace("a * b / c = " + (a * b / c)); // 30
trace("(a * b) / c = " + ((a * b) / c)); // 30
trace("---");

trace("a / b * c = " + (a / b * c)); // 13.333
trace("(a / b) * c = " + ((a / b) * c)); // 13.333
trace("---");

trace("a % b * c = " + (a % b * c)); // 8
trace("(a % b) * c = " + ((a % b) * c)); // 8
trace("---");

trace("a * b % c = " + (a * b % c)); // 0
trace("(a * b) % c = " + ((a * b) % c)); // 0
trace("---");

trace("a % b / c = " + (a % b / c)); // 0.5
trace("(a % b) / c = " + ((a % b) / c)); // 0.5
trace("---");

trace("a / b % c = " + (a / b % c)); // 3.333
trace("(a / b) % c = " + ((a / b) % c)); // 3.333
trace("---");

trace("Nested: a * b / c * b = " + (a * b / c * b)); // 180
trace("Nested reference: (((a * b) / c) * b) = " + (((a * b) / c) * b)); // 180
