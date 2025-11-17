var a = 16;   // 0b00010000
var b = 2;
var c = 1;

trace("a=" + a + ", b=" + b + ", c=" + c);
trace("----");

// Left-to-right evaluation
trace("a << b >> c = " + (a << b >> c)); // 32
trace("((a << b) >> c) = " + ((a << b) >> c)); // 32
trace("---");

trace("a >> b << c = " + (a >> b << c)); // 8
trace("((a >> b) << c) = " + ((a >> b) << c)); // 8
trace("---");

trace("a >>> b >> c = " + (a >>> b >> c)); // 2
trace("((a >>> b) >> c) = " + ((a >>> b) >> c)); // 2
trace("---");

// Nested shifts
trace("a << b >> c >>> b = " + (a << b >> c >>> b)); // 8
trace("(((a << b) >> c) >>> b) = " + (((a << b) >> c) >>> b)); // 8
trace("---");

// Mixing with + and * to verify relative precedence
trace("a << b + c = " + (a << b + c)); //128
trace("a << (b + c) = " + (a << (b + c)));  // 128, + has lower precedence than <<?
trace("---");
trace("a * b << c = " + (a * b << c)); // 64
trace("(a * b) << c = " + ((a * b) << c)); // 64, * has higher precedence than <<
