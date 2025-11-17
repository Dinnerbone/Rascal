var a = 12;  // 0b1100
var b = 5;   // 0b0101
var c = 3;   // 0b0011

trace("a=" + a + ", b=" + b + ", c=" + c);
trace("----");

// Bitwise AND
trace("a & b = " + (a & b)); // 4
trace("a & b & c = " + (a & b & c)); // 0
trace("((a & b) & c) = " + ((a & b) & c)); // 0
trace("---");

// Bitwise XOR
trace("a ^ b = " + (a ^ b));   // 9
trace("a ^ b ^ c = " + (a ^ b ^ c)); // 10
trace("((a ^ b) ^ c) = " + ((a ^ b) ^ c)); // 10
trace("---");

// Bitwise OR
trace("a | b = " + (a | b));   // 13
trace("a | b | c = " + (a | b | c)); // 15
trace("((a | b) | c) = " + ((a | b) | c)); // 15
trace("---");

// Mixing precedence: AND > XOR > OR
trace("a & b ^ c = " + (a & b ^ c)); // 7
trace("((a & b) ^ c) = " + ((a & b) ^ c)); // 7
trace("---");

trace("a ^ b | c = " + (a ^ b | c)); // 11
trace("((a ^ b) | c) = " + ((a ^ b) | c)); // 11
