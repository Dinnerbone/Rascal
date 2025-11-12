// (variables in this file are because flash will optimise these out, and we want to compare with them)

var a = 10;
var b = 3;

trace("a + b = " + (a + b)); // 13
trace("a - b = " + (a - b)); // 7
trace("a * b = " + (a * b)); // 30
trace("a / b = " + (a / b)); // 3.3333333333
trace("a % b = " + (a % b)); // 1

trace("++a = " + (++a)); // 11
trace("--a = " + (--a)); // 10
trace("a++ = " + (a++)); // 10 (then a=11)
trace("a-- = " + (a--)); // 11 (then a=10)

var hello = "Hello";
var world = "World";
trace('hello + " " + world = ' + (hello + " " + world)); // Hello World

// Assignment operators
var x = 5;
trace("x = " + x); // 5
trace("x += 2 => " + (x += 2)); // 7
trace("x -= 2 => " + (x -= 2)); // 5
trace("x *= 3 => " + (x *= 3)); // 15
trace("x /= 5 => " + (x /= 5)); // 3
trace("x %= 2 => " + (x %= 2)); // 1

// Bitwise operators
var five = 5;
var three = 3;
trace("five & three = " + (five & three)); // 1
trace("five | three = " + (five | three)); // 7
trace("five ^ three = " + (five ^ three)); // 6
trace("~five = " + (~five)); // -6
trace("five << 1 = " + (five << 1)); // 10
trace("five >> 1 = " + (five >> 1)); // 2
trace("five >>> 1 = " + (five >>> 1)); // 2
