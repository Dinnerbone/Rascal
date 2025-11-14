// Variables so that the flash compiler doesn't optimize away the actual tests
var one = 1;
var two = 2;
var three = 3;
var four = 4;
var five = 5;
var ten = 10;
var zero = 0;
var negOne = -1;
var a;
var b;
var c;
var p;
var s = "s";
var t = "t";

// --- Basic multiplicative vs additive ---
trace("one + two * three = " + (one + two * three));                 // 7   (1 + (2*3))
trace("(one + two) * three = " + ((one + two) * three));             // 9   ((1+2)*3)

// --- Unary binds tighter than multiplicative/additive ---
trace("-one * two = " + (-one * two));                               // -2  ((-1)*2)
trace("-(one * two) = " + (-(one * two)));                           // -2

// --- Prefix/postfix vs binary ---
p = one;
trace("++p + p++ = " + (++p + p++));                                  // 4   (++p -> 2, p++ -> 2, sum 4), final p = 3
p = two;
trace("p++ * 2 = " + (p++ * 2));                                      // 4   (p++ yields 2 then p becomes 3; 2*2=4)
p = two;
trace("2 * ++p = " + (2 * ++p));                                      // 6   (++p makes p=3 then 2*3)

// --- Multiplicative associativity (left-to-right) ---
trace("four / two * two = " + (four / two * two));                    // 4   ((4/2)*2)

// --- Additive vs bitwise shift ---
trace("one + two << one = " + (one + two << one));                   // 6
trace("(one + two) << one = " + ((one + two) << one));                // 6   ((1+2)<<1) = 6
trace("one + (two << one) = " + (one + (two << one)));                // 5   1 + (2<<1) == 1 + 4 = 5

// --- Relational vs additive ---
trace("one + two < five = " + (one + two < five));                    //
trace("(one + two) < five = " + ((one + two) < five));                  // true  (3 < 5)
trace("one + (two < five) = " + (one + (two < five)));                // 2? In AS2, (two < five) is boolean true; adding boolean coerces -> 1: result 2

// --- Equality vs relational (make values different to exercise precedence) ---
trace("one + two == three = " + (one + two == three));                // true
trace("(one + two) == three = " + ((one + two) == three));              // true
trace("one + (two == three) = " + (one + (two == three)));            // 1   (two==three is false -> 0; 1+0=1)

// --- Bitwise vs equality ---
trace("one | two == three = " + (one | two == three));                // true  (1|2 = 3)
trace("(one | two) == three = " + ((one | two) == three));              // true  (1|2 = 3)
trace("one | (two == three) = " + (one | (two == three)));            // true

// --- Bitwise precedence examples ---
trace("one & three | two = " + (one & three   | two));                // 3   (1&3=1, 1|2=3)
trace("(one & three) | two = " + ((one & three) | two));                // 3   (1&3=1, 1|2=3)
trace("one & (three | two) = " + (one & (three | two)));              // 1   (3|2=3, 1&3=1)

// --- Bitwise NOT and unary precedence ---
trace("~one = " + (~one));                                             // -2  (~1 = -2)
trace("-~one = " + (-~one));                                           // 2   (~1 = -2, -(-2) = 2)

// --- Logical AND/OR with comparisons ---
trace("(one < two && three > two) = " + ((one < two && three > two))); // true && true => true
trace("(one > two || three > two) = " + ((one > two || three > two))); // false || true => true

// --- Logical vs bitwise precedence (logical lower than bitwise) ---
trace("one & three && two > zero = " + (one & three && two > zero)); // true-like && true -> true

// --- Ternary operator (?:) precedence vs logical and additive ---
trace("one < two ? five : ten = " + (one < two ? five : ten));        // 5
trace("one + two < three ? five : ten = " + (one + two < three ? five : ten)); // 10

// --- Assignment associativity (right-to-left) and precedence lower than ?: ---
a = 0; b = 0; c = five;
a = b = c;
trace("a after a = b = c -> " + a);                                   // 5
trace("b after a = b = c -> " + b);                                   // 5

// Compound assignment vs multiplicative precedence:
a = 1; b = 2; c = 3;
a += b * c; // a = 1 + (2*3) = 7
trace("a after a += b * c = " + a);                                   // 7

// --- Member access (.) precedence higher than additive (example using object) ---
var obj = new Object();
obj.x = 2;
trace("obj.x + two = " + (obj.x + two));                               // 4   (2+2)

// --- typeof, instanceof (typeof is unary high precedence) ---
trace("typeof one = " + typeof one);                                   // "number"
trace("typeof s = " + typeof s);                                       // "string"

// --- Ternary and assignment combined (?: has higher precedence than =) ---
a = 1; b = 2; c = 3;
a = (b > c) ? b : c; // b>c false => a = c
trace("a after a = (b>c)?b:c = " + a);                                 // 3

// --- Multiple operators in one expression (exercise many precedence levels) ---
a = 1; b = 2; c = 3;
trace("a + b * c << one & three == ((a + (b * c)) << one & three) ? five : ten = " +
    ( (a + b * c << one & three) == ((a + (b * c)) << one & three) ? five : ten )); // 5 (the ternary selects five if equality true)

// --- Unary + and - precedence check ---
trace("+one + +two = " + (+one + +two));                                // 3
trace("-one + -two = " + (-one + -two));                                // -3

// --- Modulus precedence vs addition/subtraction ---
trace("ten % three + one = " + (ten % three + one));                    // 2
trace("(ten % three) + one = " + ((ten % three) + one));                // 2

// --- relational chaining with arithmetic to ensure grouping ---
trace("one + two * three > four = " + (one + two * three > four));   // true  (1+6 > 4)
