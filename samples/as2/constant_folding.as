// -- Unary not
// Bools
trace(!!true);
trace(!!false);
// Ints
trace(!!0);
trace(!!1);
trace(!!-1);
// Floats
trace(!!0.0);
trace(!!0.1);
// Strings
trace(!!"123");
trace(!!"2.5");
trace(!!"0");
trace(!!"1a");
trace(!!"");
trace(!!"true");
trace(!!"false");
// Confirm anything else isn't touched
trace(!!null);
trace(!!undefined);
trace(!!foo);

// -- Unary subtract
// Bools
trace(-true);
trace(-false);
// Ints
trace(-0);
trace(-1);
trace(-(-1));
// Floats
trace(-0.0);
trace(-0.1);
// Strings
trace(-"123");
trace(-"2.5");
trace(-"0");
trace(-"1a");
trace(-"");
trace(-"true");
trace(-"false");
// Confirm anything else isn't touched
trace(-null);
trace(-undefined);
trace(-foo);

trace("a" + "b" + 3 + true + 123.45);
