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

// -- Unary add
// Bools
trace(+true);
trace(+false);
// Ints
trace(+0);
trace(+1);
trace(+(-1));
// Floats
trace(+0.0);
trace(+0.1);
// Strings
trace(+"123");
trace(+"2.5");
trace(+"0");
trace(+"");
trace(+"true");
trace(+"false");
// Confirm anything else isn't touched
trace(+null);
trace(+undefined);
trace(+foo);

// -- Unary bit not
// Bools
trace(~true);
trace(~false);
// Ints
trace(~0);
trace(~1);
trace(~(-1));
trace(~5);
// Floats
trace(~0.0);
trace(~0.1);
trace(~5.2);
// Strings
trace(~"123");
trace(~"2.5");
trace(~"0");
trace(~"");
trace(~"true");
trace(~"false");
// Confirm anything else isn't touched
trace(~null);
trace(~undefined);
trace(~foo);

// -- Binary add
trace("a" + "b" + 3 + true + 123.45);
trace("1" + "2")
trace("1" + 2)
trace(1 + "2")
trace(1 + 2)
trace(1 + 2.0)
trace(1.0 + 2);
trace(true + false);
trace(true + "false");

// -- Binary sub
trace("1" - "2")
trace("1" - 2)
trace(1 - "2")
trace(1 - 2)
trace(1 - 2.0)
trace(1.0 - 2);
trace(true - false);
trace(true - "false");
