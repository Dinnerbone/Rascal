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
trace(!!"a");
trace(!!"");
trace(!!"true");
trace(!!"false");
// Confirm anything else isn't touched
trace(!!null);
trace(!!undefined);
trace(!!foo);

trace("a" + "b" + 3 + true + 123.45);
