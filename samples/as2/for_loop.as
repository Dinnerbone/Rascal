trace("for_classic");
for (var i = 0; i < 3; i++) {
    trace(i);
}

// ------------------------------------------------------------

trace("for_no_init");
var j = 3;
trace("start");
for (; j < 6; j++) {
    trace(j);
}
trace("end");

// ------------------------------------------------------------

trace("for_no_condition");
for (var k = 0; ; k++) {
    trace(k);
    if (k >= 3) {
        break;
    }
}

// ------------------------------------------------------------

trace("for_no_update");
for (var n = 0; n < 3; ) {
    trace(n);
    n++;
}

// ------------------------------------------------------------

trace("for_all_omitted");
var count = 0;
for (;;) {
    trace("loop");
    count++;
    if (count == 3) {
        break;
    }
}
trace("done");

// ------------------------------------------------------------

trace("for_in_object");
var obj = {};
obj.a = 10;
obj.b = 20;
for (var key in obj) {
    trace(key + "=" + obj[key]);
}

// ------------------------------------------------------------

trace("for_in_array");
var arr = [5, 6, 7];
for (var idx in arr) {
    trace(idx + "=" + arr[idx]);
}

// ------------------------------------------------------------

trace("for_multiple_clauses");
for (var a = 0, b = 3; a < b; a++, b--) {
    trace(a + "," + b);
}

// ------------------------------------------------------------

trace("for_continue_break");
for (var x = 0; x < 10; x++) {
    if (x == 5) {
        break;
    }
    if (x % 2 == 1) {
        continue;
    }
    trace(x);
}

// ------------------------------------------------------------

trace("for_nested");
for (var outer = 0; outer < 2; outer++) {
    for (var inner = 0; inner < 2; inner++) {
        trace(outer + "," + inner);
    }
}

// ------------------------------------------------------------

trace("for_existing_var");
var z;
for (z = 1; z <= 3; z++) {
    trace(z);
}

// ------------------------------------------------------------

trace("for_single_statement_body");
for (var q = 0; q < 3; q++)
    trace(q);