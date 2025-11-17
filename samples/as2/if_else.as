var condition = true;
var x = 1;
var y = -1;

// If without braces - single line
if (condition) doSomething();
if (x > 0) trace("positive"); else trace("negative");

// If/else if/else chains
if (x < 0) {
    trace("negative");
} else if (x > 0) {
    trace("positive");
} else if (x == 0) {
    trace("zero");
} else {
    trace("undefined");
}

// Empty blocks and single statements
if (condition) {}
if (condition) {} else {}
if (condition) trace("single"); else {}

// Nested if/else if without braces
if (x > 0)
    if (y > 0)
        trace("both positive");
    else if (y < 0)
        trace("x positive, y negative");
    else
        trace("x positive, y zero");

// Mixed braces and no braces
if (condition) {
    doSomething();
} else if (otherCondition)
    doSomethingElse();
else {
    doFinal();
}

// Multiple statements without braces (ofc doesn't work as it "looks like" it would)
if (condition)
    statement1();
    statement2();  // This will always execute!

// Dangling else cases
if (x)
    if (y)
        trace("y");
else
    trace("ambiguous else");
