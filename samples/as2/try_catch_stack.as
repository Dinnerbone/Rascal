var bad = function() {
    throw "oh noes!";
};

var foo = function() {
    try {
        return 1 + (1 + bad());
    } catch (e) {
        trace("caught: " + e);
        return 5;
    }
};

trace(foo());
trace(foo() + foo());
