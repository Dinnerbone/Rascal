function test() {
    trace("Test called!");
    return ":)";
}

function(a, b){
    trace("Anonymous function called!");
    trace("a = " + a + ", b = " + b);
}("aa", "bb");

trace(test() + test());
