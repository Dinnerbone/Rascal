function test() {
    trace("Test called!");
    return ":)";
}

var anon = function(a, b){
    trace("Anonymous function called!");
    trace("a = " + a + ", b = " + b);
};
anon("aa", "bb");

trace(test() + test());

