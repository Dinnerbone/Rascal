function test() {
    trace("Test called!");
}

function(a, b){
    trace("Anonymous function called!");
    trace("a = " + a + ", b = " + b);
}("aa", "bb");
