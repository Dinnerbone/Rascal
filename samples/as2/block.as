// The block has no actual functionality, this test guards against adding it by accident <_<
var outside = "visible";
{
    var inside = "visible";
    trace("outside: " + outside + ", inside: " + inside);
}
trace("outside: " + outside + ", inside: " + inside);
