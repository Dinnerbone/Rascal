var firstVar = 1;
firstFunction();
secondFunction();
function firstFunction() {
	trace("first function!");
	functionWithinAFunction();
	trace("firstVar = " + firstVar + ", secondVar = " + secondVar);

	function functionWithinAFunction() {
		trace("function within a function!");
	}
}
var secondVar = 2;
firstFunction();
secondFunction();
function secondFunction() {
	trace("second function!");
	trace("firstVar = " + firstVar + ", secondVar = " + secondVar);
}
firstFunction();
secondFunction();
function secondFunction() {
	trace("second function! (but redefined!)");
	trace("firstVar = " + firstVar + ", secondVar = " + secondVar);
}
firstFunction();
secondFunction();
functionWithinAFunction();
function thirdFunction() {
	trace("third function!");
}
thirdFunction();
function unusedFunction() {
	trace("Unused function!");
}
