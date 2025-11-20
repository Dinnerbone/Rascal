tellTarget(x) {
	trace("this is x");

	tellTarget("y") {
		trace("this is y");
		tellTarget("z") {
			trace("this is z");
		}
		trace("this is still y");
	}

	trace("this is x again");
}
trace("this is root");
tellTarget(x);
trace("this is still root");
tellTarget(x) trace("this is x");
trace("this is still root");
