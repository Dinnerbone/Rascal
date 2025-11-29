switch(x) {
	case 1: trace("1!");
	case 2: {
		trace("1 or 2!");
		break;
	}
	case c: {
		trace("c!");
	}
	default:
		trace("c or default!")
		trace("still c or default! no braces!");
		break
	case null:
		trace("null!");
}
trace("--");
switch(x) {
	trace("no case!")
}
trace("--");
switch(x) {
	default: trace("only default!")
}
trace("--");
switch(x) {
	case true:
}
trace("end");
