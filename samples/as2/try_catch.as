trace(" -- try catch finally -- ");

try {
	trace("try!");
    var message = "Hello World!";
    throw message;
} catch (e) {
	trace("catch!");
    trace(e);
} finally {
	trace("finally!");
}

trace(" -- try catch -- ");
try {
	trace("try!");
	throw "test";
} catch(e) {
	trace("caught something! " + e);
}

trace(" -- try catch(MovieClip) finally -- ");
try {
	trace("try!");
	throw this;
} catch(e: MovieClip) {
	trace("caught something! " + e);
} finally {
    trace("finally!");
}

trace(" -- try catch(MovieClip) catch(Number) --");
try {
	trace("try!");
	throw this;
} catch(e: MovieClip) {
	trace("catch something of MovieClip!");
} catch(e: Number) {
	trace("catch something of Number!");
} catch(e) {
	trace("catch anything else!");
}

// This one must be last! :D
trace(" -- try finally -- ");

try {
	trace("try!");
    throw null;
} finally {
	trace("finally!");
}

trace("If you see this, it didn't throw! :(")
