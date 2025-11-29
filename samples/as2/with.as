createEmptyMovieClip("foo", 1);
trace(_name)
with (foo) {
	trace(_name);
}
with (foo) trace(_name)
