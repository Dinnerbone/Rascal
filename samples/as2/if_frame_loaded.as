ifFrameLoaded(2) trace("single line");

ifFrameLoaded("scene", frame) {
	trace("block");
}

ifFrameLoaded(3) if (this) {
	trace("nested conditions!");
}

trace("end!");
