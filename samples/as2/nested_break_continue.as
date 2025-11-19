for (var i = 0; i < 4; i++) {
	trace("i = " + i);
	for (var j = 0; j < 4; j++) {
		var fn = function() {
			trace("trying to break from fn");
			break;
		}
		trace("j = " + j);
		if (j == 1) {
			fn();
			continue;
		}
		if (j == 2) {
			break;
		}
	}
	if (i == 1) {
		continue;
	}
	if (i == 2) {
		break;
	}
}
