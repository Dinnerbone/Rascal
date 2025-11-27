var x = 0;
while (x < 5) {
	trace(x);
	x += 1;
}

while (x >= 0) x--;

trace("--");

while (true) {
	if (x++ == 2) {
		continue;
	}
	if (x == 5) {
		break;
	}
	trace(x);
}
trace("end");
