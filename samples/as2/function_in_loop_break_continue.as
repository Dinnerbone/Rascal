for (var i = 0; i < 5; i++) {
    trace("loop i=" + i);

    // A helper that returns signals to the loop
    var decide = function(v) {
        if (v == 1) {
            return "continue";
        }
        if (v == 3) {
            return "break";
        }
        return "none";
    };

    // A function shouldn't have the break/continue of the parent block
    var naughty = function() {
        break;
    };
    naughty();

    var action = decide(i);
    if (action == "continue") {
        continue;
    }
    if (action == "break") {
        break;
    }

    trace("  work i=" + i);
}

trace("end");
