for (var i = 0; i < 6; i++) {
    trace("loop i=" + i);
    try {
        trace("  try:i=" + i);
        if (i == 1) {
            // continue from within try
            continue;
        }
        if (i == 2) {
            // throw, to be handled by catch which may break
            throw "boom@2";
        }
        if (i == 4) {
            // throw, to test finally + catch + continue/break mix
            throw "boom@4";
        }
    } catch (e) {
        trace("  catch:" + e);
        if (e == "boom@2") {
            // break the outer loop from catch
            break;
        }
        if (e == "boom@4") {
            // continue the outer loop from catch
            continue;
        }
    } finally {
        trace("  finally:i=" + i);
        // Attempting control flow from finally can be tricky for compilers
        if (i == 3) {
            continue;
        }
        if (i == 5) {
            break;
        }
    }

    trace("  after try/catch/finally i=" + i);
}

trace("done");
