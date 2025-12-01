for (var i = 0; i < 4; i++) {
    trace("outer i=" + i);

    for (var j = 0; j < 4; j++) {
        // continue inside inner loop skips some iterations
        if (j == 1) {
            continue;
        }

        // break inner loop only when both i and j meet a condition
        if (i == 2 && j == 2) {
            break;
        }

        trace("  inner j=" + j);
    }

    // continue the outer loop at a specific i
    if (i == 1) {
        continue;
    }

    // break the outer loop when a later condition matches
    if (i == 3) {
        break;
    }
}

trace("done");
