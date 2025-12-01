for (var i = 0; i < 6; i++) {
    trace("loop i=" + i);
    switch (i) {
        case 0:
            trace("  case 0 -> continue outer");
            continue; // continues the for-loop, not just the switch

        case 1:
        case 2: {
            trace("  case 1/2 -> fallthrough then break outer at 2");
            if (i == 2) {
                break; // breaks the for-loop
            }
            // otherwise, just exit switch via break with a block label-less style
            break;
        }

        case 3:
            trace("  case 3 -> break switch only");
            // Use an empty statement to emphasize switch-only break
            break;

        case 4:
            trace("  case 4 -> continue outer from nested block");
            {
                // nested block before continue
                if (true) {
                    continue;
                }
            }

        default:
            trace("  default -> normal work");
    }

    trace("  after switch i=" + i);
}

trace("done");
