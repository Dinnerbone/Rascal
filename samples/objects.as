var o = {a: 1, b: Object};
trace("o.a: " + o.a);
trace("o.b: " + o.b);
trace("new Object(): " + new Object());
trace("new Object(1, 2, {}, false): " + new Object(1, 2, {}, false));
trace("new Object: " + new Object);
trace("new o.b(): " + new o.b());
