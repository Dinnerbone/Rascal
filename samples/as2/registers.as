function(a, b) {
    trace(this);
    trace(arguments);
    trace(super);
    trace(_root);
    trace(_parent);
    trace(_global);
    trace(a);
}
function(a, b) {
    trace(this);
    trace(arguments);
    trace(super);
    trace(_root);
    trace(_parent);
    trace(_global);
    trace(a);
    eval(""); // disable optimisations
}

function(a, b) {
    var c;
}

function(a, b) {
    var c = 2;
    eval(""); // disable optimisations
}
