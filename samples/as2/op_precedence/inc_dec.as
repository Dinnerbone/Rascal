var a = 10;

// This is because 'trace' is a special function, and we want to see how it interacts with regular functions
var trc = function(msg) {
    trace(msg);
}

trc(a++);
trc(a--);
trc(++a);
trc(--a);
