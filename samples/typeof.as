var num = 1;
var obj = {num: 1, trace: trace};
trace("typeof num: " + (typeof num)); // number
trace("typeof obj: " + (typeof obj)); // object
trace("typeof obj.num: " + (typeof obj.num)); // number
trace("typeof obj.num + 1: " + (typeof obj.num + 1)); // number1
trace("typeof obj.num == \"number\" ? \":)\" : \":(\": " + (typeof obj.num == "number" ? ":)" : ":(")); // :)
trace("typeof obj.num + typeof obj.num: " + (typeof obj.num + typeof obj.num)); // numbernumber
trace("typeof obj.trace: " + (typeof obj.trace)); // function
trace("typeof obj.trace(): " + (typeof obj.trace())); // undefined
trace("typeof(nonexistent, obj): " + (typeof(nonexistent, obj))); // object
