var num = 1;
var obj = {num: 1};
trace("delete num: " + (delete num)); // true
trace("delete obj: " + (delete obj)); // true
var obj = {num: 1};
trace("delete obj.num: " + (delete obj.num)); // true
var obj = {num: 1};
trace("delete obj.num + 1: " + (delete obj.num + 1)); // 2
var obj = {num: 1};
trace("delete obj.num == \"number\" ? \":)\" : \":(\": " + (delete obj.num ? ":)" : ":(")); // :)
var obj = {num: 1};
trace("delete obj.num + delete obj.num: " + (delete obj.num + delete obj.num)); // 1
var obj = {num: 1, trace: trace};
trace("delete obj.trace: " + (delete obj.trace)); // true
var obj = {num: 1, trace: trace};
trace("delete obj.trace(): " + (delete obj.trace())); // false
trace("delete(nonexistent, obj): " + (delete(nonexistent, obj))); // false, because of a Flash compiler bug