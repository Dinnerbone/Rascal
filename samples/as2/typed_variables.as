var num:Number = 42;
var str:String = "Hello";
var flag:Boolean = true;
var obj:Object = {};
var arr:Array = [];

function add(a:Number, b:Number):Number {
    return a + b;
}

function getMessage():String {
    return "Test message";
}

function processData(data:Object):void {
    trace(data);
}

var numbers:Array = new Array();
var strings:Array = ["one", "two", "three"];

var someValue:Object = "123";
var numValue:Number = Number(someValue);
var strValue:String = String(42);

function complexFunc(name:String, age:Number, isValid:Boolean):Object {
    return {name: name, age: age, valid: isValid};
}

var userData:Object = {
    id:Number = 1,
    name:String = "User",
    scores:Array = [1, 2, 3]
};

function forEach(arr:Array, callback:Function):void {
    for (var i:Number = 0; i < arr.length; i++) {
        callback(arr[i]);
    }
}
