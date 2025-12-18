import flash.display.BitmapData;

class ClassMembersTest implements AnEmptyInterface {
    var regularPropertyWithoutValue;
    var regularPropertySetToFive = 5;
    var BitmapData = "BitmapData on class!";

    function test() {
        trace("test!");
    }

    function ClassMembersTest(name: String) {
        greet("world");
        trace(BitmapData);
        trace("regularPropertyWithoutValue: " + regularPropertyWithoutValue);
    }

    function greet(name: String): Void {
        trace("Hey, " + name + "!");
    }

    function maybeABitmapData(): BitmapData {
        return null; // maybe not
    }
}
