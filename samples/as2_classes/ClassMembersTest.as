import flash.display.BitmapData;
import path.to.ANestedClass;

class ClassMembersTest implements AnEmptyInterface {
    var regularPropertyWithoutValue;
    var regularPropertySetToFive = 5;
    var BitmapData = "BitmapData on class!";
    static var scream = "Ahhh!";

    function test() {
        trace("test!");
    }

    function ClassMembersTest(name: String) {
        greet("world");
        trace(BitmapData);
        trace("regularPropertyWithoutValue: " + regularPropertyWithoutValue);

        trace(ANestedClass.create().success());

        virtualProperty = "a value";
        trace(virtualProperty);
    }

    function greet(name: String): Void {
        trace("Hey, " + name + "!");
        shoutStatically();
    }

    static function shoutStatically() {
        trace(scream);
    }

    function maybeABitmapData(): BitmapData {
        return null; // maybe not
    }

    private function secret() {}

    public function notASecret() {}

    function get virtualProperty() {
        return "virtual!";
    }

    function get onlyGetter() {
        return "getter";
    }

    function set onlySetter(value) {
        trace("New value: " + value);
    }

    function set virtualProperty(value) {
        trace("Virtually setting to " + value);
    }

    static function get virtualPropertyProbablyStatic() {
        return "Static!";
    }

    function set virtualPropertyProbablyStatic(value) {
        trace("New value: " + value);
    }

    function set virtualPropertyProbablyNotStatic(value) {
        trace("New value: " + value);
    }

    static function get virtualPropertyProbablyNotStatic() {
        return "Static!";
    }
}
