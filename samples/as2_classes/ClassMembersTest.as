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
}
