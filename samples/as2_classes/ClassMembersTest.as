class ClassMembersTest implements AnEmptyInterface {
    var regularPropertyWithoutValue;
    var regularPropertySetToFive = 5;

    function test() {
        trace("test!");
    }

    function ClassMembersTest(name: String) {
        this.greet("world");
    }

    function greet(name: String): Void {
        trace("Hey, " + name + "!");
    }
}
