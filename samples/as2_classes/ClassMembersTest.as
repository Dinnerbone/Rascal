class ClassMembersTest implements AnEmptyInterface {
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
