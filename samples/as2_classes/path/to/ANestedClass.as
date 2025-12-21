class path.to.ANestedClass extends path.to.ANestedBaseClass implements path.to.ANestedInterface {
    static var message = "Success!";

    public static function create(): ANestedClass {
        return new ANestedClass();
    }

    public function success() {
        trace(ANestedClass.message);
    }
}
