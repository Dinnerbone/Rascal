class path.to.ANestedClass {
    static var message = "Success!";

    public static function create(): ANestedClass {
        return new ANestedClass();
    }

    public function success() {
        trace(ANestedClass.message);
    }
}
