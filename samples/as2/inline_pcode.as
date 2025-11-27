function traceThroughPcode(message) {
    trace("-- start of a very special message --");
    @PCode {
        Push "message"
        GetVariable
        Trace
    }
    trace("-- end special message --");
}

traceThroughPcode("Hello, world!");
