// Simple line comment at start of file
/// Triple-slash doc-style comment (should behave as line comment)

var a = "1"; // Trailing comment after statement

// Multiple line comments
// stacked
// together

/* Single-line block comment */ var b = "2"; /* trailing block */

var /* between keyword and ident */ c /* between ident and op */ = /* between op and literal */ "3"; /* end */

/*
 Multi-line block comment
 Spanning several lines
*/
var d = "4"; /* block comment after statement on same line */

test( /* before arg */ a /* between args */, /* before second arg */ b ); // call with comments inside

// Comment-only line followed by code
var e = "5";

var f = "// not a comment"; // string containing slashes
var g = "/* not a comment */"; // string containing block markers
var h = "\"/* still not a comment */\""; // escaped quotes around block markers

/* Empty block: */ /**/ var i = "6"; /**/ // ensure empty block is handled

/* Block comment that
   looks like it might nest: /* inner */// but should end at first */
var j = "7";

/* Block comment that ends at EOF (no trailing newline) */ var k = "8"; /* EOF block */