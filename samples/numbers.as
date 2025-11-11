// Decimal integers
var i0 = 0;
var i1 = 1;
var i42 = 42;
var iLarge = 2147483647; // max i32
var iLargePlusOne = 2147483648; // one over i32

// Floating-point (decimal)
var f0 = 0.0;
var f1 = 1.23;
var fLeadingDot = .5;  // leading dot
var fTrailingDot = 5.; // trailing dot

// Hex integers (case-insensitive hex digits)
var h0 = 0x0;
var hMix = 0x1A2b;
var hMaxByte = 0xFF;

// Exponent notation
var eBig = 1e10;
var eSmall = 1E-10;
var ePlus = 2.5e+3;
var eLeadingDot = .5e2;
var eTrailingDot = 5.e3;
var eSci = 6.022e23;

trace("i0 = " + i0);
trace("i1 = " + i1);
trace("i42 = " + i42);
trace("iLarge = " + iLarge);
trace("iLargePlusOne = " + iLargePlusOne);

trace("f0 = " + f0);
trace("f1 = " + f1);
trace("fLeadingDot = " + fLeadingDot);
trace("fTrailingDot = " + fTrailingDot);

trace("h0 = " + h0);
trace("hMix = " + hMix);
trace("hMaxByte = " + hMaxByte);

trace("eBig = " + eBig);
trace("eSmall = " + eSmall);
trace("ePlus = " + ePlus);
trace("eLeadingDot = " + eLeadingDot);
trace("eTrailingDot = " + eTrailingDot);
trace("eSci = " + eSci);
