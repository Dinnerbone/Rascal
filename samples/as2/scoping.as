function() {
	var a = "outer_a";
	var b = "outer_b";
	var c = "outer_c";
	{
		a = "inner_a";
		var b = "inner_b";
	}
}

function() {
	var a = "outer_a";
	var b = "outer_b";
	var c = "outer_c";
	if ("a") {
		a = "inner_a";
		var b = "inner_b";
	}
}

function() {
	var a = "outer_a";
	var b = "outer_b";
	var c = "outer_c";
	if (!"a") {
	} else {
		a = "inner_a";
		var b = "inner_b";
	}
}

function() {
	var a = "outer_a";
	var b = "outer_b";
	var c = "outer_c";
	for (var i = 0; i < 1; i++) {
		a = "inner_a";
		var b = "inner_b";
	}
}

function() {
	var a = "outer_a";
	var b = "outer_b";
	var c = "outer_c";
	var i = 0;
	while (i++ < 1) {
		a = "inner_a";
		var b = "inner_b";
	}
}

function() {
	var a = "outer_a";
	var b = "outer_b";
	var c = "outer_c";
	switch (1) {
		case 1:
			a = "inner_a";
			var b = "inner_b";
			break;
	}
}

function() {
	var a = "outer_a";
	var b = "outer_b";
	var c = "outer_c";
	try {
		throw "err";
	} catch (e) {
		a = "inner_a";
		var b = "inner_b";
	}
}

function() {
	var a = "outer_a";
	var b = "outer_b";
	var c = "outer_c";
	try {
		a = "try_a";
		var b = "try_b";
	} finally {
		a = "finally_a";
		var b = "finally_b";
	}
}

function() {
	var a = "outer_a";
	var b = "outer_b";
	var c = "outer_c";
	function inner() {
		a = "inner_a";
		var b = "inner_b";
	}
	inner();
}

function() {
	var a = "outer_a";
	var b = "outer_b";
	var c = "outer_c";
	var obj = { x: 1 };
	for (var k in obj) {
		a = "inner_a";
		var b = "inner_b";
	}
}

function() {
	var a = "outer_a";
	function inner() {
	// Any eval(), anywhere in nested scope, kills any optimisations of the parent scopes
		trace(eval(a));
	}
	inner();
}

function() {
	var a = "a";
	// Any with(), anywhere in nested scope, kills any optimisations of the parent scopes
	function() {
		with(_global) {}
	}
}
