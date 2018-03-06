/* boolexman -- boolean expression manipulator
Copyright (c) 2018 Mert Bora ALPER <bora@boramalper.org>

Permission to use, copy, modify, and/or distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
*/

var promptCounter = 1;
var inputHistory = [];

var line     = undefined;
var prompt   = undefined;
var terminal = undefined;


document.onload = function () {
	line     = document.getElementById("line");
	prompt   = document.getElementById("prompt");
	terminal = document.getElementById("terminal");

	line.addEventListener("keyup", function(event) {
    	event.preventDefault();
    	if (event.keyCode === 13) {  // Enter
    		var input = event.target.value;

    		inputHistory.push(input);
    		pushTerminal(prompt.innerHTML + "> ");
			pushTerminal(input + "\n");
			event.target.value = "";
			updatePrompt();

	        processInput(input);
    	}
	});
}();


function updatePrompt(argument) {
	// https://stackoverflow.com/a/29833199/4466589
	prompt.innerHTML = ("000" + ++promptCounter).slice(-4);
	assert(promptCounter <= 9999);
}


function processInput(input) {
	var res = boolexman(input);

	if (res.startsWith("I")) {
		pushTerminal(res.slice(1) + "\n");
	} else if (res.startsWith("D")) {
		display(res.slice(1));
	} else {
		window.alert("BUG! boolexman's output starts neither with 'I' nor with 'D'!");
	}
}


function pushTerminal(text) {
	terminal.innerHTML += format(text);
}


function display(raw) {
	var win = window.open("", "", "centerscreen,toolbar=no,location=no,directories=no,status=no,menubar=no,scrollbars=yes,resizable=yes,width=1024,height=768");
	win.document.body.innerHTML = `
		<!doctype html>

		<html lang="en-GB">
  		<head>
		    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
		    <link href="https://fonts.googleapis.com/css?family=Roboto+Mono:400,700&amp;subset=cyrillic,greek" rel="stylesheet">
		    <meta name="viewport" content="width=device-width, initial-scale=1">
		</head>
		<body>
			<pre style="font-family: 'Roboto Mono', monospace; white-space: pre; line-height: 1.1;">` + format(raw) + `</pre>
		</body>
		</html>
	`;

	'' + format(raw) + "</pre>";
}


function format(raw) {
	var openTags = [];  // Stack (FILO); Store Only the Tag Name (e.g. "b", "u")
	var formatted = "";

	for (var i = 0; i < raw.length; i++) {
		var slice = raw.slice(i);

		if (slice.startsWith("\x1b[")) {
			switch (slice.charAt(2)) {
			case "1":  // bold
				formatted += "<b>";
				openTags.push("b");
				break;

			case "9":  // strike
				formatted += "<strike>";
				openTags.push("strike");
				break;

			case "4":  // underline
				formatted += "<u>";
				openTags.push("u");
				break;

			case "7":  // reverse
				formatted += '<span style="background-color: black; color: white;">';
				openTags.push("span");
				break;

			case "0":  // (close tag)
				var closingTagName = openTags.pop()
				assert(closingTagName !== undefined);
				formatted += "</" + closingTagName + ">";
				break;
			}

			i += 3; // skip the tag
		} else {
			var char = slice.charAt(0);
			switch (char) {
			case "\n":
				formatted += "<br>";
				break;

			case " ":
				formatted += "&nbsp;";
				break;

			default:
				formatted += char;
				break;
			}
		}
	}

	return formatted;
}


// https://stackoverflow.com/a/15313435/4466589
function assert(condition, message) {
    if (!condition) {
        message = message || "Assertion failed";
        if (typeof Error !== "undefined") {
            throw new Error(message);
        }
        throw message; // Fallback
    }
}
