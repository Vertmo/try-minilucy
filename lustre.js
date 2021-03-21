(function(mod) {
    if (typeof exports === "object" && typeof module === "object") // CommonJS
        mod(require("../../lib/codemirror"));
    else if (typeof define === "function" && define.amd) // AMD
        define(["../../lib/codemirror"], mod);
    else // Plain browser env
        mod(CodeMirror);
})
(function(CodeMirror) {
    "use strict";

    CodeMirror.defineMode("lustre", function (options) {

        function wordRegexp(words) {
            return new RegExp("^((" + words.join(")|(") + "))\\b");
        }

        var indentKeywords = wordRegexp(["let"]);
        var dedentKeywords = wordRegexp(["tel"]);
        var keywords = wordRegexp(
            ["node","returns", "var",
             "if","then","else",
             "fby", "->",
             "when", "merge",
             "switch", "do", "end",
             "reset", "every",
             "automaton", "state", "until", "unless", "continue",
             "last"]);
        var types = wordRegexp(["int","bool","on"]);
        var atoms = wordRegexp(["true","false"]);
        var numberLiteral = /^(-|)(\d+)/;

        return {
            startState: function() {
                return {
                    indentLevel: 0,
                };
            },
            token: function(stream, state) {
                if (stream.eatSpace()) return null;
                if (stream.match("--")) { stream.skipToEnd(); return "comment"; }

                if (stream.match(indentKeywords)) {
                    state.indentLevel+=options.indentUnit;
                    return "keyword";
                }

                if (stream.match(dedentKeywords)) {
                    state.indentLevel-=options.indentUnit;
                    return "keyword";
                }

                if (stream.match(keywords)) return "keyword";
                if (stream.match(types)) return "type";
                if (stream.match(atoms)) return "atom";
                if (stream.match(numberLiteral)) return "number";


                var ch = stream.next();
                return null;
            },
            indent: function(state, stringAfter) {
                if(stringAfter.match(dedentKeywords)) {
                    return state.indentLevel-options.indentUnit;
                }
                return state.indentLevel;
            },

            electricInput: new RegExp("\s*((tel))"),
            lineComment: "--",
            blockCommentStart: "(*",
            blockCommentEnd: "*)"
        };
    });

    CodeMirror.defineMIME("text/lustre", "lustre");
});
