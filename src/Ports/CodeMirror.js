"use strict";

// Eff-ize a function, takes as input the function
function effizeF(f) {
    return function () {
        const args = arguments
        return function () {
            return f.apply(null, args)
        }
    }
}

// Eff-ize a method, takes as input the method name (as a string)
function effizeM(m) {
    return function () {
        var me = arguments[arguments.length - 1]
        var args = Array.prototype.slice.call(arguments, 0, -1)
        return function () {
            return me[m].apply(me, args)
        }
    }
}

exports._codeMirror = effizeF(CodeMirror)
exports._getDoc     = effizeM("getDoc")
exports._hasFocus   = effizeM("hasFocus")
