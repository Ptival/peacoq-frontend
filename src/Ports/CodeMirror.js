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
        const me = arguments[0]
        const args = Array.prototype.slice.call(arguments, 1)
        // console.log("Will call ", me, "'s method", m, " with arguments ", args)
        return function () {
            // console.log("About to call ", me, "'s method", m, " with arguments ", args)
            return me[m].apply(me, args)
        }
    }
}

function on(eventType, kArgs) {
    return function(self, callback) {
        return function() {
            return self.on(eventType, function() {
                if (kArgs.length != arguments.length) {
                    throw "FFI problem: for " + eventType + ", wrong kArgs length"
                }
                const args = {}
                for (var i in kArgs) {
                    args[kArgs[i]] = arguments[i]
                }
                callback(args)()
            })
        }
    }
}

exports._onCodeMirrorChange = on("change", ["instance", "changeObj"])

exports._codeMirror = effizeF(CodeMirror)
exports._getDoc     = effizeM("getDoc")
exports._getValue   = effizeM("getValue")
exports._hasFocus   = effizeM("hasFocus")
exports._markText   = effizeM("markText")
