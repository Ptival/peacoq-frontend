"use strict"

function toSexp(atom, list, e) {
    function go(e) {
        if (Array.isArray(e)) {
            return list(e.map(go))
        } else {
            return atom(e)
        }
    }
    return go(e)
}

exports._sexp = function(nothing, just, atom, list, s) {
    const res = window.sexp(s)
    if (res instanceof Error) {
        return nothing
    }
    return just(toSexp(atom, list, res))
}
