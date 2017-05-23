const nodePath = "./node_modules"

requirejs.config({
  packages: [
    {
      name: 'codemirror',
      location: `${nodePath}/codemirror`,
      main: 'lib/codemirror'
    }
  ],
  paths: {
    // "codemirror": `${nodePath}/codemirror/lib/codemirror`,
    // "codemirror/mode/mllike/mllike": `${nodePath}/codemirror/mode/mllike/mllike`,
    // "codemirror/lib/codemirror": `${nodePath}/codemirror/lib/codemirror`,
  },
  shim: {
    "codemirror/mode/mllike/mllike": { deps: ["codemirror"] },
  },
  waitSeconds: 0,
})

requirejs([
    "codemirror",
    "codemirror/mode/mllike/mllike",
], (CodeMirror) => {
    console.log(`Binding CodeMirror`)
    window.CodeMirror = CodeMirror
    require([
        "output/app",
    ])
})
