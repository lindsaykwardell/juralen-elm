import initAnalyzer from "./analyzer/juralen-analyzer.es.js"

console.log("worker.js loaded")

const analyzer = initAnalyzer()

onmessage = function (e) {
    analyzer.analyze(e.data)
}
