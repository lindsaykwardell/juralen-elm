//@ts-ignore
import initAnalyzer from "./analyzer/juralen-analyzer.es.js"

console.log("worker.js loaded")

const analyzer = initAnalyzer((payload: string) => postMessage(payload))

onmessage = function (e) {
    analyzer.analyze(e.data)
}
