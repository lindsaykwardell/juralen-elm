//@ts-ignore
import initAnalyzer from "./analyzer/juralen-analyzer.es.js"

const analyzer = initAnalyzer((payload: string) => postMessage(payload))

onmessage = function (e) {
    analyzer.analyze(e.data)
}
