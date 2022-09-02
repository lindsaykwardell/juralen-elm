import { Elm } from "./Main.elm"

let app: any
let stalledTimeout: null | NodeJS.Timeout = null

export default (analyzed: (option: string) => void) => {
    app = Elm.Game.Analyzer.Main.init()

    const subscribe = <T>(port: string, callback: (payload: T) => void) => {
        app?.ports[port]?.subscribe(callback)
    }
    const send = <T>(port: string, value: T) => {
        app?.ports[port]?.send(value)
    }

    function initStallTimeout() {
        stalledTimeout = setTimeout(() => {
            console.error("TIMEOUT OCCURRED!")
            analyzed("[]")
        }, 2000)
    }

    const analyze = (game: string) => {
        send("analyze", game)
        initStallTimeout()
    }

    subscribe("analyzed", (payload: string) => {
        analyzed(payload)
        // @ts-ignore
        clearTimeout(stalledTimeout)
        stalledTimeout = null
    })

    subscribe("logError", (err: string) => {
        console.error(err)
    })

    return { analyze }
}
