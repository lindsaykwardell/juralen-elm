import { Elm } from "./Main.elm"

let app: any

export default (analyzed: (option: string) => void) => {
    app = Elm.Game.Analyzer.Main.init()

    const subscribe = <T>(port: string, callback: (payload: T) => void) => {
        app?.ports[port]?.subscribe(callback)
    }
    const send = <T>(port: string, value: T) => {
        app?.ports[port]?.send(value)
    }

    const analyze = (game: string) => {
        send("analyze", game)
    }

    subscribe("analyzed", (payload: string) => {
        analyzed(payload)
    })

    subscribe("logError", (err: string) => {
        console.error(err)
    })

    return { analyze }
}
