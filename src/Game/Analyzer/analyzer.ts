import { Elm } from "./Main.elm"
import { ref, watch } from "vue"

let app: any

export default () => {
    app = Elm.Game.Analyzer.Main.init()
    const error = ref<string | null>(null)
    const res = ref<string | null>(null)

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
        res.value = payload
    })

    subscribe("logError", (err: string) => {
        error.value = err
    })

    watch(error, () => console.log(error.value))
    watch(res, () => console.log(res.value))

    return { analyze, error, res }
}
