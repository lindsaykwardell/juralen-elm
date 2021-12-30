import { Elm } from "./Main.elm"

const root = document.querySelector("#app div")
const app = Elm.Main.init({
    node: root,
})

export const subscribe = <T>(port: string, callback: (payload: T) => void) => {
    app.ports[port]?.subscribe(callback)
}

export const send = <T>(port: string, value: T) => {
    app.ports[port]?.send(value)
}
