import { Elm } from "./Main.elm"

const root = document.querySelector("#app div")
const app = Elm.Main.init({
    node: root,
})

export const subscribe = (port, callback) => {
    app.ports[port]?.subscribe(callback)
}

export const send = (port, value) => {
    app.ports[port]?.send(value)
}
