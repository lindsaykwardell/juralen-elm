import "./style.css"
import "./src/Components"
import app from "./src/app"
import { registerSW } from "virtual:pwa-register"

const updateSW = registerSW({
    // onNeedRefresh() {},
    // onOfflineReady() {},
})

setTimeout(() => {
    app.ports.authStatus.send(true)
}, 2000)
