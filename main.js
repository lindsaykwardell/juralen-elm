import "./style.css"
import "./src/Components"
import app from "./src/app"
import audioControl from "./src/audio/audioControl"
import { registerSW } from "virtual:pwa-register"

const updateSW = registerSW({
    // onNeedRefresh() {},
    // onOfflineReady() {},
})

setTimeout(() => {
    app.ports.authStatus.send(true)
}, 2000)

app.ports.playThemeMusic.subscribe(async () => {
    await audioControl.stop()
    audioControl.selectSong("theme:0")
    audioControl.fadeIn()
})

app.ports.playGameMusic.subscribe(async () => {
    await audioControl.stop()
    audioControl.shuffleAlbum("inGame")
})
