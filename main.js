import "./style.css"
import "./src/Components"
import { send, subscribe } from "./src/app"
import audioControl from "./src/audio/audioControl"
import { registerSW } from "virtual:pwa-register"

const updateSW = registerSW({
    // onNeedRefresh() {},
    // onOfflineReady() {},
})

setTimeout(() => {
    send("authStatus", true)
}, 2000)

subscribe("toggleMute", () => audioControl.toggleMute())
subscribe("playThemeMusic", async () => {
    await audioControl.stop()
    audioControl.selectSong("theme:0")
    audioControl.fadeIn()
})
subscribe("playGameMusic", async () => {
    await audioControl.stop()
    audioControl.shuffleAlbum("inGame")
})
subscribe("saveGame", (game) => localStorage.setItem("game", game))
subscribe("loadGame", () => {
    const game = localStorage.getItem("game")
    if (game) {
        send("gameLoaded", game)
    }
})
