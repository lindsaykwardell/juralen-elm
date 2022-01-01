import "./style.css"
import "./src/Components"
import { send, subscribe } from "./src/app"
import audioControl from "./src/audio/audioControl"
// @ts-ignore
import { registerSW } from "virtual:pwa-register"
// @ts-ignore
import Worker from "./src/worker?worker"
const analyzerWorker = new Worker()

registerSW({
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
subscribe("saveGame", (game: string) => {
    localStorage.setItem("game", game)
})
subscribe("loadGame", () => {
    const game = localStorage.getItem("game")
    if (game) {
        send("gameLoaded", game)
    }
})

// Analyzer
subscribe("analyze", (game: string) => analyzerWorker.postMessage(game))
analyzerWorker.onmessage = (e: any) => {
    send("analyzed", e.data)
}
