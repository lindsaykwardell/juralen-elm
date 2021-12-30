import "./style.css"
import "./src/Components"
import { send, subscribe } from "./src/app"
import audioControl from "./src/audio/audioControl"
// @ts-ignore
import { registerSW } from "virtual:pwa-register"
// import init, { add, main, analyze } from "juralen-analyzer"
// @ts-ignore
import Worker from "./src/worker?worker"
// import initAnalyzer from "./src/analyzer/juralen-analyzer.es.js"

// await init()
const analyzerWorker = new Worker()
// const analyzer = initAnalyzer()

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
subscribe("saveGame", (game: string) => {
    localStorage.setItem("game", game)
    // analyzer.analyze(game)
    analyzerWorker.postMessage(game)
})
subscribe("loadGame", () => {
    const game = localStorage.getItem("game")
    if (game) {
        send("gameLoaded", game)
    }
})
