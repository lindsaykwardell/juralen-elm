import "./style.css"
import "./src/Components"
import { send, subscribe } from "./src/app"
import { audioControl, sfxControl } from "./src/audio/audioControl"
// @ts-ignore
import { registerSW } from "virtual:pwa-register"
// @ts-ignore
import Worker from "./src/worker?worker"

registerSW()
const analyzerWorker = new Worker()

// Music
subscribe("toggleMute", () => {
    audioControl.toggleMute()
    sfxControl.toggleMute()
})
subscribe("playThemeMusic", async () => {
    if (audioControl.isSongPlaying("theme:0")) return
    await audioControl.stop()
    audioControl.selectSong("theme:0")
    audioControl.fadeIn()
})
subscribe("playGameMusic", async () => {
    await audioControl.stop()
    audioControl.shuffleAlbum("inGame")
})

// SFX
subscribe("playEndGameSfx", (isHumanWinner) => {
    const song = isHumanWinner ? "sfx:0" : "sfx:1"

    audioControl.pause()
    sfxControl.selectSong(song)
    sfxControl.fadeIn()

    const playAftergameMusic = setInterval(() => {
        if (!sfxControl.isSongPlaying(song)) {
            clearInterval(playAftergameMusic)
            audioControl.shuffleAlbum("afterGame")
        }
    }, 1000)
})

// Save/Load game
subscribe("saveGame", (game: string) => {
    localStorage.setItem("game", game)
})
subscribe("loadGame", () => {
    const game = localStorage.getItem("game")
    if (game) {
        send("gameLoaded", game)
    }
})

// Save/load lobby settings
subscribe("saveLobby", (settings: string) => {
    localStorage.setItem("lobbySettings", settings)
})
subscribe("loadLobby", () => {
    const settings = localStorage.getItem("lobbySettings")
    if (settings) {
        send("lobbyLoaded", settings)
    }
})

// Analyzer
subscribe("analyze", (game: string) => analyzerWorker.postMessage(game))
analyzerWorker.onmessage = (e: any) => {
    send("analyzed", e.data)
}
