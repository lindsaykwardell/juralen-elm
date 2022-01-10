import celticWarrior from "./celtic-warrior.mp3"
import anInnocentSword from "./an-innocent-sword.mp3"
import crusadeOfTheCastellan from "./crusade-of-the-castellan.mp3"
import guardians from "./guardians.mp3"
import landOfAFolkDivided from "./land-of-a-folk-divided.mp3"
import rememberTheWay from "./remember-the-way.mp3"
import streetsOfSantIvo from "./streets-of-santivo.mp3"
import renaissanceStrings from "./renaissance-strings.mp3"

import victory from "./victory.mp3"
import defeat from "./defeat.mp3"

type Albums = { [key: string]: string[] }

let sub = () => {}

class AudioControl {
    private audio: HTMLAudioElement
    private sfx: HTMLAudioElement
    private music: Albums
    private maxVol: number
    private shuffle: string | null

    constructor(music: Albums) {
        const muted = localStorage.getItem("muted") === "true"

        this.audio = new Audio()
        this.sfx = new Audio()
        this.audio.volume = 0
        this.sfx.volume = 0
        this.music = music
        this.maxVol = muted ? 0 : 1
        this.shuffle = null
    }

    public get muted() {
        return this.maxVol === 0
    }

    play() {
        this.audio.play()
    }

    pause() {
        this.audio.pause()
    }

    stop() {
        this.audio.removeEventListener("ended", sub)
        return new Promise<void>((resolve) => {
            this.fadeOut().then(() => {
                this.audio.pause()
                this.audio.currentTime = 0
                resolve()
            })
        })
    }

    isSongPlaying(song: string) {
        const loc = song.split(":")
        return (
            this.audio.src.includes(this.music[loc[0]][Number(loc[1])]) &&
            !this.audio.paused
        )
    }

    selectSong(song: string) {
        const loc = song.split(":")
        this.audio.src = this.music[loc[0]][Number(loc[1])]
    }

    shuffleAlbum(album: string) {
        this.shuffle = album
        this.nextShuffle()
        sub = this.nextShuffle.bind(this)
        this.audio.addEventListener("ended", sub)
    }

    nextShuffle() {
        let track = -1
        while (!this.music?.[this.shuffle || ""]?.[track]) {
            track = Math.floor(
                Math.random() * this.music?.[this.shuffle || ""]?.length
            )
        }
        this.audio.src = this.music[this.shuffle || ""][track]
        this.fadeIn()
    }

    setMaxVolume(maxVol: number) {
        this.maxVol = maxVol
    }

    setVolume(vol: number) {
        this.audio.volume = vol
    }

    fadeOut() {
        return new Promise<void>((resolve) => {
            const fadeAudio = setInterval(() => {
                if (this.audio.volume !== 0 && this.audio.volume - 0.1 >= 0) {
                    this.audio.volume -= 0.1
                } else {
                    this.audio.volume = 0
                    clearInterval(fadeAudio)
                    resolve()
                }
            }, 200)
        })
    }

    fadeIn() {
        return new Promise<void>((resolve) => {
            this.audio.volume = 0
            this.play()
            const fadeAudio = setInterval(() => {
                if (
                    this.audio.volume < this.maxVol &&
                    this.audio.volume + 0.05 <= 1
                ) {
                    this.audio.volume += 0.05
                } else {
                    clearInterval(fadeAudio)
                    resolve()
                }
            }, 200)
        })
    }

    toggleMute() {
        if (this.muted) {
            localStorage.setItem("muted", "false")
            this.setMaxVolume(1)
            this.fadeIn()
        } else {
            localStorage.setItem("muted", "true")
            this.setMaxVolume(0)
            this.fadeOut()
        }
    }
}

export const audioControl = new AudioControl({
    theme: [celticWarrior],
    inGame: [
        anInnocentSword,
        crusadeOfTheCastellan,
        guardians,
        landOfAFolkDivided,
        rememberTheWay,
        streetsOfSantIvo,
    ],
    afterGame: [renaissanceStrings],
})

export const sfxControl = new AudioControl({
    sfx: [victory, defeat],
})
