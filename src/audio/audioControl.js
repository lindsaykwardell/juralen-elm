import celticWarrior from "./celtic-warrior.mp3"
import anInnocentSword from "./an-innocent-sword.mp3"
import crusadeOfTheCastellan from "./crusade-of-the-castellan.mp3"
import guardians from "./guardians.mp3"
import landOfAFolkDivided from "./land-of-a-folk-divided.mp3"
import rememberTheWay from "./remember-the-way.mp3"
import streetsOfSantIvo from "./streets-of-santivo.mp3"

let sub = () => null

class AudioControl {
    constructor(audio, music) {
        this.audio = audio
        this.audio.volume = 0
        this.music = music
        this.maxVol = 1
        this.nextAction = null
        this.shuffle = null
    }

    play() {
        this.audio.play()
    }

    pause() {
        this.audio.pause()
    }

    stop() {
        this.audio.removeEventListener("ended", sub)
        return new Promise(resolve => {
            this.fadeOut().then(() => {
                this.audio.pause()
                this.audio.currentTime = 0
                resolve()
            })
        })
    }

    selectSong(song) {
        const loc = song.split(":")
        this.audio.src = this.music[loc[0]][loc[1]]
    }

    shuffleAlbum(album) {
        this.shuffle = album
        this.nextShuffle()
        sub = this.nextShuffle.bind(this)
        this.audio.addEventListener("ended", sub)
    }

    nextShuffle() {
        let track = -1
        while (!this.music?.[this.shuffle]?.[track]) {
            track = Math.floor(Math.random() * this.music?.[this.shuffle]?.length)
        }
        this.audio.src = this.music[this.shuffle][track]
        this.fadeIn()
    }

    setMaxVolume(maxVol) {
        this.maxVol = maxVol
    }

    setVolume(vol) {
        this.audio.volume = vol
    }

    fadeOut() {
        return new Promise(resolve => {
            const fadeAudio = setInterval(() => {
                if (this.audio.volume !== 0 && this.audio.volume - 0.1 >= 0) {
                    this.audio.volume -= 0.1
                } else {
                    clearInterval(fadeAudio)
                    resolve()
                }
            }, 200)
        })
    }

    fadeIn() {
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
            }
        }, 200)
    }
}

export default new AudioControl(new Audio(), {
    theme: [celticWarrior],
    inGame: [
        anInnocentSword,
        crusadeOfTheCastellan,
        guardians,
        landOfAFolkDivided,
        rememberTheWay,
        streetsOfSantIvo
    ]
})
