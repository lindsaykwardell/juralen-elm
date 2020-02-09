import "./main.css"
import { Elm } from "./Main.elm"
import * as serviceWorker from "./serviceWorker"

import firebaseAuth from "./firebase/auth"
import audioControl from "./audio/audioControl"

audioControl.selectSong("theme:0")
audioControl.fadeIn()

// Initialize Elm application
const app = Elm.Main.init({
    flags: {
        isProd: process.env.NODE_ENV === "production",
        token: ""
    },
    node: document.getElementById("root")
})

// Firebase integration
// Set to a timeout to display the splash screen.
setTimeout(() => {
    firebaseAuth(app.ports)
}, 3000)

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister()
