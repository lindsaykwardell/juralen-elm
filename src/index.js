import "./main.css"
import { Elm } from "./Main.elm"
import * as serviceWorker from "./serviceWorker"

const netlifyIdentity = window.netlifyIdentity

// import audioControl from "./audio/audioControl"

// audioControl.selectSong("theme:0")
// audioControl.fadeIn()

// Initialize Elm application
const app = Elm.Main.init({
    flags: {
        isProd: process.env.NODE_ENV === "production",
        token: ""
    },
    node: document.getElementById("root")
})

// Initialize identity
netlifyIdentity.init()

// Validate existing user
setTimeout(() => {
    try {
        netlifyIdentity.refresh().then(jwt => {
            app.ports.authStatus.send(true)
        })
    } catch (err) {
        app.ports.authStatus.send(false)
    }
}, 2000);

// Inform Elm when auth has taken place
netlifyIdentity.on('login', user => {
    console.log(user)
    netlifyIdentity.close()
    app.ports.authStatus.send(true)
})

// Subscribe to Elm login
app.ports.login.subscribe(() => {
    netlifyIdentity.open()
})

// Subscribe to Elm logout
app.ports.logout.subscribe(() => {
    netlifyIdentity.logout()
    app.ports.authStatus.send(false)
})


// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister()
