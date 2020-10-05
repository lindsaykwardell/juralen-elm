import firebase from "firebase/app"
import "firebase/auth"

const config = {
    apiKey: process.env.ELM_APP_FIREBASE_API_KEY,
    authDomain: process.env.ELM_APP_FIREBASE_AUTH_DOMAIN,
    databaseURL: process.env.ELM_APP_FIREBASE_DB_URL,
    projectId: process.env.ELM_APP_FIREBASE_PROJECT_ID,
    storageBucket: process.env.ELM_APP_FIREBASE_STORAGE_BUCKET,
    messagingSenderId: process.env.ELM_APP_FIREBASE_MSGR_SENDER_ID,
    appId: process.env.ELM_APP_FIREBASE_APP_ID,
}

firebase.initializeApp(config)

firebase
    .firestore()
    .enablePersistence()
    .catch(function (err) {
        if (err.code === "failed-precondition") {
            // Multiple tabs open, persistence can only be enabled
            // in one tab at a a time.
            // ...
        } else if (err.code === "unimplemented") {
            // The current browser does not support all of the
            // features required to enable persistence
            // ...
        }
    })

export default firebase
