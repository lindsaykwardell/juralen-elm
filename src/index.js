import "./main.css"
import { Elm } from "./Main.elm"
import * as serviceWorker from "./serviceWorker"

import { Server, Model, Response } from "miragejs"
import uuid from "uuid/v4"

new Server({
    models: {
        user: Model
    },
    routes() {
        this.get("/test", (schema, request) => {
            return { name: "bob" }
        })

        this.post("/api/login", (schema, request) => {
            let credentials = JSON.parse(request.requestBody)

            const { models: users } = schema.users.all()

            const user = users.find(user => user.name === credentials.username)

            if (!user || user.password !== credentials.password) {
                return new Response(400, {}, { errors: ["Invalid login"] })
            } else {
                const token = uuid()

                return { username: user.name, token }
            }
        })

        this.post("/api/auth", (schema, request) => {
            let credentials = JSON.parse(request.requestBody)
            return { username: "Yagaboosh", token: credentials.token }
        })
    },
    seeds(server) {
        server.create("user", { name: "Yagaboosh", password: "password123" })
    }
})

const app = Elm.Main.init({
    flags: {
        isProd: process.env.NODE_ENV === "production",
        token: localStorage.getItem("token") || ""
    },
    node: document.getElementById("root")
})

app.ports.storeJwt.subscribe(token => {
    localStorage.setItem(`token`, token)
})

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister()
