import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

import { Server, Model, Response } from "miragejs";

new Server({
  models: {
    user: Model
  },
  routes() {
    this.get("/test", (schema, request) => {
      return { name: "bob" };
    });

    this.post("/api/login", (schema, request) => {
      debugger
      let credentials = JSON.parse(request.requestBody)

      const user = schema.users.all().find(user => user.name === credentials.name)

      if (!user || user.password !== credentials.password) {
        return new Response(400, {}, { errors: ["Invalid login"] })
      }
      else {
        return user
      }
    });
  },
  seeds(server) {
    server.create("user", { name: "Yagaboosh", password: "password123" });
  }
});

Elm.Main.init({
  flags: process.env.NODE_ENV === "production",
  node: document.getElementById("root")
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
