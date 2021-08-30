import "./style.css";
import { Elm } from "./src/Main.elm";

const root = document.querySelector("#app div");
const app = Elm.Main.init({ 
  node: root,
  flags: {
    isProd: import.meta.env.PROD
  }
});

setTimeout(() => {
  app.ports.authStatus.send(true);
}, 2000)