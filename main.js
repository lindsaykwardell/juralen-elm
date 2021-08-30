import "./style.css";
import { Elm } from "./src/Main.elm";

const root = document.querySelector("#app div");
const app = Elm.Main.init({ 
  node: root
});

setTimeout(() => {
  app.ports.authStatus.send(true);
}, 2000)