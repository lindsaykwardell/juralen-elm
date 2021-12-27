import { Elm } from './Game/Analyzer/Main.elm'

console.log("worker.js loaded");

const analyzer = Elm.Game.Analyzer.Main.init()

console.log(analyzer)

onmessage = function(e) {
  console.log(e.data);
}