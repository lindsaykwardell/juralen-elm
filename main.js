import "./style.css";
import './src/Components'
import app from './src/app'

setTimeout(() => {
  app.ports.authStatus.send(true);
}, 2000)