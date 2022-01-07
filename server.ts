import express from "express"
import { Server } from "socket.io"
const { watch, reactive } = require("vue")

type GameId = string & { __brand: "UUID" }

enum Msg {
    CREATE_GAME = "CREATE_GAME",
    JOIN_GAME = "JOIN_GAME",
    LEAVE_GAME = "LEAVE_GAME",
    GAME_MSG = "GAME_MSG",
}

enum Effect {
    UPDATE_LOBBY = "UPDATE_LOBBY",
}

const app = express()
const port = 3001

const joinableGames: {
    [key: GameId]: number
} = reactive({})

const server = app.listen(port, () => {
    console.log("listening on *:port")
})

const io = new Server(server, {
    cors: {
        origin: "*",
    },
})

io.on("connection", (socket) => {
    let activeGameId: GameId | null = null

    // socket.join("lobby")

    socket.on(Msg.CREATE_GAME, (gameId: GameId) => {
        activeGameId = gameId
        joinableGames[activeGameId] = 1
        socket.emit(Msg.CREATE_GAME, activeGameId)
        socket.join(activeGameId)
        // socket.leave("lobby")
    })

    socket.on(Msg.JOIN_GAME, (uuid: GameId) => {
        if (Object.keys(joinableGames).includes(uuid)) {
            activeGameId = uuid
            joinableGames[uuid]++
            socket.join(uuid)
            // socket.leave("lobby")
        }
    })

    socket.on(Msg.LEAVE_GAME, () => {
        if (activeGameId) {
            joinableGames[activeGameId]--
            socket.leave(activeGameId)
            if (joinableGames[activeGameId] === 0) {
                delete joinableGames[activeGameId]
            }
            activeGameId = null
            // socket.join("lobby")
        }
    })

    socket.on(Msg.GAME_MSG, (msg: string) => {
        if (activeGameId) {
            socket.to(activeGameId).emit(Msg.GAME_MSG, msg)
        }
    })
})

// watch(joinableGames, () => {
//     io.to("lobby").emit(Effect.UPDATE_LOBBY, joinableGames)
// })
