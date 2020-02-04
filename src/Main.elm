module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Game
import Game.Core as Core
import Html exposing (div, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing (Decoder, field, string)
import Juralen.Player exposing (NewPlayer)
import Lobby
import Process
import Task
import Url exposing (Url)



---- MODEL ----


type Page
    = Splash
    | Home
    | Login
    | Lobby Lobby.Model
    | Game Core.Model


type alias Model =
    { page : Page
    , newPlayers : List NewPlayer
    }


init : Bool -> ( Model, Cmd Msg )
init isProd =
    ( { page = Splash, newPlayers = [] }, delay 2000.0 (ChangePage (Lobby (Tuple.first Lobby.init))) )



---- FUNCTIONS ----



delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)



---- UPDATE ----


type Msg
    = ChangePage Page
    | GotLobbyMsg Lobby.Msg
    | GotGameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePage page ->
            case page of
                Game game ->
                    ( { model | page = Game (Tuple.first (Game.init model.newPlayers)) }, Cmd.map GotGameMsg (Tuple.second (Game.init model.newPlayers)) )

                Lobby lobby ->
                    ( { model | page = Lobby lobby }, Cmd.map GotLobbyMsg (Tuple.second Lobby.init) )

                _ ->
                    ( model, Cmd.none )

        GotLobbyMsg lobbyMsg ->
            case model.page of
                Lobby lobbyModel ->
                    case lobbyMsg of
                        Lobby.StartGame ->
                            update (ChangePage (Game (Tuple.first (Game.init lobbyModel.newPlayerList)))) { model | newPlayers = lobbyModel.newPlayerList }

                        _ ->
                            toLobby model (Lobby.update lobbyMsg lobbyModel)

                _ ->
                    ( model, Cmd.none )

        GotGameMsg gameMsg ->
            case model.page of
                Game gameModel ->
                    toGame model (Game.update gameMsg gameModel)

                _ ->
                    ( model, Cmd.none )


toLobby : Model -> ( Lobby.Model, Cmd Lobby.Msg ) -> ( Model, Cmd Msg )
toLobby model ( lobby, cmd ) =
    ( { model | page = Lobby lobby }, Cmd.map GotLobbyMsg cmd )


toGame : Model -> ( Core.Model, Cmd Game.Msg ) -> ( Model, Cmd Msg )
toGame model ( game, cmd ) =
    ( { model | page = Game game }, Cmd.map GotGameMsg cmd )



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "Juralen"
    , body =
        [ case model.page of
            Splash ->
                div [ class "splash" ] [ Html.h1 [] [ text "JURALEN" ] ]

            Lobby lobby ->
                Lobby.view lobby |> Html.map GotLobbyMsg

            Game game ->
                Game.view game |> Html.map GotGameMsg

            _ ->
                div [] []
        ]
    }



---- PROGRAM ----


main : Program Bool Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }
