port module Main exposing (main)

import Browser exposing (Document)
import Components.Modal as Modal
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Icon as Icon
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import FontAwesome.Transforms as Icon
import Game
import Game.Core as Core
import Game.Option as Analysis
import Game.Player exposing (NewPlayer, revertToNewPlayer)
import Game.Update as Game
import Html exposing (button, div, hr, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List.Extra as List
import Lobby
import Process
import Settings
import Task



---- MODEL ----


type Page
    = Splash
    | Home
    | Lobby Lobby.Model
    | Game Core.Model


type alias Model =
    { page : Page
    , gameStatus : Core.GameStatus
    , inTransition : Bool
    , showSettings : Bool
    , newPlayers : List NewPlayer
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = Splash
      , gameStatus = Core.NoGame
      , inTransition = False
      , showSettings = False
      , newPlayers = []
      }
    , delay 2000 (InitChangePage Home)
    )



---- FUNCTIONS ----


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)



---- UPDATE ----


type Msg
    = InitChangePage Page
    | ChangePage Page
    | StartGame Lobby.Model
    | ReturnHome
    | ToggleSettings
    | GotSettingsMessage Settings.Msg
    | GotLobbyMsg Lobby.Msg
    | GotGameMsg Game.Msg
    | LoadGame
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitChangePage page ->
            ( { model | inTransition = True }, delay 1000 (ChangePage page) )

        ChangePage page ->
            let
                resetModel =
                    { model | inTransition = False, showSettings = False, gameStatus = Core.NoGame }
            in
            case page of
                Splash ->
                    ( { resetModel | page = Splash }, Cmd.none )

                Home ->
                    ( { resetModel | page = Home }, playThemeMusic () )

                Lobby lobby ->
                    ( { resetModel | page = Lobby lobby }, Cmd.batch [ Cmd.map GotLobbyMsg (Tuple.second Lobby.init), playThemeMusic () ] )

                Game game ->
                    ( { resetModel
                        | gameStatus = Core.ActiveGame
                        , page = Game game
                      }
                    , Cmd.batch
                        [ if game.turn <= 0 then
                            Cmd.map GotGameMsg (Task.succeed Cmd.none |> Task.perform (\_ -> Game.InitializeScenario))

                          else
                            case List.getAt game.activePlayer game.players of
                                Nothing ->
                                    Cmd.none

                                Just player ->
                                    if player.isHuman == False then
                                        Cmd.map GotGameMsg (Task.succeed Cmd.none |> Task.perform (\_ -> Game.PerformAiTurn))

                                    else
                                        Cmd.none
                        , playGameMusic ()
                        ]
                    )

        ToggleSettings ->
            ( { model | showSettings = not model.showSettings }, Cmd.none )

        StartGame lobbyModel ->
            update
                (InitChangePage
                    (Game
                        (Tuple.first
                            (Game.init
                                { newPlayerList = lobbyModel.newPlayerList
                                , aiSpeed = Maybe.withDefault 250 lobbyModel.aiSpeed
                                , size = { x = Maybe.withDefault 9 lobbyModel.maxX, y = Maybe.withDefault 9 lobbyModel.maxY }
                                , scenarioType = lobbyModel.scenarioType
                                }
                            )
                        )
                    )
                )
                { model | newPlayers = lobbyModel.newPlayerList }

        ReturnHome ->
            update (InitChangePage Home) model

        GotSettingsMessage settingsMsg ->
            case settingsMsg of
                Settings.ToggleMute ->
                    ( model, toggleMute () )

                Settings.ExitGame ->
                    case model.page of
                        Game gameModel ->
                            let
                                lobbyModel : Lobby.Model
                                lobbyModel =
                                    { newPlayerList = List.map revertToNewPlayer gameModel.players
                                    , nextId =
                                        List.foldl
                                            (\player id ->
                                                if player.id > id then
                                                    player.id

                                                else
                                                    id
                                            )
                                            1
                                            gameModel.players
                                            + 1
                                    , aiSpeed = Just gameModel.aiSpeed
                                    , scenarioType = gameModel.scenario.scenarioType
                                    , maxX = Just <| gameModel.scenario.maxX + 1
                                    , maxY = Just <| gameModel.scenario.maxY + 1
                                    }
                            in
                            update (InitChangePage (Lobby lobbyModel)) model

                        _ ->
                            update (InitChangePage (Lobby (Tuple.first Lobby.init))) model

                Settings.ReturnHome ->
                    update (InitChangePage Home) model

        GotLobbyMsg lobbyMsg ->
            case model.page of
                Lobby lobbyModel ->
                    let
                        ( newModel, cmd ) =
                            Lobby.update lobbyMsg lobbyModel
                    in
                    ( { model | page = Lobby newModel }, Cmd.map GotLobbyMsg cmd )

                _ ->
                    ( model, Cmd.none )

        GotGameMsg gameMsg ->
            case model.page of
                Game gameModel ->
                    case gameMsg of
                        Game.OpenSettings ->
                            update ToggleSettings model

                        Game.EndGame ->
                            update ToggleSettings { model | gameStatus = Core.CompletedGame }

                        _ ->
                            let
                                ( newModel, cmd ) =
                                    Game.update gameMsg gameModel
                            in
                            ( { model | page = Game newModel }, Cmd.map GotGameMsg cmd )

                _ ->
                    ( model, Cmd.none )

        LoadGame ->
            ( model, loadGame () )

        NoOp ->
            ( model, Cmd.none )



---- PORTS ----


port playGameMusic : () -> Cmd msg


port playThemeMusic : () -> Cmd msg


port toggleMute : () -> Cmd msg


port loadGame : () -> Cmd msg


port gameLoaded : (String -> msg) -> Sub msg


port analyzed : (String -> msg) -> Sub msg



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ([ gameLoaded
            (\json ->
                let
                    decoded =
                        Decode.decodeString Core.decoder json
                in
                case decoded of
                    Ok game ->
                        InitChangePage (Game game)

                    _ ->
                        NoOp
            )
         , analyzed
            (\json ->
                if json == "null" then
                    GotGameMsg Game.EndTurn

                else
                    case Decode.decodeString Analysis.decoder json of
                        Ok analysis ->
                            GotGameMsg (Game.PerformAction analysis)

                        Err _ ->
                            GotGameMsg Game.EndTurn
            )
         ]
            ++ (case model.page of
                    Lobby lobby ->
                        [ Sub.map GotLobbyMsg (Lobby.subscriptions lobby) ]

                    _ ->
                        []
               )
        )



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "Juralen"
    , body =
        [ Icon.css
        , div
            [ class
                ("fixed h-screen w-screen pointer-events-none z-50 bg-juralen transition-opacity ease-in-out duration-150"
                    ++ (if not model.inTransition then
                            " opacity-0"

                        else
                            ""
                       )
                )
            ]
            []
        , Modal.view
            { show = model.showSettings
            , onClose = ToggleSettings
            , content =
                { gameStatus = model.gameStatus
                , allowLogout = True
                }
                    |> Settings.view
                    |> Html.map GotSettingsMessage
            }
        , div [ class "flex flex-col h-screen" ]
            [ case model.page of
                Splash ->
                    div []
                        [ div [ class "fixed h-screen w-screen flex flex-col justify-center items-center bg-black-75" ] [ hr [] [], Html.h1 [ class "text-white font-stoke my-4" ] [ text "JURALEN" ], hr [] [] ]
                        , div [ class "splash" ] []
                        ]

                Home ->
                    div [ class "sm:flex-grow flex flex-col justify-center items-center h-screen w-screen" ]
                        [ Html.h1 [ class "text-white font-stoke my-4" ] [ text "JURALEN" ]
                        , div [ class "flex justify-center items-center gap-8 w-full h-[70%] sm:h-[50vh]" ]
                            [ button
                                [ class "border-2 bg-blue-600 hover:bg-blue-700 p-3 w-full xl:w-2/5 md:w-1/3 sm:w-2/3 h-full text-white text-3xl"
                                , onClick (InitChangePage (Lobby (Tuple.first Lobby.init)))
                                ]
                                [ text "New Game" ]
                            , div [ class "h-full w-full xl:w-2/5 md:w-1/3 sm:w-2/3 flex flex-col gap-8" ]
                                [ button
                                    [ class "border-2 border-green-600 hover:bg-green-700 p-3 text-white text-3xl w-full h-[50%]"
                                    , onClick LoadGame
                                    ]
                                    [ text "Load Game" ]
                                , button
                                    [ class "border-2 border-blue-300 hover:bg-blue-400 p-3 text-white text-3xl w-full h-[50%]"
                                    ]
                                    [ text "Join Game" ]
                                , button
                                    [ class "border-2 border-red-600 hover:bg-red-700 p-3 text-white text-3xl w-full"
                                    , onClick ToggleSettings
                                    ]
                                    [ text "Settings" ]
                                ]
                            ]
                        ]

                Lobby lobby ->
                    div [ class "flex-grow" ] [ Lobby.view lobby { startGame = StartGame, returnHome = ReturnHome, toMsg = GotLobbyMsg } ]

                Game game ->
                    div [ class "flex-grow" ]
                        [ Game.view game |> Html.map GotGameMsg
                        ]
            ]
        ]
    }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
