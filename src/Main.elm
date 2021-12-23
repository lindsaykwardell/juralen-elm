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
import Game.Player exposing (NewPlayer, revertToNewPlayer)
import Game.Settings as Settings exposing (settingsModal)
import Game.Update as Game
import Html exposing (button, div, hr, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Lobby
import Process
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
    , Cmd.none
    )



---- FUNCTIONS ----


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)



---- UPDATE ----


type Msg
    = Login
    | UpdateAuthStatus Bool
    | InitChangePage Page
    | ChangePage Page
    | ToggleSettings
    | GotSettingsMessage Settings.Msg
    | GotLobbyMsg Lobby.Msg
    | GotGameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login ->
            ( model, login () )

        UpdateAuthStatus currentAuthStatus ->
            if currentAuthStatus then
                update (InitChangePage Home) model

            else if model.page /= Splash then
                update (InitChangePage Splash) model

            else
                ( model, login () )

        InitChangePage page ->
            ( { model | inTransition = True }, delay 1000 (ChangePage page) )

        ChangePage page ->
            case page of
                Splash ->
                    ( { model | inTransition = False, showSettings = False, gameStatus = Core.NoGame, page = Splash }, Cmd.none )

                Home ->
                    ( { model | inTransition = False, showSettings = False, gameStatus = Core.NoGame, page = Home }, Cmd.none )

                Lobby lobby ->
                    ( { model | inTransition = False, showSettings = False, gameStatus = Core.NoGame, page = Lobby lobby }, Cmd.batch [ Cmd.map GotLobbyMsg (Tuple.second Lobby.init), playThemeMusic () ] )

                Game game ->
                    ( { model
                        | inTransition = False
                        , showSettings = False
                        , gameStatus = Core.ActiveGame
                        , page = Game game
                      }
                    , Cmd.batch
                        [ Cmd.map GotGameMsg (Task.succeed Cmd.none |> Task.perform (\_ -> Game.InitializeScenario))
                        , playGameMusic ()
                        ]
                    )

        ToggleSettings ->
            ( { model | showSettings = not model.showSettings }, Cmd.none )

        GotSettingsMessage settingsMsg ->
            case settingsMsg of
                Settings.Logout ->
                    ( model, logout () )

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
                                    , maxX = Just gameModel.scenario.maxX
                                    , maxY = Just gameModel.scenario.maxY
                                    }
                            in
                            update (InitChangePage (Lobby lobbyModel)) model

                        _ ->
                            update (InitChangePage (Lobby (Tuple.first Lobby.init))) model

        GotLobbyMsg lobbyMsg ->
            case model.page of
                Lobby lobbyModel ->
                    case lobbyMsg of
                        Lobby.StartGame ->
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

                        _ ->
                            toLobby model (Lobby.update lobbyMsg lobbyModel)

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
                            toGame model (Game.update gameMsg gameModel)

                _ ->
                    ( model, Cmd.none )


toLobby : Model -> ( Lobby.Model, Cmd Lobby.Msg ) -> ( Model, Cmd Msg )
toLobby model ( lobby, cmd ) =
    ( { model | page = Lobby lobby }, Cmd.map GotLobbyMsg cmd )


toGame : Model -> ( Core.Model, Cmd Game.Msg ) -> ( Model, Cmd Msg )
toGame model ( game, cmd ) =
    ( { model | page = Game game }, Cmd.map GotGameMsg cmd )



---- PORTS ----


port login : () -> Cmd msg


port logout : () -> Cmd msg


port playGameMusic : () -> Cmd msg


port playThemeMusic : () -> Cmd msg


port toggleMute : () -> Cmd msg


port authStatus : (Bool -> msg) -> Sub msg



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ authStatus UpdateAuthStatus
        ]



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
                    |> settingsModal
                    |> Html.map GotSettingsMessage
            }
        , div [ class "flex flex-col h-screen" ]
            [ case model.page of
                Splash ->
                    div [ onClick Login ]
                        [ div [ class "fixed h-screen w-screen flex flex-col justify-center items-center bg-black-75" ] [ hr [] [], Html.h1 [ class "text-white font-stoke my-4" ] [ text "JURALEN" ], hr [] [] ]
                        , div [ class "splash" ] []
                        ]

                Home ->
                    div [ class "flex-grow flex justify-center items-center" ]
                        [ button [ class "border-2 bg-gray-600 p-3 xl:w-1/5 lg:w-1/3 md:w-1/2 sm:w-2/3", onClick (InitChangePage (Lobby (Tuple.first Lobby.init))) ] [ text "Enter Lobby" ]
                        ]

                Lobby lobby ->
                    div [ class "flex-grow" ] [ Lobby.view lobby |> Html.map GotLobbyMsg ]

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
