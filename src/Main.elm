port module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import FontAwesome.Svg as SvgIcon
import FontAwesome.Transforms as Icon
import Game
import Game.Core as Core
import Game.Settings as Settings exposing (Settings, settingsModal)
import Html exposing (Attribute, button, div, form, h2, input, text, hr)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit, preventDefaultOn)
import Http
import Json.Decode as Decode exposing (Decoder, field, string)
import Json.Encode as Encode
import Juralen.Player exposing (NewPlayer, revertToNewPlayer)
import Lobby
import Process
import Task
import Url exposing (Url)



---- MODEL ----


type alias Init =
    { isProd : Bool
    }


type Page
    = Splash
    | Login
    | Home
    | Lobby Lobby.Model
    | Game Core.Model


type alias Model =
    { page : Page
    , isGameActive : Bool
    , inTransition : Bool
    , showSettings : Bool
    , settings : Settings
    , email : String
    , password : String
    , newPlayers : List NewPlayer
    }


type alias LoginPayload =
    { email : String
    , password : String
    }


defaultModel : Model
defaultModel =
    { page = Splash
    , isGameActive = False
    , inTransition = False
    , showSettings = False
    , settings = {}
    , email = ""
    , password = ""
    , newPlayers = []
    }


init : Init -> ( Model, Cmd Msg )
init payload =
    ( defaultModel, Cmd.none )



---- FUNCTIONS ----


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


preventDefaultOnSubmit : msg -> Attribute msg
preventDefaultOnSubmit msg =
    preventDefaultOn "submit" (Decode.succeed ( msg, True ))



---- UPDATE ----


type Msg
    = EnterEmail String
    | EnterPassword String
    | AttemptLogin
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
        EnterEmail email ->
            ( { model | email = email }, Cmd.none )

        EnterPassword password ->
            ( { model | password = password }, Cmd.none )

        AttemptLogin ->
            ( { model | password = "" }
            , login { email = model.email, password = model.password }
            )

        UpdateAuthStatus currentAuthStatus ->
            if currentAuthStatus then
                update (InitChangePage Home) model

            else
                update (InitChangePage Login) model

        InitChangePage page ->
            ( { model | inTransition = True }, delay 2000 (ChangePage page) )

        ChangePage page ->
            case page of
                Login ->
                    ( { model | inTransition = False, showSettings = False, isGameActive = False, page = Login }, Cmd.none )

                Home ->
                    ( { model | inTransition = False, showSettings = False, isGameActive = False, page = Home }, Cmd.none )

                Lobby lobby ->
                    ( { model | inTransition = False, showSettings = False, isGameActive = False, page = Lobby lobby }, Cmd.map GotLobbyMsg (Tuple.second Lobby.init) )

                Game _ ->
                    ( { model | inTransition = False, showSettings = False, isGameActive = True, page = Game (Tuple.first (Game.init model.newPlayers)) }, Cmd.map GotGameMsg (Tuple.second (Game.init model.newPlayers)) )

                _ ->
                    ( { model | inTransition = False }, Cmd.none )

        ToggleSettings ->
            ( { model | showSettings = not model.showSettings }, Cmd.none )

        GotSettingsMessage settingsMsg ->
            case settingsMsg of
                Settings.Logout ->
                    ( model, logout () )

                Settings.ExitGame ->
                    case model.page of
                        Game gameModel ->
                            let 
                            
                                lobbyModel : Lobby.Model
                                lobbyModel = 
                                    { newPlayerList = List.map revertToNewPlayer gameModel.players
                                    , nextId = List.foldl (\player id ->
                                            if player.id > id then player.id else id
                                        )
                                        1
                                        gameModel.players
                                    }
                            
                            in
                                update (InitChangePage (Lobby lobbyModel)) model    

                        _ ->
                            update (InitChangePage (Lobby (Tuple.first Lobby.init))) model

                Settings.CloseSettings ->
                    update ToggleSettings model

        GotLobbyMsg lobbyMsg ->
            case model.page of
                Lobby lobbyModel ->
                    case lobbyMsg of
                        Lobby.StartGame ->
                            update (InitChangePage (Game (Tuple.first (Game.init lobbyModel.newPlayerList)))) { model | newPlayers = lobbyModel.newPlayerList }

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



---- PORTS ----


port login : LoginPayload -> Cmd msg


port logout : () -> Cmd msg


port authStatus : (Bool -> msg) -> Sub msg



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
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
                ("fixed h-screen w-screen pointer-events-none z-20 bg-juralen transition-opacity ease-in-out duration-150"
                    ++ (if not model.inTransition then
                            " opacity-0"

                        else
                            ""
                       )
                )
            ]
            []
        , if model.showSettings then
            { isGameActive = model.isGameActive, allowLogout = True } |> settingsModal |> Html.map GotSettingsMessage

          else
            div [] []
        , div []
            [ if model.page == Splash || model.page == Login then
                div [] []

              else
                div [ class "sticky flex bg-gray-700 mb-3 z-10" ]
                    [ div [ class "flex-grow text-right text-xl" ] [ button [ class "p-2 hover:bg-gray-600 text-white", onClick ToggleSettings ] [ Html.span [ class "mr-3" ] [ text "Settings" ], Icon.viewIcon Icon.cog ] ] ]
            , case model.page of
                Splash ->
                    div []
                        [ div [ class "fixed h-screen w-screen flex flex-col justify-center items-center bg-black-75" ] [ hr [] [], Html.h1 [ class "text-white font-stoke my-4" ] [ text "JURALEN" ], hr [] [] ]
                        , div [ class "splash" ] []
                        ]

                Login ->
                    div [ class "login" ]
                        [ div [ class "border-2 bg-gray-600 p-3 xl:w-1/5 lg:w-1/3 md:w-1/2 sm:w-2/3" ]
                            [ h2 [ class "m-3" ] [ text "Log In" ]
                            , form [ preventDefaultOnSubmit AttemptLogin ]
                                [ input [ class "rounded my-3 w-full p-2", type_ "text", placeholder "Email Address", onInput EnterEmail, value model.email ] []
                                , input [ class "rounded my-3 w-full p-2", type_ "password", placeholder "Password", onInput EnterPassword, value model.password ] []
                                , div [ class "mx-6" ]
                                    [ input [ class "px-3 py-2 bg-blue-300 hover:bg-blue-500 pointer rounded w-full", type_ "submit", value "Login" ] []
                                    ]
                                ]
                            ]
                        ]

                Home ->
                    div [ class "login" ]
                        [ button [ class "border-2 bg-gray-600 p-3 xl:w-1/5 lg:w-1/3 md:w-1/2 sm:w-2/3", onClick (InitChangePage (Lobby (Tuple.first Lobby.init))) ] [ text "Enter Lobby" ]
                        ]

                Lobby lobby ->
                    Lobby.view lobby |> Html.map GotLobbyMsg

                Game game ->
                    Game.view game |> Html.map GotGameMsg
            ]
        ]
    }



---- PROGRAM ----


main : Program Init Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
