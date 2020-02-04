port module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Game
import Game.Core as Core
import Html exposing (Attribute, button, div, form, h2, input, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit, preventDefaultOn)
import Http
import Json.Decode as Decode exposing (Decoder, field, string)
import Json.Encode as Encode
import Juralen.Player exposing (NewPlayer)
import Lobby
import Process
import Task
import Url exposing (Url)



---- MODEL ----


type alias Init =
    { isProd : Bool
    , token : String
    }


type Page
    = Splash
    | Login
    | Home
    | Lobby Lobby.Model
    | Game Core.Model


type alias Model =
    { page : Page
    , username : String
    , password : String
    , token : String
    , newPlayers : List NewPlayer
    }


type alias LoginPayload =
    { token : String
    , username : String
    }


defaultModel : Model
defaultModel =
    { page = Splash
    , username = ""
    , password = ""
    , token = ""
    , newPlayers = []
    }


init : Init -> ( Model, Cmd Msg )
init payload =
    if payload.isProd then
        ( { defaultModel | token = payload.token }, delay 2000.0 (ChangePage (Lobby (Tuple.first Lobby.init))) )

    else
        -- ( { defaultModel | page = Login, token = payload.token }, Cmd.none )
        ( { defaultModel | token = payload.token }, delay 0 (ChangePage Login) )



---- FUNCTIONS ----


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


preventDefaultOnSubmit : msg -> Attribute msg
preventDefaultOnSubmit msg =
    preventDefaultOn "submit" (Decode.succeed ( msg, True ))


decodeLoginPayload : Decoder LoginPayload
decodeLoginPayload =
    Decode.map2 LoginPayload
        (field "token" string)
        (field "username" string)



---- UPDATE ----


type Msg
    = EnterUsername String
    | EnterPassword String
    | AttemptLogin
    | LoginResult (Result Http.Error LoginPayload)
    | ChangePage Page
    | GotLobbyMsg Lobby.Msg
    | GotGameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnterUsername username ->
            ( { model | username = username }, Cmd.none )

        EnterPassword password ->
            ( { model | password = password }, Cmd.none )

        AttemptLogin ->
            ( { model | password = "" }
            , Http.post
                { url = "/api/login"
                , body =
                    Http.jsonBody
                        (Encode.object
                            [ ( "username", Encode.string model.username )
                            , ( "password", Encode.string model.password )
                            ]
                        )
                , expect = Http.expectJson LoginResult decodeLoginPayload
                }
            )

        LoginResult result ->
            case result of
                Err err ->
                    let
                        _ =
                            Debug.log "Error!" err
                    in
                    ( model, Cmd.none )

                Ok loginPayload ->
                    let
                        _ =
                            Debug.log "Success!" loginPayload
                    in
                    update (ChangePage Home) { model | username = loginPayload.username, token = loginPayload.token }

        ChangePage page ->
            case page of
                Login ->
                    if String.length model.token == 0 then
                        ( { model | page = Login }, Cmd.none )

                    else
                        -- Validate token
                        ( { model | page = Login }
                        , Http.post
                            { url = "/api/auth"
                            , body =
                                Http.jsonBody
                                    (Encode.object
                                        [ ( "token", Encode.string model.token ) ]
                                    )
                            , expect = Http.expectJson LoginResult decodeLoginPayload
                            }
                        )

                Home ->
                    ( { model | page = Home }, storeJwt model.token )

                Lobby lobby ->
                    ( { model | page = Lobby lobby }, Cmd.map GotLobbyMsg (Tuple.second Lobby.init) )

                Game _ ->
                    ( { model | page = Game (Tuple.first (Game.init model.newPlayers)) }, Cmd.map GotGameMsg (Tuple.second (Game.init model.newPlayers)) )

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



---- PORTS ----


port storeJwt : String -> Cmd msg



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "Juralen"
    , body =
        [ case model.page of
            Splash ->
                div [ class "splash" ] [ Html.h1 [ class "text-white" ] [ text "JURALEN" ] ]

            Login ->
                div [ class "login" ]
                    [ div [ class "border-2 bg-gray-600 p-3 xl:w-1/5 lg:w-1/3 md:w-1/2 sm:w-2/3" ]
                        [ h2 [ class "m-3" ] [ text "Log In" ]
                        , form [ preventDefaultOnSubmit AttemptLogin ]
                            [ input [ class "rounded my-3 w-full p-2", type_ "text", placeholder "Username", onInput EnterUsername ] []
                            , input [ class "rounded my-3 w-full p-2", type_ "password", placeholder "Password", onInput EnterPassword ] []
                            , input [ class "px-3 py-2 bg-blue-300 hover:bg-blue-500 pointer rounded", type_ "submit" ] [ text "Log In" ]
                            ]
                        ]
                    ]

            Home ->
                div [ class "login" ]
                    [ div [ class "border-2 bg-gray-600 p-3 xl:w-1/5 lg:w-1/3 md:w-1/2 sm:w-2/3" ]
                        [ button [ onClick (ChangePage (Lobby (Tuple.first Lobby.init))) ] [ text "Enter Lobby" ]
                        ]
                    ]

            Lobby lobby ->
                Lobby.view lobby |> Html.map GotLobbyMsg

            Game game ->
                Game.view game |> Html.map GotGameMsg
        ]
    }



---- PROGRAM ----


main : Program Init Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }
