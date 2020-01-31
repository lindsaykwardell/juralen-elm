module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Game
import Html exposing (div, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing (Decoder, field, string)
import Process
import Task
import Url exposing (Url)



---- MODEL ----


type Page
    = Splash
    | Home
    | Login
    | Lobby
    | Game Game.Model


type alias Model =
    { page : Page }


init : Bool -> Url -> Nav.Key -> ( Model, Cmd Msg )
init isProd url key =
    if isProd then
        ( { page = Splash }, delay 2000.0 (ChangePage (Game (Tuple.first Game.init))) )

    else
        case url.path of
            "/game" ->
                ( { page = Game (Tuple.first Game.init) }, Tuple.second Game.init |> Cmd.map GotGameMsg )

            "/login" ->
                ( { page = Home }, fetchTestData )

            _ ->
                ( { page = Splash }, delay 2000.0 (ChangePage (Game (Tuple.first Game.init))) )



---- FUNCTIONS ----


fetchTestData : Cmd Msg
fetchTestData =
    Http.get
        { url = "/test"
        , expect = Http.expectJson GotTestData (field "name" string)
        }


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)



---- UPDATE ----


type Msg
    = ChangePage Page
    | GotTestData (Result Http.Error String)
    | GotGameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePage page ->
            case page of
                Game game ->
                    ( { page = Game game }, Cmd.map GotGameMsg (Tuple.second Game.init) )

                _ ->
                    ( model, Cmd.none )

        GotTestData result ->
            case result of
                Ok string ->
                    let
                        _ =
                            Debug.log "Success" string
                    in
                    ( model, Cmd.none )

                Err _ ->
                    let
                        _ =
                            Debug.log "Failure" ":("
                    in
                    ( model, Cmd.none )

        GotGameMsg gameMsg ->
            case model.page of
                Game gameModel ->
                    toGame model (Game.update gameMsg gameModel)

                _ ->
                    ( model, Cmd.none )


toGame : Model -> ( Game.Model, Cmd Game.Msg ) -> ( Model, Cmd Msg )
toGame model ( game, cmd ) =
    ( { model | page = Game game }, Cmd.map GotGameMsg cmd )



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "Juralen"
    , body =
        [ case model.page of
            Game game ->
                Game.view game |> Html.map GotGameMsg

            Splash ->
                div [ class "splash" ] [ Html.h1 [] [ text "JURALEN" ] ]

            _ ->
                div [] []
        ]
    }



---- PROGRAM ----


main : Program Bool Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = \_ -> Debug.todo "handle URL requests"
        , onUrlChange = \_ -> Debug.todo "handle URL changes"
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }
