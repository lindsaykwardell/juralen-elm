module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Game
import Html exposing (div, text)
import Html.Attributes exposing (class)
import Url exposing (Url)
import Process
import Task


---- MODEL ----


type Page
    = Splash
    | Home
    | Lobby
    | Game Game.Model


type alias Model =
    { page : Page }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { page = Splash}, delay 3000.0 (ChangePage (Game (Tuple.first Game.init))) )



---- FUNCTIONS ----


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)



---- UPDATE ----


type Msg
    = ChangePage Page
    | GotGameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePage page ->
            case page of 
                Game game ->
                    ( { page = Game game }, Cmd.map GotGameMsg (Tuple.second Game.init) )

                _ ->
                    ( model, Cmd.none)
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
                div [class "splash"] [Html.h1 [] [text "JURALEN"]]

            _ ->
                div [] []
        ]
    }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = \_ -> Debug.todo "handle URL requests"
        , onUrlChange = \_ -> Debug.todo "handle URL changes"
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }
