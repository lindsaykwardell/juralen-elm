module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Game
import Html exposing (div)
import Url exposing (Url)



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
    ( { page = Game (Tuple.first Game.init) }, Cmd.map GotGameMsg (Tuple.second Game.init) )



---- UPDATE ----


type Msg
    = GotGameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
