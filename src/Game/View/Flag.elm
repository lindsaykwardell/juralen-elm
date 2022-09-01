module Game.View.Flag exposing (..)

import Game.PlayerColor
import Html exposing (..)
import Html.Attributes exposing (..)


view : Game.PlayerColor.PlayerColor -> Html msg
view playerColor =
    div [ class "flex flex-col mr-2" ]
        [ div [ class ("triangle " ++ Game.PlayerColor.toString playerColor) ] []
        , div [ class ("triangle " ++ Game.PlayerColor.toString playerColor) ] []
        ]
