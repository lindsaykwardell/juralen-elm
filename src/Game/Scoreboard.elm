module Game.Scoreboard exposing (..)

import Game.Core exposing (Model, getPlayerScore)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Game.PlayerColor exposing (toClass)


view : Model -> Html msg
view model =
    let
        totalScore =
            List.foldl
                (\player score ->
                    score + getPlayerScore model player.id
                )
                0
                model.players
    in
    div [ class "flex w-full h-2 m-2" ]
        (List.map
            (\player ->
                let
                    playerScore =
                        getPlayerScore model player.id

                    scorePercentage =
                        100 * toFloat playerScore / toFloat totalScore
                in
                div [ class (player.color |> toClass), style "width" (String.fromFloat scorePercentage ++ "%") ] []
            )
            model.players
        )
