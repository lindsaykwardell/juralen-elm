module Game.Scoreboard exposing (..)

import Game.Core exposing (Model, getPlayerScore)
import Game.PlayerColor exposing (isDark, toClass, toTextClass)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)


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
    div [ class "flex w-full m-2" ]
        (List.filterMap
            (\player ->
                let
                    playerScore =
                        getPlayerScore model player.id

                    scorePercentage =
                        100 * toFloat playerScore / toFloat totalScore

                    hoverColor =
                        if isDark player.color then
                            "hover:text-white "

                        else
                            "hover:text-black "
                in
                if playerScore <= 0 then
                    Nothing

                else
                    Just
                        (div
                            [ class ("h-2 p-1 hover:h-auto truncate " ++ hoverColor ++ (player.color |> toClass) ++ " " ++ (player.color |> toTextClass))
                            , style "width" (String.fromFloat scorePercentage ++ "%")
                            ]
                            [ text (player.name ++ ": " ++ String.fromInt playerScore) ]
                        )
            )
            model.players
        )
