module Game.Scoreboard exposing (..)

import Game.Core exposing (Model, getPlayerRankings, getPlayerScore)
import Game.History
import Game.Player
import Game.PlayerColor exposing (isDark, toClass, toTextClass)
import Game.PlayerScore exposing (PlayerScore)
import Html exposing (Html, div, span, text)
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
    div [ class "flex w-5/6 m-2 h-8" ]
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


graph : Model -> Html msg
graph model =
    let
        scores =
            List.map .scores model.actionHistory
                |> List.reverse
    in
    div [ class "flex h-full" ]
        (scores
            |> List.map (graphCol model)
        )


graphCol : Model -> List PlayerScore -> Html msg
graphCol model scores =
    div [ class "flex-1 flex flex-col" ]
        (let
            totalScore =
                List.foldl
                    (\playerScore score ->
                        score + playerScore.score
                    )
                    0
                    scores
         in
         scores
            |> List.map
                (\playerScore ->
                    let
                        playerColor =
                            Game.Player.get model.players playerScore.playerId |> .color

                        scorePercentage =
                            100 * toFloat playerScore.score / toFloat totalScore
                    in
                    if playerScore.score <= 0 then
                        div [] []

                    else
                        div
                            [ class ("" ++ (playerColor |> toClass))
                            , style "height" (String.fromFloat scorePercentage ++ "%")
                            ]
                            []
                )
        )


stats : Model -> Html msg
stats model =
    div [ class "flex justify-around gap-8 flex-wrap p-4 w-full" ]
        [ case
            model
                |> getPlayerRankings
                |> List.sortBy (\rank -> rank.score)
                |> List.reverse
                |> List.head
          of
            Just playerScore ->
                statCard
                    { title = "Winner"
                    , content =
                        span []
                            [ playerScore.playerId |> Game.Player.get model.players |> .name |> text
                            , text <| " (" ++ String.fromInt playerScore.score ++ ")"
                            ]
                    , size = Reg
                    }

            _ ->
                text ""
        , statCard
            { title = "Most Aggressive Player"
            , content =
                text
                    (Game.History.mostAggressivePlayer
                        model.players
                        model.actionHistory
                        |> .name
                    )
            , size = Reg
            }
        , statCard
            { title = "Least Aggressive Player"
            , content =
                text
                    (Game.History.leastAggressivePlayer
                        model.players
                        model.actionHistory
                        |> .name
                    )
            , size = Reg
            }
        , statCard
            { title = "Favorite Unit"
            , content = text (Game.History.favoriteUnit model.actionHistory)
            , size = Sm
            }
        , statCard
            { title = "Most Common Research"
            , content = text (Game.History.mostCommonResearch model.actionHistory)
            , size = Sm
            }
        , statCard
            { title = "Units Built"
            , content = text ((String.fromInt <| Game.History.unitsBuilt model.actionHistory) ++ " units")
            , size = Sm
            }
        , statCard
            { title = "Total Attacks"
            , content = text ((String.fromInt <| Game.History.totalCombats model.actionHistory) ++ " attacks")
            , size = Sm
            }
        ]


type StatCardSize
    = Sm
    | Reg


statCard : { size : StatCardSize, title : String, content : Html msg } -> Html msg
statCard { size, title, content } =
    let
        cardClass =
            case size of
                Sm ->
                    "lg:w-1/5 md:w-1/4"

                Reg ->
                    "lg:w-1/4 md:w-1/3"
    in
    div [ class <| cardClass ++ " w-full bg-gray-600 p-3 flex flex-col gap-3" ]
        [ span [ class "text-xl" ] [ text title ]
        , content
        ]
