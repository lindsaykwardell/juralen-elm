module Game.Settings exposing (..)

import Game.Core exposing (GameStatus)
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Juralen.Player exposing (Player)
import Juralen.PlayerColor


type alias Settings =
    {}


type alias DisplaySettings =
    { gameStatus : GameStatus
    , allowLogout : Bool
    , playerRanking : List Player
    }


type Msg
    = Logout
    | CloseSettings
    | ExitGame


settingsModal : DisplaySettings -> Html Msg
settingsModal settings =
    div [ class "fixed flex justify-center items-center h-screen w-screen" ]
        [ div [ class "fixed bg-juralen-transparent z-30 h-screen w-screen", onClick CloseSettings ]
            []
        , div [ class "bg-gray-600 z-40 w-1/3" ]
            [ h1 [ class "text-white" ] [ text "Settings" ]
            , div [ class "flex flex-col p-2" ]
                [ if settings.gameStatus /= Game.Core.NoGame then
                    button [ class "bg-blue-300 hover:bg-blue-400 rounded w-full my-1", onClick ExitGame ] [ text "Return to Lobby" ]

                  else
                    text ""
                ]
            , div [ class "flex flex-col p-5" ]
                (List.concat
                    [ [ div [ class "text-xl text-center" ]
                            [ text
                                (if settings.gameStatus == Game.Core.CompletedGame then
                                    "Game over!"

                                 else
                                    ""
                                )
                            ]
                      ]
                    , List.map
                        (\player ->
                            div
                                [ class
                                    ("py-1 "
                                        ++ Juralen.PlayerColor.toClass player.color
                                        ++ (if Juralen.PlayerColor.isDark player.color then
                                                " text-white"

                                            else
                                                " text-black"
                                           )
                                    )
                                ]
                                [ text (player.name ++ " - " ++ String.fromInt player.score) ]
                        )
                        settings.playerRanking
                    ]
                )
            ]
        ]
