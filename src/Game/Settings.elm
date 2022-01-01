module Game.Settings exposing (..)

import Game.Core exposing (GameStatus)
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias DisplaySettings =
    { gameStatus : GameStatus
    , allowLogout : Bool
    }


type Msg
    = Logout
    | ToggleMute
    | ExitGame
    | ReturnHome


settingsModal : DisplaySettings -> Html Msg
settingsModal settings =
    div []
        [ h1 [ class "text-white" ] [ text "Settings" ]
        , div [ class "flex flex-col p-2" ]
            [ if settings.gameStatus /= Game.Core.NoGame then
                button [ class "bg-blue-300 hover:bg-blue-400 rounded w-full my-1", onClick ExitGame ] [ text "Return to Lobby" ]

              else
                text ""
            , if settings.gameStatus /= Game.Core.NoGame then
                button [ class "bg-blue-300 hover:bg-blue-400 rounded w-full my-1", onClick ReturnHome ] [ text "Return Home" ]

              else
                text ""
            , button [ class "bg-blue-300 hover:bg-blue-400 rounded w-full my-1", onClick ToggleMute ] [ text "Mute/Unmute Audio" ]
            ]
        , div [ class "flex flex-col p-5" ]
            [ div [ class "text-xl text-center text-white" ]
                [ text
                    (if settings.gameStatus == Game.Core.CompletedGame then
                        "Game over!"

                     else
                        ""
                    )
                ]
            ]
        ]
