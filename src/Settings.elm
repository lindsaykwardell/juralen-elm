module Settings exposing (DisplaySettings, Msg(..), view)

import Game.Core exposing (GameStatus)
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias DisplaySettings =
    { gameStatus : GameStatus
    , allowLogout : Bool
    }


type Msg
    = ToggleMute
    | ExitGame
    | ReturnHome


view : DisplaySettings -> Html Msg
view settings =
    div []
        [ h1 [ class "text-white" ] [ text "Settings" ]
        , div [ class "flex flex-col gap-2 p-2" ]
            [ if settings.gameStatus /= Game.Core.NoGame then
                button [ class "bg-blue-300 hover:bg-blue-400 w-full", onClick ExitGame ] [ text "Return to Lobby" ]

              else
                text ""
            , if settings.gameStatus /= Game.Core.NoGame then
                button [ class "bg-blue-300 hover:bg-blue-400 w-full", onClick ReturnHome ] [ text "Return Home" ]

              else
                text ""
            , button [ class "bg-blue-300 hover:bg-blue-400 w-full", onClick ToggleMute ] [ text "Mute/Unmute Audio" ]
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
