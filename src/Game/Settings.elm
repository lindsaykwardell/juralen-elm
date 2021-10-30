module Game.Settings exposing (..)

import Game.Core exposing (GameStatus)
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias Settings =
    {}


type alias DisplaySettings =
    { gameStatus : GameStatus
    , allowLogout : Bool
    }


type Msg
    = Logout
    | ToggleMute
    | CloseSettings
    | ExitGame


settingsModal : DisplaySettings -> Html Msg
settingsModal settings =
    div [ class "fixed flex justify-center items-center h-screen w-screen z-20" ]
        [ div [ class "fixed bg-juralen-transparent z-30 h-screen w-screen", onClick CloseSettings ]
            []
        , div [ class "bg-gray-600 z-40 w-11/12 md:w-1/3" ]
            [ h1 [ class "text-white" ] [ text "Settings" ]
            , div [ class "flex flex-col p-2" ]
                [ if settings.gameStatus /= Game.Core.NoGame then
                    button [ class "bg-blue-300 hover:bg-blue-400 rounded w-full my-1", onClick ExitGame ] [ text "Return to Lobby" ]

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
        ]
