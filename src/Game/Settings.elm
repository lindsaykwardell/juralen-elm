module Game.Settings exposing (..)

import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias Settings =
    {}

type alias DisplaySettings = 
    { isGameActive : Bool
    , allowLogout : Bool
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
        , div [ class "bg-gray-600 z-40 w-1/3 h-64" ]
            [ h1 [ class "text-white" ] [ text "Settings" ]
            , div [ class "flex flex-col p-2" ]
                [ if settings.isGameActive then button [ class "bg-blue-300 hover:bg-blue-400 rounded w-full my-1", onClick ExitGame ] [ text "Return to Lobby" ] else text ""
                , if settings.allowLogout then button [ class "bg-blue-300 hover:bg-blue-400 rounded w-full my-1", onClick Logout ] [ text "Log Out" ] else text ""
                ]
            ]
        ]
