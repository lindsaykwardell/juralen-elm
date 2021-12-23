module Components.Modal exposing (view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


view :
    { show : Bool
    , onClose : msg
    , content : Html msg
    }
    -> Html msg
view config =
    if config.show then
        div [ class "fixed flex justify-center items-center h-screen w-screen z-20" ]
            [ div [ class "fixed bg-juralen-transparent z-30 h-screen w-screen", onClick config.onClose ]
                []
            , div [ class "bg-gray-600 z-40 w-11/12 md:w-1/3" ]
                [ config.content
                ]
            ]

    else
        text ""
