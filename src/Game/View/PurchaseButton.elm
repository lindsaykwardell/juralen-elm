module Game.View.PurchaseButton exposing (blue, green, yellow)

import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (class, src, title)
import Html.Events exposing (onClick)


type alias Config msg =
    { icon : String
    , cost : Int
    , description : String
    , onClick : msg
    }


greenClasses : String
greenClasses =
    "border-green-400 bg-green-800 hover:bg-green-200"


blueClasses : String
blueClasses =
    "border-blue-400 bg-blue-800 hover:bg-blue-200"


yellowClasses : String
yellowClasses =
    "border-yellow-400 bg-yellow-800 hover:bg-yellow-200"


green : Config msg -> Html msg
green config =
    view config greenClasses


blue : Config msg -> Html msg
blue config =
    view config blueClasses


yellow : Config msg -> Html msg
yellow config =
    view config yellowClasses


view : Config msg -> String -> Html msg
view config color =
    button
        [ class <| "purchase-button relative border py-2 px-3 m-2 w-16 h-16 " ++ color
        , onClick config.onClick
        ]
        [ div [ class "absolute bottom-[-6px] right-[-6px] w-6 h-6 rounded-full bg-amber-400 shadow-md border-2 border-yellow-600 flex justify-center items-center" ]
            [ text <| String.fromInt config.cost ]
        , img [ src config.icon, title config.description, class "aspect-square" ] []
        , span [ class "tooltip absolute top-[120%] right-[-50%] md:right-0 w-[125px] bg-black text-white z-[2] p-1 shadow-lg border border-gray-600 text-sm" ]
            [ text config.description
            , div [ class "tooltip-arrow" ] []
            ]
        ]
