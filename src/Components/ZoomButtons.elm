module Components.ZoomButtons exposing (zoomButtons)

import Html exposing (Attribute, Html)
zoomButtons : List (Attribute a) -> List (Html a) -> Html a
zoomButtons =
    Html.node "zoom-buttons"