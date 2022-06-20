module Game.Resources exposing (Resources, decoder, encoder, spend, useActions)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode


type alias Resources =
    { actions : Float
    , gold : Int
    }


decoder : Decoder Resources
decoder =
    Decode.succeed Resources
        |> Decode.required "actions" Decode.float
        |> Decode.required "gold" Decode.int


encoder : Resources -> Encode.Value
encoder resources =
    Encode.object
        [ ( "actions", Encode.float resources.actions )
        , ( "gold", Encode.int resources.gold )
        ]


spend : Resources -> Int -> Resources
spend resources cost =
    { resources | gold = resources.gold - cost }


useActions : Resources -> Float -> Resources
useActions resources cost =
    { resources | actions = resources.actions - cost }
