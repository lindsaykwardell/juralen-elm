module Game.Option exposing (Option, decoder, encoder)

import Game.Action as Action exposing (Action)
import Game.Loc as Loc exposing (Loc)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode


type alias Option =
    { loc : Loc
    , action : Action
    , score : Int
    }


decoder : Decoder Option
decoder =
    Decode.succeed Option
        |> Decode.required "loc" Loc.decoder
        |> Decode.required "action" Action.decoder
        |> Decode.required "score" Decode.int


encoder : Option -> Encode.Value
encoder option =
    Encode.object
        [ ( "loc", Loc.encoder option.loc )
        , ( "action", Action.encoder option.action )
        , ( "score", Encode.int option.score )
        ]
