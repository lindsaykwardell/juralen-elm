module Game.Level exposing (Level, XP, at, currentLevel, decoder, encoder, gainXp, toXp)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode


type Level
    = Level Model


type alias Model =
    { level : Int
    , xp : XP
    , toNextLevel : Int
    }


decoder : Decoder Level
decoder =
    Decode.succeed Model
        |> Decode.required "level" Decode.int
        |> Decode.required "xp" (Decode.int |> Decode.andThen (\xp -> Decode.succeed (XP xp)))
        |> Decode.required "toNextLevel" Decode.int
        |> Decode.andThen (\model -> Decode.succeed (Level model))


encoder : Level -> Encode.Value
encoder (Level level) =
    let
        (XP xp) =
            level.xp
    in
    Encode.object
        [ ( "level", Encode.int level.level )
        , ( "xp", Encode.int xp )
        , ( "toNextLevel", Encode.int level.toNextLevel )
        ]


type XP
    = XP Int


toXp : Level -> XP
toXp (Level level) =
    XP level.level


gainXp : Maybe XP -> Level -> Level
gainXp maybeXp (Level level) =
    case maybeXp of
        Nothing ->
            Level level

        Just (XP xp) ->
            let
                (XP prevXpValue) =
                    level.xp

                newXpValue =
                    xp + prevXpValue

                gainedNewLevel =
                    newXpValue >= level.toNextLevel
            in
            if gainedNewLevel then
                Level { level | level = level.level + 1, xp = XP <| newXpValue - level.toNextLevel, toNextLevel = level.toNextLevel * 2 }

            else
                Level { level | xp = XP newXpValue }


currentLevel : Level -> Int
currentLevel (Level level) =
    level.level


at : Int -> Level
at level =
    Level { level = level, xp = XP 0, toNextLevel = level * 2 }
