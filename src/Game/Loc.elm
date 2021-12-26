module Game.Loc exposing (Loc, at, coords, decoder, diff, encoder, getDistance, getX, getY)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import List.Extra as List


type Loc
    = Loc Int Int


decoder : Decoder Loc
decoder =
    Decode.string
        |> Decode.andThen
            (\loc ->
                let
                    decodeCoords =
                        String.split "," loc

                    maybeX =
                        List.getAt 0 decodeCoords

                    maybeY =
                        List.getAt 1 decodeCoords
                in
                case ( maybeX, maybeY ) of
                    ( Just strX, Just strY ) ->
                        case ( String.toInt strX, String.toInt strY ) of
                            ( Just x, Just y ) ->
                                Decode.succeed (Loc x y)

                            _ ->
                                Decode.fail "Invalid coordinates"

                    _ ->
                        Decode.fail "Invalid coordinates"
            )


encoder : Loc -> Encode.Value
encoder loc =
    let
        x =
            getX loc

        y =
            getY loc
    in
    Encode.string (String.fromInt x ++ "," ++ String.fromInt y)


at : Int -> Int -> Loc
at x y =
    Loc x y


diff : Loc -> Int -> Int -> Loc
diff (Loc fromX fromY) x y =
    Loc (fromX + x) (fromY + y)


getDistance : Loc -> Loc -> Int
getDistance (Loc fromX fromY) (Loc toX toY) =
    let
        x =
            if (fromX - toX) < 0 then
                (fromX - toX) * -1

            else
                fromX - toX

        y =
            if (fromY - toY) < 0 then
                (fromY - toY) * -1

            else
                fromY - toY
    in
    x + y


coords : Loc -> ( Int, Int )
coords (Loc x y) =
    ( x, y )


getX : Loc -> Int
getX (Loc x _) =
    x


getY : Loc -> Int
getY (Loc _ y) =
    y
