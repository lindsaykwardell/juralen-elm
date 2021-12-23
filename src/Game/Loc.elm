module Game.Loc exposing (Loc, at, coords, diff, getDistance, getX, getY)


type Loc
    = Loc Int Int


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
