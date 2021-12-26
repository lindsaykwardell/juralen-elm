module Game.Level exposing (Level, XP, at, currentLevel, gainXp, toXp)


type Level
    = Level
        { level : Int
        , xp : XP
        , toNextLevel : Int
        }


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
