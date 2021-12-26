module Game.Combat exposing (..)

import Game.Cell exposing (Cell)
import Game.Level exposing (XP)
import Game.Loc exposing (Loc)
import Game.Player exposing (Player)
import Game.Unit exposing (Unit, isDead)
import Game.UnitType
import List.Extra as List
import Process
import Random
import Task



-- Define units in combat
-- Define placement of units in combat
-- 2 rows (like StarCom), but when attacking a structure, there's a wall in the way of attackers
-- It is assumed that if the attackers had been in the city prior to the attack, they were expelled before attacking.
-- No sneak attacks around walls (except maybe if only rogues in the attack)
-- All units can attack from behind a wall, but ranged units (archers and wizards) do not suffer a penalty.
-- Any unit can attack the wall, but wizards and rogues have a bonus
-- Combat damage is determined by a die roll (atk +/- 1 * bonus * penalty)
-- Opponents click on the unit they want to attack.
-- If there are more than two players (active player or cell owner) then they may choose which side to assist.
-- AI will run though the entire combat in its own function before determining a side.
-- Unit order is shuffled to determine priority. Units are placed onto the rows in order, then combat proceeds in that order.


type alias Model =
    { units : List Unit
    , deadUnits : List Unit
    , attacker : Unit
    , defender : Unit
    , attackingPlayer : Player
    , defendingPlayer : Player
    , whoGoesFirst : CombatRole
    , defBonus : Int
    , cell : Cell
    }


type CombatRole
    = Attacker
    | Defender


type StatUpgrade
    = MaxHp
    | Attack
      -- | Range
    | MaxMoves


type Msg
    = DetermineAttacker (List Unit) Int
    | DetermineDefender (List Unit) Int
    | GetRandomUnit CombatRole
    | RunCombat
    | DetermineNextAction { checkLevel : Bool }
    | GetRandomStat Unit
    | UpgradeUnitStat Unit Int
    | ExitCombat


randomDefinedMax : Int -> Random.Generator Int
randomDefinedMax max =
    Random.int 0 max


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetRandomUnit unitRole ->
            let
                units =
                    if unitRole == Attacker then
                        Game.Unit.controlledBy model.units model.attackingPlayer.id

                    else
                        Game.Unit.controlledBy model.units model.defendingPlayer.id

                nextCmd =
                    (if unitRole == Attacker then
                        DetermineAttacker

                     else
                        DetermineDefender
                    )
                        units
            in
            ( model, Random.generate nextCmd (randomDefinedMax (List.length units - 1)) )

        DetermineAttacker units roll ->
            let
                unit : Unit
                unit =
                    case List.getAt roll units of
                        Nothing ->
                            Game.Unit.empty

                        Just thisUnit ->
                            thisUnit
            in
            update (GetRandomUnit Defender) { model | attacker = unit }

        DetermineDefender units roll ->
            let
                unit : Unit
                unit =
                    case List.getAt roll units of
                        Nothing ->
                            Game.Unit.empty

                        Just thisUnit ->
                            thisUnit
            in
            ( { model | defender = unit }
            , if isHumanInvolved model then
                delay 500 RunCombat

              else
                Task.succeed Cmd.none |> Task.perform (\_ -> RunCombat)
            )

        RunCombat ->
            ( if model.whoGoesFirst == Attacker then
                model
                    |> attackerAction
                    |> defenderAction
                    |> updateUnitStatus
                    |> healUnits Attacker
                    |> healUnits Defender
                    |> switchPlayerRoles

              else
                model
                    |> defenderAction
                    |> attackerAction
                    |> updateUnitStatus
                    |> healUnits Attacker
                    |> switchPlayerRoles
            , Task.succeed Cmd.none |> Task.perform (\_ -> DetermineNextAction { checkLevel = True })
            )

        DetermineNextAction { checkLevel } ->
            let
                attackerHasUnits : Bool
                attackerHasUnits =
                    List.length (Game.Unit.controlledBy model.units model.attackingPlayer.id) > 0

                defenderHasUnits : Bool
                defenderHasUnits =
                    List.length (Game.Unit.controlledBy model.units model.defendingPlayer.id) > 0

                unitGainedLevel : Maybe Unit
                unitGainedLevel =
                    case ( isDead model.attacker, isDead model.defender ) of
                        ( True, True ) ->
                            Nothing

                        ( False, True ) ->
                            let
                                unit =
                                    Game.Unit.fromId model.units model.attacker.id
                            in
                            if Game.Level.currentLevel unit.level /= Game.Level.currentLevel model.attacker.level then
                                Just unit

                            else
                                Nothing

                        ( True, False ) ->
                            let
                                unit =
                                    Game.Unit.fromId model.units model.defender.id
                            in
                            if Game.Level.currentLevel unit.level /= Game.Level.currentLevel model.defender.level then
                                Just unit

                            else
                                Nothing

                        ( False, False ) ->
                            Nothing

                nextCombat : Cmd Msg
                nextCombat =
                    if isHumanInvolved model then
                        delay 500 (GetRandomUnit Attacker)

                    else
                        Task.succeed Cmd.none |> Task.perform (\_ -> GetRandomUnit Attacker)

                exitCombat : Cmd Msg
                exitCombat =
                    if isHumanInvolved model then
                        Cmd.none

                    else
                        Task.succeed Cmd.none |> Task.perform (\_ -> ExitCombat)

                levelUpStat : Unit -> Cmd Msg
                levelUpStat unit =
                    Task.succeed Cmd.none |> Task.perform (\_ -> GetRandomStat unit)
            in
            case ( checkLevel, attackerHasUnits, defenderHasUnits ) of
                ( True, True, True ) ->
                    case unitGainedLevel of
                        Nothing ->
                            ( model, nextCombat )

                        Just leveledUpUnit ->
                            ( model, levelUpStat leveledUpUnit )

                ( True, _, _ ) ->
                    case unitGainedLevel of
                        Nothing ->
                            ( model, exitCombat )

                        Just leveledUpUnit ->
                            ( model, levelUpStat leveledUpUnit )

                ( False, True, True ) ->
                    ( model, nextCombat )

                _ ->
                    ( model, exitCombat )

        GetRandomStat unit ->
            ( model, Random.generate (UpgradeUnitStat unit) (randomDefinedMax 10) )

        UpgradeUnitStat unit roll ->
            let
                stat : StatUpgrade
                stat =
                    if roll <= 4 then
                        MaxHp

                    else if roll <= 7 then
                        Attack

                    else
                        -- Range
                        MaxMoves
            in
            ( { model
                | units =
                    List.map
                        (\u ->
                            if u.id == unit.id then
                                case stat of
                                    MaxHp ->
                                        { u | maxHealth = u.maxHealth + 1, health = u.maxHealth + 1 }

                                    Attack ->
                                        { u | attack = u.attack + 1, health = u.maxHealth }

                                    -- Range ->
                                    --     { u | range = u.range + 1, health = u.maxHealth }
                                    MaxMoves ->
                                        { u | maxMoves = u.maxMoves + 1, health = u.maxHealth }

                            else
                                u
                        )
                        model.units
              }
            , Task.succeed Cmd.none |> Task.perform (\_ -> DetermineNextAction { checkLevel = False })
            )

        ExitCombat ->
            ( model, Cmd.none )


attackerAction : Model -> Model
attackerAction model =
    if Game.Unit.isDead model.attacker then
        model

    else if model.defBonus > 0 then
        { model | defBonus = model.defBonus - model.attacker.attack }

    else
        let
            defender =
                Game.Unit.takeDamage model.defender model.attacker.attack
        in
        { model | defender = defender }


defenderAction : Model -> Model
defenderAction model =
    if Game.Unit.isDead model.defender then
        model

    else
        let
            attacker =
                Game.Unit.takeDamage model.attacker model.defender.attack
        in
        { model | attacker = attacker }


healUnits : CombatRole -> Model -> Model
healUnits role model =
    case role of
        Attacker ->
            if model.attacker.unitType == Game.UnitType.Priest then
                let
                    healedUnits : List Unit
                    healedUnits =
                        List.map
                            (\unit ->
                                if unit.controlledBy == model.attackingPlayer.id && unit.id /= model.attacker.id then
                                    let
                                        maxHealth : Int
                                        maxHealth =
                                            unit.maxHealth
                                    in
                                    { unit
                                        | health =
                                            if unit.health + 1 > maxHealth then
                                                unit.health

                                            else
                                                unit.health + 1
                                    }

                                else
                                    unit
                            )
                            model.units
                in
                { model | units = healedUnits }

            else
                model

        Defender ->
            if model.defender.unitType == Game.UnitType.Priest then
                let
                    healedUnits : List Unit
                    healedUnits =
                        List.map
                            (\unit ->
                                if unit.controlledBy == model.defendingPlayer.id && unit.id /= model.defender.id then
                                    let
                                        initialValues : Game.UnitType.InitialValues
                                        initialValues =
                                            Game.UnitType.initialValues unit.unitType

                                        maxHealth : Int
                                        maxHealth =
                                            initialValues.health
                                    in
                                    { unit
                                        | health =
                                            if unit.health + 1 > maxHealth then
                                                unit.health

                                            else
                                                unit.health + 1
                                    }

                                else
                                    unit
                            )
                            model.units
                in
                { model | units = healedUnits }

            else
                model


switchPlayerRoles : Model -> Model
switchPlayerRoles model =
    { model
        | whoGoesFirst =
            if model.whoGoesFirst == Attacker then
                Defender

            else
                Attacker
    }


updateUnitStatus : Model -> Model
updateUnitStatus model =
    let
        postCombat : List Unit
        postCombat =
            applyCombatToUnits model.units model.attacker model.defender

        livingUnits : List Unit
        livingUnits =
            removeDeadUnits postCombat

        deadUnits : List Unit
        deadUnits =
            moveUnitsToGraveyard model.deadUnits postCombat
    in
    { model | units = livingUnits, deadUnits = deadUnits }


applyCombatToUnits : List Unit -> Unit -> Unit -> List Unit
applyCombatToUnits units attacker defender =
    List.map
        (\unit ->
            if unit.id == attacker.id then
                { attacker | level = Game.Level.gainXp (gainDeadUnitXp defender) unit.level }

            else if unit.id == defender.id then
                { defender | level = Game.Level.gainXp (gainDeadUnitXp attacker) unit.level }

            else
                unit
        )
        units


gainDeadUnitXp : Unit -> Maybe XP
gainDeadUnitXp unit =
    if isDead unit then
        Just <| Game.Level.toXp unit.level

    else
        Nothing


removeDeadUnits : List Unit -> List Unit
removeDeadUnits units =
    List.filter (\unit -> not (Game.Unit.isDead unit)) units


moveUnitsToGraveyard : List Unit -> List Unit -> List Unit
moveUnitsToGraveyard deadUnits units =
    deadUnits ++ List.filter (\unit -> Game.Unit.isDead unit) units


delay : Float -> b -> Cmd b
delay int msg =
    Process.sleep int |> Task.perform (\_ -> msg)


isHumanInvolved : Model -> Bool
isHumanInvolved model =
    model.attackingPlayer.isHuman || model.defendingPlayer.isHuman
