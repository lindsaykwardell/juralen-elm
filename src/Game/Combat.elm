module Game.Combat exposing (..)

import Array exposing (Array)
import Juralen.Player exposing (Player)
import Juralen.Unit exposing (Unit)
import Process
import Random
import Task
import Juralen.UnitType



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
    }


type CombatRole
    = Attacker
    | Defender


type Msg
    = DetermineAttacker (List Unit) Int
    | DetermineDefender (List Unit) Int
    | GetRandomUnit CombatRole
    | RunCombat
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
                        Juralen.Unit.controlledBy model.units model.attackingPlayer.id

                    else
                        Juralen.Unit.controlledBy model.units model.defendingPlayer.id

                nextCmd =
                    (if unitRole == Attacker then
                        DetermineAttacker

                     else
                        DetermineDefender
                    )
                        units
            in
            ( model, Random.generate nextCmd (randomDefinedMax (List.length units)) )

        DetermineAttacker units roll ->
            let
                unitArray : Array Unit
                unitArray =
                    Array.fromList units

                unit : Unit
                unit =
                    case Array.get roll unitArray of
                        Nothing ->
                            Juralen.Unit.empty

                        Just thisUnit ->
                            thisUnit
            in
            update (GetRandomUnit Defender) { model | attacker = unit }

        DetermineDefender units roll ->
            let
                unitArray : Array Unit
                unitArray =
                    Array.fromList units

                unit : Unit
                unit =
                    case Array.get roll unitArray of
                        Nothing ->
                            Juralen.Unit.empty

                        Just thisUnit ->
                            thisUnit
            in
            update RunCombat { model | defender = unit }

        RunCombat ->
            let
                newModel : Model
                newModel =
                    if model.whoGoesFirst == Attacker then
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

                attackerHasUnits : Bool
                attackerHasUnits =
                    List.length (Juralen.Unit.controlledBy newModel.units newModel.attackingPlayer.id) > 0

                defenderHasUnits : Bool
                defenderHasUnits =
                    List.length (Juralen.Unit.controlledBy newModel.units newModel.defendingPlayer.id) > 0
            in
            if attackerHasUnits && defenderHasUnits then
                update (GetRandomUnit Attacker) newModel

            else
                ( newModel, Process.sleep 0 |> Task.perform (\_ -> ExitCombat) )

        ExitCombat ->
            ( model, Cmd.none )


attackerAction : Model -> Model
attackerAction model =
    if Juralen.Unit.isDead model.attacker then
        model

    else if model.defBonus > 0 then
        { model | defBonus = model.defBonus - model.attacker.attack }

    else
        let
            defender =
                Juralen.Unit.takeDamage model.defender model.attacker.attack
        in
        { model | defender = defender }


defenderAction : Model -> Model
defenderAction model =
    if Juralen.Unit.isDead model.defender then
        model

    else
        let
            attacker =
                Juralen.Unit.takeDamage model.attacker model.defender.attack
        in
        { model | attacker = attacker }

healUnits : CombatRole -> Model -> Model
healUnits role model =
    case role of
        Attacker ->
            if model.attacker.unitType == Juralen.UnitType.Priest then
                let
                    healedUnits : List Unit
                    healedUnits = List.map (\unit -> 
                        if unit.controlledBy == model.attackingPlayer.id && unit.id /= model.attacker.id then
                            let
                                initialValues : Juralen.UnitType.InitialValues
                                initialValues = Juralen.UnitType.initialValues unit.unitType

                                maxHealth : Int
                                maxHealth = initialValues.health
                            in
                                { unit | health = if unit.health + 1 > maxHealth then unit.health else unit.health + 1 }

                        else unit) model.units
                in
                    { model | units = healedUnits }
            else
                model

        Defender ->
            if model.defender.unitType == Juralen.UnitType.Priest then
                let
                    healedUnits : List Unit
                    healedUnits = List.map (\unit -> 
                        if unit.controlledBy == model.defendingPlayer.id && unit.id /= model.defender.id then
                            let
                                initialValues : Juralen.UnitType.InitialValues
                                initialValues = Juralen.UnitType.initialValues unit.unitType

                                maxHealth : Int
                                maxHealth = initialValues.health
                            in
                                { unit | health = if unit.health + 1 > maxHealth then unit.health else unit.health + 1 }

                        else unit) model.units
                in
                    { model | units = healedUnits }
            else
                model

switchPlayerRoles : Model -> Model
switchPlayerRoles model =
    { model | whoGoesFirst = if model.whoGoesFirst == Attacker then Defender else Attacker }


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
                attacker

            else if unit.id == defender.id then
                defender

            else
                unit
        )
        units


removeDeadUnits : List Unit -> List Unit
removeDeadUnits units =
    List.filter (\unit -> not (Juralen.Unit.isDead unit)) units


moveUnitsToGraveyard : List Unit -> List Unit -> List Unit
moveUnitsToGraveyard deadUnits units =
    deadUnits ++ List.filter (\unit -> Juralen.Unit.isDead unit) units
