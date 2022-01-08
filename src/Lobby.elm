module Lobby exposing (..)

import Game.AnalyzerMode exposing (AnalyzerMode)
import Game.Player exposing (NewPlayer)
import Game.PlayerColor exposing (PlayerColor)
import Game.Scenario exposing (ScenarioType(..))
import Html exposing (Html, button, div, h1, input, label, option, select, text)
import Html.Attributes exposing (checked, class, placeholder, selected, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra as List
import Random


type alias Model =
    { newPlayerList : List NewPlayer
    , nextId : Int
    , aiSpeed : Maybe Float
    , scenarioType : ScenarioType
    , maxX : Maybe Int
    , maxY : Maybe Int
    }


type Msg
    = UpdateName Int String
    | UpdateHumanity Int Bool
    | UpdateColor Int String
    | UpdateAnalyzer Int String
    | UpdateAiSpeed String
    | UpdateMaxX String
    | UpdateMaxY String
    | UpdateScenario ScenarioType
    | AddPlayer
    | AddPlayerName Int Int
    | RemovePlayer Int


nameList : List String
nameList =
    [ "Ilthanen Juralen"
    , "Lord Telinstrom"
    , "Archmage Velsyph"
    , "Uinen"
    , "Dakh"
    , "Drelk'ar"
    , "Tevin"
    , "Vanaan"
    , "Lord Laisonen"
    , "Lord Nerison"
    , "Lord Sielvern"
    , "Heliel"
    , "Veladion"
    , "Lady Elisten"
    ]


randomDefinedMax : Int -> Random.Generator Int
randomDefinedMax max =
    Random.int 0 max


init : ( Model, Cmd Msg )
init =
    ( { newPlayerList =
            [ { id = 1
              , name = "Player 1"
              , isHuman = True
              , color = Game.PlayerColor.Red
              , analyzer = Game.AnalyzerMode.Default
              }
            , { id = 2
              , name = "Player 2"
              , isHuman = False
              , color = Game.PlayerColor.Blue
              , analyzer = Game.AnalyzerMode.Default
              }
            ]
      , nextId = 3
      , aiSpeed = Just 250
      , scenarioType = Conquest
      , maxX = Just 9
      , maxY = Just 9
      }
    , Cmd.none
    )


firstAvailableColor : List PlayerColor -> List PlayerColor -> PlayerColor
firstAvailableColor colorList selectedColors =
    case colorList of
        color :: remainingColors ->
            if List.foldl (\selectedColor found -> found || color == selectedColor) False selectedColors then
                firstAvailableColor remainingColors selectedColors

            else
                color

        _ ->
            Game.PlayerColor.None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName id name ->
            ( { model
                | newPlayerList =
                    List.map
                        (\newPlayer ->
                            if newPlayer.id == id then
                                { newPlayer | name = name }

                            else
                                newPlayer
                        )
                        model.newPlayerList
              }
            , Cmd.none
            )

        UpdateHumanity id isHuman ->
            ( { model
                | newPlayerList =
                    List.map
                        (\newPlayer ->
                            if newPlayer.id == id then
                                { newPlayer | isHuman = isHuman }

                            else
                                newPlayer
                        )
                        model.newPlayerList
              }
            , Cmd.none
            )

        UpdateColor id color ->
            ( { model
                | newPlayerList =
                    List.map
                        (\newPlayer ->
                            if newPlayer.id == id then
                                { newPlayer | color = Game.PlayerColor.fromString color }

                            else
                                newPlayer
                        )
                        model.newPlayerList
              }
            , Cmd.none
            )

        UpdateAnalyzer id analyzerName ->
            let
                analyzer : AnalyzerMode
                analyzer =
                    case analyzerName of
                        "DEFAULT" ->
                            Game.AnalyzerMode.Default

                        "AGGRESSIVE" ->
                            Game.AnalyzerMode.Aggressive

                        "DEFENSIVE" ->
                            Game.AnalyzerMode.Defensive

                        "PASSIVE" ->
                            Game.AnalyzerMode.Passive

                        "EXPANSIONIST" ->
                            Game.AnalyzerMode.Expansionist

                        _ ->
                            Game.AnalyzerMode.Default

                newPlayerList : List NewPlayer
                newPlayerList =
                    List.map
                        (\player ->
                            if player.id == id then
                                { player | analyzer = analyzer }

                            else
                                player
                        )
                        model.newPlayerList
            in
            ( { model | newPlayerList = newPlayerList }, Cmd.none )

        UpdateAiSpeed speed ->
            ( { model
                | aiSpeed = String.toFloat speed
              }
            , Cmd.none
            )

        UpdateMaxX maxX ->
            ( { model
                | maxX = String.toInt maxX
              }
            , Cmd.none
            )

        UpdateMaxY maxY ->
            ( { model
                | maxY = String.toInt maxY
              }
            , Cmd.none
            )

        AddPlayer ->
            if List.length model.newPlayerList < 8 then
                let
                    nextId =
                        model.nextId + 1

                    color : PlayerColor
                    color =
                        firstAvailableColor Game.PlayerColor.toList (List.map (\player -> player.color) model.newPlayerList)

                    newPlayer =
                        { id = model.nextId, name = "Player " ++ String.fromInt model.nextId, isHuman = False, color = color, analyzer = Game.AnalyzerMode.Default }

                    newPlayerList =
                        model.newPlayerList ++ [ newPlayer ]
                in
                ( { model | newPlayerList = newPlayerList, nextId = nextId }, Random.generate (AddPlayerName newPlayer.id) (randomDefinedMax (List.length nameList - 1)) )

            else
                ( model, Cmd.none )

        AddPlayerName playerId randomNumber ->
            case List.getAt randomNumber nameList of
                Just name ->
                    let
                        nameExists =
                            List.length (List.filter (\player -> player.name == name) model.newPlayerList) > 0

                        analyzerMode =
                            if randomNumber // 2 == 1 then
                                Game.AnalyzerMode.Expansionist

                            else if randomNumber // 3 == 1 then
                                Game.AnalyzerMode.Passive

                            else if randomNumber // 4 == 1 then
                                Game.AnalyzerMode.Defensive

                            else if randomNumber // 5 == 1 then
                                Game.AnalyzerMode.Aggressive

                            else
                                Game.AnalyzerMode.Default
                    in
                    if nameExists then
                        ( model, Random.generate (AddPlayerName playerId) (randomDefinedMax (List.length nameList - 1)) )

                    else
                        ( { model
                            | newPlayerList =
                                List.map
                                    (\player ->
                                        if player.id == playerId then
                                            { player | name = name, analyzer = analyzerMode }

                                        else
                                            player
                                    )
                                    model.newPlayerList
                          }
                        , Cmd.none
                        )

                Nothing ->
                    ( model, Cmd.none )

        RemovePlayer id ->
            let
                newPlayerList =
                    List.filter (\newPlayer -> newPlayer.id /= id) model.newPlayerList
            in
            ( { model | newPlayerList = newPlayerList }, Cmd.none )

        UpdateScenario scenario ->
            -- For campaign scenarios, fetch the config for the given scenario and load it into the lobby settings
            -- Also need to disable manipulation of the player settings somehow
            -- Otherwise, just set the scenario type
            ( { model | scenarioType = scenario }
            , Cmd.none
            )


view : Model -> { startGame : Model -> msg, returnHome : msg, toMsg : Msg -> msg } -> Html msg
view model { startGame, returnHome, toMsg } =
    div [ class "m-auto w-11/12" ]
        [ h1 [ class "text-white" ] [ text "Juralen" ]
        , div []
            [ div [ class "flex justify-end" ]
                [ div [ class "w-1/3 lg:w-1/5" ] [ button [ class "p-3 w-full bg-gray-400 hover:bg-gray-600", onClick AddPlayer ] [ text "Add Player" ] ]
                ]
            , div [ class "bg-gray-700 p-2 shadow" ]
                (List.concat
                    [ model.newPlayerList
                        |> List.map
                            (newPlayerInput
                                (List.map (\player -> player.color) model.newPlayerList)
                            )
                    , [ div [ class "p-3 flex justify-end" ]
                            [ label [ class "text-white" ]
                                [ text "Scenario"
                                , select
                                    [ class "text-black px-2 py-1 mx-3"
                                    , onInput
                                        (\val ->
                                            UpdateScenario <| Game.Scenario.onSelectScenario val
                                        )
                                    ]
                                    (let
                                        ( conquestSelected, scoreSelected, turnCountSelected ) =
                                            case model.scenarioType of
                                                Conquest ->
                                                    ( True, False, False )

                                                ScoreReached _ ->
                                                    ( False, True, False )

                                                NumberOfTurns _ ->
                                                    ( False, False, True )
                                     in
                                     [ option [ value "CONQUEST", selected conquestSelected ] [ text "Conquest" ]
                                     , option [ value "SCORE", selected scoreSelected ] [ text "Score" ]
                                     , option [ value "TURN", selected turnCountSelected ] [ text "Turn Count" ]
                                     ]
                                    )
                                ]
                            , case model.scenarioType of
                                Conquest ->
                                    div [] []

                                ScoreReached score ->
                                    label [ class "text-white" ]
                                        [ text "Max Score"
                                        , input
                                            [ class "w-16 text-black px-2 py-1 mx-3"
                                            , value (String.fromInt score)
                                            , onInput (\val -> UpdateScenario <| ScoreReached <| Maybe.withDefault 0 <| String.toInt val)
                                            ]
                                            []
                                        ]

                                NumberOfTurns turns ->
                                    label [ class "text-white" ]
                                        [ text "Max Score"
                                        , input
                                            [ class "w-16 text-black px-2 py-1 mx-3"
                                            , value (String.fromInt turns)
                                            , onInput (\val -> UpdateScenario <| NumberOfTurns <| Maybe.withDefault 0 <| String.toInt val)
                                            ]
                                            []
                                        ]
                            , label [ class "text-white" ]
                                [ text "Width"
                                , input
                                    [ class "w-16 bg-white mx-3 text-black px-2 py-1"
                                    , type_ "number"
                                    , value
                                        (case model.maxX of
                                            Nothing ->
                                                ""

                                            Just maxX ->
                                                String.fromInt maxX
                                        )
                                    , placeholder "9"
                                    , onInput (\val -> UpdateMaxX val)
                                    ]
                                    []
                                ]
                            , label [ class "text-white" ]
                                [ text "Height"
                                , input
                                    [ class "w-16 bg-white mx-3 text-black px-2 py-1"
                                    , type_ "number"
                                    , value
                                        (case model.maxY of
                                            Nothing ->
                                                ""

                                            Just maxY ->
                                                String.fromInt maxY
                                        )
                                    , placeholder "9"
                                    , onInput (\val -> UpdateMaxY val)
                                    ]
                                    []
                                ]
                            , label [ class "text-white" ]
                                [ text "AI Speed"
                                , input
                                    [ class "w-16 bg-white ml-3 text-black px-2 py-1"
                                    , type_ "number"
                                    , value
                                        (case model.aiSpeed of
                                            Nothing ->
                                                ""

                                            Just aiSpeed ->
                                                String.fromFloat aiSpeed
                                        )
                                    , placeholder "250"
                                    , onInput (\val -> UpdateAiSpeed val)
                                    ]
                                    []
                                ]
                            ]
                      ]
                    ]
                )
            ]
            |> Html.map toMsg
        , div [ class "flex justify-center gap-4" ]
            [ button [ class "bg-green-600 p-2 hover:bg-green-500 transition duration-150 mt-6", onClick (startGame model) ] [ text "Start Game" ]
            , button [ class "bg-blue-600 p-2 hover:bg-blue-500 transition duration-150 mt-6 text-white", onClick returnHome ] [ text "Return to Home" ]
            ]
        ]


newPlayerInput : List PlayerColor -> NewPlayer -> Html Msg
newPlayerInput selectedColors player =
    div [ class "flex flex-col lg:flex-row py-2 items-center" ]
        [ div [ class "flex-shrink mr-3" ]
            [ button [ class "bg-gray-700 border-red-500 border-2 py-1 px-1 m-2 lg:m-0 hover:bg-red-500 text-white transition duration-200", onClick (RemovePlayer player.id) ]
                [ text "Remove Player" ]
            ]
        , div [ class "flex-grow w-full lg:w-auto" ] [ input [ class "p-2 w-full", type_ "text", value player.name, onInput (UpdateName player.id) ] [] ]
        , div [ class "flex-1" ]
            [ label [ class "text-white" ]
                [ text "Is Human"
                , input [ class "ml-3", type_ "checkbox", checked player.isHuman, onClick (UpdateHumanity player.id (not player.isHuman)) ] []
                ]
            ]
        , div [ class "flex-1 flex flex-col lg:flex-row items-center mx-1 w-full lg:w-auto" ]
            [ label [ class "text-white" ] [ text "Analyzer" ]
            , select [ class "p-2 lg:ml-3 w-full lg:w-auto", onInput (UpdateAnalyzer player.id) ]
                [ option [ value "DEFAULT", selected (player.analyzer == Game.AnalyzerMode.Default) ] [ text "Default" ]
                , option [ value "AGGRESSIVE", selected (player.analyzer == Game.AnalyzerMode.Aggressive) ] [ text "Aggressive" ]
                , option [ value "DEFENSIVE", selected (player.analyzer == Game.AnalyzerMode.Defensive) ] [ text "Defensive" ]
                , option [ value "PASSIVE", selected (player.analyzer == Game.AnalyzerMode.Passive) ] [ text "Passive" ]
                , option [ value "EXPANSIONIST", selected (player.analyzer == Game.AnalyzerMode.Expansionist) ] [ text "Expansionist" ]
                ]
            ]
        , div [ class "flex-1 flex flex-col lg:flex-row items-center mx-1 w-full lg:w-auto m-2 lg:m-0" ]
            [ label [ class "text-white " ]
                [ text "Color" ]
            , select
                [ class
                    ("p-2 lg:mx-3 w-32 w-full "
                        ++ Game.PlayerColor.toClass player.color
                        ++ (if Game.PlayerColor.isDark player.color then
                                ""

                            else
                                " text-black"
                           )
                    )
                , onInput (UpdateColor player.id)
                ]
                (List.map
                    (\playerColor ->
                        option
                            [ value (Game.PlayerColor.toString playerColor)
                            , selected (playerColor == player.color)
                            , class (Game.PlayerColor.toClass playerColor)
                            ]
                            [ text (playerColor |> Game.PlayerColor.toString |> String.toUpper) ]
                    )
                    (List.filter
                        (\color ->
                            color
                                == player.color
                                || not (List.foldl (\selectedColor found -> found || selectedColor == color) False selectedColors)
                        )
                        Game.PlayerColor.toList
                    )
                )
            ]
        ]
