module Lobby exposing (..)

import Array
import Html exposing (Html, button, div, h1, input, label, option, select, text)
import Html.Attributes exposing (checked, class, selected, type_, value)
import Html.Events exposing (onClick, onInput)
import Game.AnalyzerMode exposing (AnalyzerMode)
import Game.Player exposing (NewPlayer)
import Game.PlayerColor exposing (PlayerColor)
import Random


type alias Model =
    { newPlayerList : List NewPlayer
    , nextId : Int
    , aiSpeed : Float
    , maxX : Int
    , maxY : Int
    }


type Msg
    = UpdateName Int String
    | UpdateHumanity Int Bool
    | UpdateColor Int String
    | UpdateAnalyzer Int String
    | UpdateAiSpeed String
    | UpdateMaxX String
    | UpdateMaxY String
    | AddPlayer
    | AddPlayerName Int Int
    | RemovePlayer Int
    | StartGame


nameList : Array.Array String
nameList =
    Array.fromList
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
            [ { id = 1, name = "Player 1", isHuman = True, color = Game.PlayerColor.Red, analyzer = Game.AnalyzerMode.Default }
            ]
      , nextId = 2
      , aiSpeed = 250
      , maxX = 8
      , maxY = 8
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
                | aiSpeed =
                    case String.toFloat speed of
                        Just speedFloat ->
                            speedFloat

                        _ ->
                            500
              }
            , Cmd.none
            )

        UpdateMaxX maxX ->
            ( { model
                | maxX =
                    case String.toInt maxX of
                        Just maxXFloat ->
                            maxXFloat - 1

                        _ ->
                            8
              }
            , Cmd.none
            )

        UpdateMaxY maxY ->
            ( { model
                | maxY =
                    case String.toInt maxY of
                        Just maxYFloat ->
                            maxYFloat - 1

                        _ ->
                            8
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
                ( { model | newPlayerList = newPlayerList, nextId = nextId }, Random.generate (AddPlayerName newPlayer.id) (randomDefinedMax (Array.length nameList - 1)) )

            else
                ( model, Cmd.none )

        AddPlayerName playerId randomNumber ->
            case Array.get randomNumber nameList of
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
                        ( model, Random.generate (AddPlayerName playerId) (randomDefinedMax (Array.length nameList - 1)) )

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

        StartGame ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "m-auto w-11/12" ]
        [ h1 [ class "text-white" ] [ text "Juralen" ]
        , div [ class "flex justify-end" ]
            [ div [ class "w-1/3 lg:w-1/5" ] [ button [ class "p-3 w-full bg-gray-400 hover:bg-gray-600 rounded-t", onClick AddPlayer ] [ text "Add Player" ] ]
            ]
        , div [ class "bg-gray-700 p-2 shadow rounded-tl rounded-b" ]
            (List.concat
                [ List.map
                    (newPlayerInput
                        (List.map (\player -> player.color) model.newPlayerList)
                    )
                    model.newPlayerList
                , [ div [ class "p-3 flex justify-end" ]
                        [ label [ class "text-white" ]
                            [ text "Width"
                            , input
                                [ class "w-16 bg-white rounded ml-3 text-black px-2 py-1 mr-3"
                                , type_ "number"
                                , value (String.fromInt (model.maxX + 1))
                                , onInput UpdateMaxX
                                ]
                                []
                            ]
                        , label [ class "text-white" ]
                            [ text "Height"
                            , input
                                [ class "w-16 bg-white rounded ml-3 text-black px-2 py-1 mr-3"
                                , type_ "number"
                                , value (String.fromInt (model.maxY + 1))
                                , onInput UpdateMaxY
                                ]
                                []
                            ]
                        , label [ class "text-white" ]
                            [ text "AI Speed"
                            , input
                                [ class "w-16 bg-white rounded ml-3 text-black px-2 py-1"
                                , type_ "number"
                                , value (String.fromFloat model.aiSpeed)
                                , onInput UpdateAiSpeed
                                ]
                                []
                            ]
                        ]
                  ]
                ]
            )
        , div [ class "text-center" ]
            [ button [ class "bg-green-600 p-2 rounded hover:bg-green-500 transition duration-150 mt-6", onClick StartGame ] [ text "Start Game" ]
            ]
        ]


newPlayerInput : List PlayerColor -> NewPlayer -> Html Msg
newPlayerInput selectedColors player =
    div [ class "flex flex-col lg:flex-row py-2 items-center" ]
        [ div [ class "flex-shrink mr-3" ]
            [ button [ class "bg-gray-700 border-red-500 border-2 py-1 px-1 m-2 lg:m-0 rounded hover:bg-red-500 text-white transition duration-200", onClick (RemovePlayer player.id) ]
                [ text "Remove Player" ]
            ]
        , div [ class "flex-grow w-full lg:w-auto" ] [ input [ class "p-2 rounded w-full", type_ "text", value player.name, onInput (UpdateName player.id) ] [] ]
        , div [ class "flex-1" ]
            [ label [ class "text-white" ]
                [ text "Is Human"
                , input [ class "ml-3", type_ "checkbox", checked player.isHuman, onClick (UpdateHumanity player.id (not player.isHuman)) ] []
                ]
            ]
        , div [ class "flex-1 flex flex-col lg:flex-row items-center mx-1 w-full lg:w-auto" ]
            [ label [ class "text-white" ] [ text "Analyzer" ]
            , select [ class "p-2 lg:ml-3 rounded w-full lg:w-auto", onInput (UpdateAnalyzer player.id) ]
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
                    ("p-2 lg:mx-3 rounded w-32 w-full "
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
