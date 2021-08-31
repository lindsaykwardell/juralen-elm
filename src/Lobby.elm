module Lobby exposing (..)

import Array
import Html exposing (Html, button, div, h1, input, label, option, select, text)
import Html.Attributes exposing (checked, class, selected, type_, value)
import Html.Events exposing (onClick, onInput)
import Juralen.AnalyzerMode exposing (AnalyzerMode)
import Juralen.Player exposing (NewPlayer)
import Juralen.PlayerColor exposing (PlayerColor)
import Random


type alias Model =
    { newPlayerList : List NewPlayer
    , nextId : Int
    , aiSpeed : Float
    }


type Msg
    = UpdateName Int String
    | UpdateHumanity Int Bool
    | UpdateColor Int String
    | UpdateAnalyzer Int String
    | UpdateAiSpeed String
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
            [ { id = 1, name = "Player 1", isHuman = True, color = Juralen.PlayerColor.Red, analyzer = Juralen.AnalyzerMode.Default }
            ]
      , nextId = 2
      , aiSpeed = 250
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
            Juralen.PlayerColor.None


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
                                { newPlayer | color = Juralen.PlayerColor.fromString color }

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
                            Juralen.AnalyzerMode.Default

                        "AGGRESSIVE" ->
                            Juralen.AnalyzerMode.Aggressive

                        "DEFENSIVE" ->
                            Juralen.AnalyzerMode.Defensive

                        "PASSIVE" ->
                            Juralen.AnalyzerMode.Passive

                        "EXPANSIONIST" ->
                            Juralen.AnalyzerMode.Expansionist

                        _ ->
                            Juralen.AnalyzerMode.Default

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

        AddPlayer ->
            if List.length model.newPlayerList < 8 then
                let
                    nextId =
                        model.nextId + 1

                    color : PlayerColor
                    color =
                        firstAvailableColor Juralen.PlayerColor.toList (List.map (\player -> player.color) model.newPlayerList)

                    newPlayer =
                        { id = model.nextId, name = "Player " ++ String.fromInt model.nextId, isHuman = False, color = color, analyzer = Juralen.AnalyzerMode.Default }

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
                                Juralen.AnalyzerMode.Expansionist

                            else if randomNumber // 3 == 1 then
                                Juralen.AnalyzerMode.Passive

                            else if randomNumber // 4 == 1 then
                                Juralen.AnalyzerMode.Defensive

                            else if randomNumber // 5 == 1 then
                                Juralen.AnalyzerMode.Aggressive

                            else
                                Juralen.AnalyzerMode.Default
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
    div [ class "m-auto w-3/4" ]
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
        [ div [ class "flex-grow w-full lg:w-auto" ] [ input [ class "p-2 rounded w-full", type_ "text", value player.name, onInput (UpdateName player.id) ] [] ]
        , div [ class "flex-1" ]
            [ label [ class "text-white" ]
                [ text "Is Human"
                , input [ class "ml-3", type_ "checkbox", checked player.isHuman, onClick (UpdateHumanity player.id (not player.isHuman)) ] []
                ]
            ]
        , div [ class "flex-1 flex flex-col lg:flex-row items-center mx-1 w-full lg:w-auto" ]
            [ label [ class "text-white" ] [ text "Analyzer" ]
            , select [ class "p-2 lg:ml-3 rounded w-full lg:w-auto", onInput (UpdateAnalyzer player.id) ]
                [ option [ value "DEFAULT", selected (player.analyzer == Juralen.AnalyzerMode.Default) ] [ text "Default" ]
                , option [ value "AGGRESSIVE", selected (player.analyzer == Juralen.AnalyzerMode.Aggressive) ] [ text "Aggressive" ]
                , option [ value "DEFENSIVE", selected (player.analyzer == Juralen.AnalyzerMode.Defensive) ] [ text "Defensive" ]
                , option [ value "PASSIVE", selected (player.analyzer == Juralen.AnalyzerMode.Passive) ] [ text "Passive" ]
                , option [ value "EXPANSIONIST", selected (player.analyzer == Juralen.AnalyzerMode.Expansionist) ] [ text "Expansionist" ]
                ]
            ]
        , div [ class "flex-1 flex flex-col lg:flex-row items-center mx-1 w-full lg:w-auto m-2 lg:m-0" ]
            [ label [ class "text-white " ]
                [ text "Color" ]
            , select
                [ class
                    ("p-2 lg:ml-3 rounded w-32 w-full lg:w-auto "
                        ++ Juralen.PlayerColor.toClass player.color
                        ++ (if Juralen.PlayerColor.isDark player.color then
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
                            [ value (Juralen.PlayerColor.toString playerColor)
                            , selected (playerColor == player.color)
                            , class (Juralen.PlayerColor.toClass playerColor)
                            ]
                            [ text (playerColor |> Juralen.PlayerColor.toString |> String.toUpper) ]
                    )
                    (List.filter
                        (\color ->
                            color
                                == player.color
                                || not (List.foldl (\selectedColor found -> found || selectedColor == color) False selectedColors)
                        )
                        Juralen.PlayerColor.toList
                    )
                )
            ]
        , div [ class "flex-shrink" ]
            [ button [ class "bg-gray-700 border-red-500 border-2 py-1 px-1 m-2 lg:m-0 rounded hover:bg-red-500 text-white transition duration-200", onClick (RemovePlayer player.id) ]
                [ text "Remove Player" ]
            ]
        ]
