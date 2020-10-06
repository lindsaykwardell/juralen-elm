module Lobby exposing (..)

import Html exposing (Attribute, Html, button, div, h1, input, label, option, select, text)
import Html.Attributes exposing (checked, class, selected, type_, value)
import Html.Events exposing (onClick, onInput)
import Juralen.Player exposing (NewPlayer)
import Juralen.PlayerColor exposing (PlayerColor)
import Juralen.PlayerColor
import Juralen.PlayerColor


type alias Model =
    { newPlayerList : List NewPlayer
    , nextId : Int
    }


type Msg
    = UpdateName Int String
    | UpdateHumanity Int Bool
    | UpdateColor Int String
    | AddPlayer
    | RemovePlayer Int
    | StartGame


init : ( Model, Cmd Msg )
init =
    ( { newPlayerList =
            [ { id = 1, name = "Lindsay", isHuman = True, color = Juralen.PlayerColor.Red }
            , { id = 2, name = "Ilthanen Juralen", isHuman = False, color = Juralen.PlayerColor.Blue }
            , { id = 3, name = "Velsyph", isHuman = False, color = Juralen.PlayerColor.Green }
            , { id = 4, name = "Dakh", isHuman = False, color = Juralen.PlayerColor.Yellow }
            ]
      , nextId = 5
      }
    , Cmd.none
    )

firstAvailableColor : List PlayerColor -> List PlayerColor -> PlayerColor
firstAvailableColor colorList selectedColors =
    case colorList of
        (color :: remainingColors) ->
            if 
                List.foldl (\selectedColor found -> found || color == selectedColor) False selectedColors
            then
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

        AddPlayer ->
            if List.length model.newPlayerList < 8 then
                let
                    nextId =
                        model.nextId + 1

                    color : PlayerColor
                    color = firstAvailableColor Juralen.PlayerColor.toList (List.map (\player -> player.color) model.newPlayerList)

                    newPlayer =
                        { id = model.nextId, name = "Player " ++ String.fromInt model.nextId, isHuman = False, color = color }

                    newPlayerList =
                        model.newPlayerList ++ [ newPlayer ]
                in
                ( { model | newPlayerList = newPlayerList, nextId = nextId }, Cmd.none )

            else
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
        [ h1 [ class "text-white" ] [ text "Start New Game" ]
        , div [ class "flex justify-end" ]
            [ div [ class "w-1/5" ] [ button [ class "p-3 w-full bg-gray-400 hover:bg-gray-600 rounded-t", onClick AddPlayer ] [ text "Add Player" ] ]
            ]
        , div [ class "bg-gray-700 p-2 shadow rounded-tl rounded-b" ] (List.map 
            (newPlayerInput 
                (List.map (\player -> player.color) model.newPlayerList)
            ) 
            model.newPlayerList)
        , div [ class "text-center" ]
            [ button [ class "bg-green-600 p-2 rounded hover:bg-green-500 transition duration-150 mt-6", onClick StartGame ] [ text "Start Game" ]
            ]
        ]


newPlayerInput : List PlayerColor -> NewPlayer -> Html Msg
newPlayerInput selectedColors player =
    div [ class "flex py-2 items-center" ]
        [ div [ class "flex-grow" ] [ input [ class "p-2 rounded w-full", type_ "text", value player.name, onInput (UpdateName player.id) ] [] ]
        , div [ class "flex-1" ]
            [ label [ class "text-white" ]
                [ text "Is Human"
                , input [ class "ml-3", type_ "checkbox", checked player.isHuman, onClick (UpdateHumanity player.id (not player.isHuman)) ] []
                ]
            ]
        , div [ class "flex-1" ]
            [ label [ class "text-white" ]
                [ text "Color"
                , select [ class (
                        "p-2 ml-3 rounded w-64 " 
                        ++ Juralen.PlayerColor.toClass player.color 
                        ++ (if Juralen.PlayerColor.isDark player.color then "" else " text-black")
                    ), onInput (UpdateColor player.id) ] (
                    List.map (\playerColor -> 
                        option 
                            [ value (Juralen.PlayerColor.toString playerColor)
                            , selected (playerColor == player.color)
                            , class (Juralen.PlayerColor.toClass playerColor)
                            ] 
                            [ text (playerColor |> Juralen.PlayerColor.toString |> String.toUpper) ]) 
                            (List.filter (\color -> color == player.color || 
                                    not (List.foldl (\selectedColor found -> found || selectedColor == color) False selectedColors)
                                ) 
                                Juralen.PlayerColor.toList))
                ]
            ]
        , div [ class "flex-shrink" ]
            [
                button [ class "w-32 bg-gray-700 border-red-500 border-2 py-1 px-1 rounded hover:bg-red-500 text-white transition duration-200", onClick (RemovePlayer player.id) ] 
                    [ text "Remove Player" ]
            ]
        ]
