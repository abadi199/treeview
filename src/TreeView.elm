module TreeView
    exposing
        ( treeView
        , InternalState
        , Config
        , Data
        , Node(..)
        , NodeValue
        , initialState
        , subscriptions
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import Json.Decode
import AnimationFrame
import Time
import DOM


subscriptions : Config msg -> InternalState -> Sub msg
subscriptions config internalState =
    let
        state =
            unboxState internalState

        updateState time =
            let
                updatedTime =
                    { state | currentTime = time }

                updatedState =
                    case state.action of
                        Idle ->
                            updatedTime

                        Expanding ->
                            updateHeights config updatedTime
            in
                updatedState
                    |> InternalState
                    |> config.onState
    in
        AnimationFrame.times updateState


updateHeights : Config msg -> State -> State
updateHeights config state =
    let
        collapsed =
            not <| Dict.member state.id state.expandedNodes

        updateExpandedNodes =
            if collapsed then
                Dict.insert state.id ()
            else
                Dict.remove state.id

        updateState height =
            let
                _ =
                    Debug.log "height" height
            in
                state.expandedNodes
                    |> updateExpandedNodes
                    |> (\expandedNodes ->
                            { state
                                | expandedNodes = expandedNodes
                                , action = Idle
                                , heights =
                                    if height > 0 then
                                        Dict.insert state.id height state.heights
                                    else
                                        state.heights
                            }
                       )

        decode : Json.Decode.Decoder Float
        decode =
            DOM.target <| DOM.nextSibling <| DOM.nextSibling (DOM.boundingClientRect |> Json.Decode.map .height)
    in
        state.currentNode
            |> Maybe.map (Json.Decode.decodeValue decode)
            |> Maybe.andThen Result.toMaybe
            |> Maybe.map updateState
            |> Maybe.withDefault state


type alias Data =
    List Node


type Node
    = Node NodeValue


type alias NodeValue =
    { id : String, text : String, children : List Node }


type alias State =
    { expandedNodes : Dict String ()
    , currentTime : Time.Time
    , currentNode : Maybe Json.Decode.Value
    , heights : Dict String Float
    , action : Action
    , id : String
    }


initialState : InternalState
initialState =
    InternalState
        { expandedNodes = Dict.empty
        , currentTime = 0
        , id = ""
        , currentNode = Nothing
        , heights = Dict.empty
        , action = Idle
        }


type Action
    = Idle
    | Expanding


type InternalState
    = InternalState State


unboxState : InternalState -> State
unboxState internalState =
    case internalState of
        InternalState state ->
            state


type alias Config msg =
    { onState : InternalState -> msg
    , onChecked : List String -> msg
    , useCheckbox : Bool
    }


defaultConfig : (InternalState -> msg) -> (List String -> msg) -> Config msg
defaultConfig onState onChecked =
    { onState = onState
    , onChecked = onChecked
    , useCheckbox = True
    }


treeView : Config msg -> InternalState -> Data -> Html msg
treeView config internalState data =
    let
        state =
            unboxState internalState

        styles =
            case state.action of
                Expanding ->
                    style [ ( "visibility", "hidden" ) ]

                Idle ->
                    style []
    in
        ul [ class "treeview", styles ]
            (List.map (unbox >> renderNodeValue config state) data)


renderNodeValue : Config msg -> State -> NodeValue -> Html msg
renderNodeValue config state nodeValue =
    let
        collapsed =
            not <| Dict.member nodeValue.id state.expandedNodes

        hasChildren =
            not <| List.isEmpty nodeValue.children
    in
        li []
            [ span
                [ classList [ ( "arrow", True ), ( "no-children", not hasChildren ) ]
                , arrowClick config state nodeValue.id
                ]
                [ case ( hasChildren, collapsed ) of
                    ( True, True ) ->
                        text "▶"

                    ( True, False ) ->
                        text "▼"

                    _ ->
                        text ""
                ]
            , if config.useCheckbox then
                label [] [ input [ type_ "checkbox" ] [], text nodeValue.text ]
              else
                text nodeValue.text
            , ul
                [ class <|
                    "node "
                        ++ (if collapsed then
                                "collapsed"
                            else
                                "expanded"
                           )
                , style <|
                    if state.action == Expanding then
                        [ ( "height", "auto" ) ]
                    else if not <| Dict.member nodeValue.id state.heights then
                        [ ( "height", "0" ) ]
                    else
                        [ ( "height", (Dict.get nodeValue.id state.heights |> Maybe.withDefault 0 |> toString) ++ "px" ) ]
                ]
                (List.map (unbox >> renderNodeValue config state) nodeValue.children)
            , text <| toString state.action
            ]


arrowClick : Config msg -> State -> String -> Html.Attribute msg
arrowClick config state id =
    let
        updateState value =
            { state | currentNode = Just value, action = Expanding }
                |> InternalState
                |> config.onState
    in
        on "click" (Json.Decode.value |> Json.Decode.map updateState)


arrowClick_ : Config msg -> State -> String -> Html.Attribute msg
arrowClick_ config state id =
    let
        collapsed =
            not <| Dict.member id state.expandedNodes

        updateExpandedNodes =
            if collapsed then
                Dict.insert id ()
            else
                Dict.remove id

        updatedState height =
            state.expandedNodes
                |> updateExpandedNodes
                |> (\expandedNodes ->
                        { state
                            | expandedNodes = expandedNodes
                            , heights =
                                if height > 0 then
                                    Dict.insert id height state.heights
                                else
                                    state.heights
                        }
                   )
                |> InternalState
                |> config.onState

        decode : Json.Decode.Decoder Float
        decode =
            DOM.target <| DOM.nextSibling <| DOM.nextSibling (DOM.boundingClientRect |> Json.Decode.map .height)
    in
        on "click" (decode |> Json.Decode.map (updatedState))


unbox : Node -> NodeValue
unbox node =
    case node of
        Node nodeValue ->
            nodeValue


map : (NodeValue -> b) -> Node -> b
map f =
    unbox >> f
