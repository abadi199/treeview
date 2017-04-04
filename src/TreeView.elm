module TreeView
    exposing
        ( treeView
        , InternalState
        , Config
        , Data
        , Node(..)
        , NodeValue
        , initialState
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set exposing (Set)
import Json.Decode


type alias Data =
    List Node


type Node
    = Node NodeValue


type alias NodeValue =
    { id : String, text : String, children : List Node }


type alias State =
    { expandedNodes : Set String
    }


initialState : InternalState
initialState =
    InternalState
        { expandedNodes = Set.empty }


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
    , nodeHeight : Int
    }


defaultConfig : (InternalState -> msg) -> (List String -> msg) -> Config msg
defaultConfig onState onChecked =
    { onState = onState
    , onChecked = onChecked
    , useCheckbox = True
    , nodeHeight = 10
    }


treeView : Config msg -> InternalState -> Data -> Html msg
treeView config internalState data =
    let
        state =
            unboxState internalState
    in
        div []
            [ ul [ class "treeview" ]
                (List.map (unbox >> renderNodeValue config state) data)
            ]


renderNodeValue : Config msg -> State -> NodeValue -> Html msg
renderNodeValue config state nodeValue =
    li [ setNodeHeight config state nodeValue ]
        [ span
            [ classList [ ( "arrow", True ), ( "no-children", not (hasChildren nodeValue) ) ]
            , arrowClick config state nodeValue.id
            ]
            [ case ( hasChildren nodeValue, isCollapsed state nodeValue ) of
                ( True, True ) ->
                    text "+"

                ( True, False ) ->
                    text "-"

                _ ->
                    text ""
            ]
        , leaf config nodeValue
        , children config state nodeValue
        ]


setNodeHeight : Config msg -> State -> NodeValue -> Attribute msg
setNodeHeight config state nodeValue =
    style
        [ ( "height", toString (calculateHeight config state nodeValue) ++ "px" )
        , ( "transition", "height 0.5s" )
        , ( "overflow", "hidden" )
        ]


calculateHeight : Config msg -> State -> NodeValue -> Int
calculateHeight config state nodeValue =
    if hasVisibleChildren state nodeValue then
        List.foldl
            (\node height -> height + calculateHeight config state (unbox node))
            config.nodeHeight
            nodeValue.children
    else
        config.nodeHeight


leaf : Config msg -> NodeValue -> Html msg
leaf config nodeValue =
    if config.useCheckbox then
        label [ setLeafHeight config ]
            [ input [ type_ "checkbox" ] [], text nodeValue.text ]
    else
        div
            [ setLeafHeight config ]
            [ text nodeValue.text ]


children : Config msg -> State -> NodeValue -> Html msg
children config state nodeValue =
    if hasVisibleChildren state nodeValue then
        ul
            [ class <|
                "node "
                    ++ (if isCollapsed state nodeValue then
                            "collapsed"
                        else
                            "expanded"
                       )
            ]
            (List.map (unbox >> renderNodeValue config state) nodeValue.children)
    else
        text ""


hasChildren : NodeValue -> Bool
hasChildren nodeValue =
    not (List.isEmpty nodeValue.children)


hasVisibleChildren : State -> NodeValue -> Bool
hasVisibleChildren state nodeValue =
    hasChildren nodeValue && not (isCollapsed state nodeValue)


isCollapsed : State -> NodeValue -> Bool
isCollapsed state nodeValue =
    not <| Set.member nodeValue.id state.expandedNodes


setLeafHeight : Config msg -> Attribute msg
setLeafHeight config =
    style [ ( "height", toString config.nodeHeight ++ "px" ), ( "display", "inline-block" ) ]


arrowClick : Config msg -> State -> String -> Html.Attribute msg
arrowClick config state id =
    let
        updateState value =
            { state
                | expandedNodes = updateExpandedNodes state id
            }
                |> InternalState
                |> config.onState
    in
        on "click" (Json.Decode.value |> Json.Decode.map updateState)


updateExpandedNodes : State -> String -> Set String
updateExpandedNodes state id =
    if Set.member id state.expandedNodes then
        Set.remove id state.expandedNodes
    else
        Set.insert id state.expandedNodes


unbox : Node -> NodeValue
unbox node =
    case node of
        Node nodeValue ->
            nodeValue


map : (NodeValue -> b) -> Node -> b
map f =
    unbox >> f
