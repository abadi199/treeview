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
            config.onState <| InternalState { state | currentTime = time }
    in
        AnimationFrame.times updateState


type alias Data =
    List Node


type Node
    = Node NodeValue


type alias NodeValue =
    { id : String, text : String, children : List Node }


type alias State =
    { expandedNodes : Dict String ()
    , currentTime : Time.Time
    , heights : Dict String Float
    }


initialState : InternalState
initialState =
    InternalState
        { expandedNodes = Dict.empty
        , currentTime = 0
        , heights = Dict.empty
        }


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
    ul [ class "treeview" ]
        (List.map (unbox >> renderNodeValue config (unboxState internalState)) data)


renderNodeValue : Config msg -> State -> NodeValue -> Html msg
renderNodeValue config state nodeValue =
    let
        collapsed =
            not <| Dict.member nodeValue.id state.expandedNodes

        hasChildren =
            not <| List.isEmpty nodeValue.children
    in
        li []
            [ span [ classList [ ( "arrow", True ), ( "no-children", not hasChildren ) ], arrowClick config state nodeValue.id ]
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
                    if collapsed then
                        [ ( "height", "0px" ) ]
                    else
                        [ ( "height", (Dict.get nodeValue.id state.heights |> Maybe.withDefault 0 |> toString) ++ "px" ) ]
                ]
                (List.map (unbox >> renderNodeValue config state) nodeValue.children)
            ]


arrowClick : Config msg -> State -> String -> Html.Attribute msg
arrowClick config state id =
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
