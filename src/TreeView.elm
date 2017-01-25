module TreeView exposing (treeView, InternalState, Config, Data, Node(..), NodeValue)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Data =
    List Node


type Node
    = Node NodeValue


type alias NodeValue =
    { id : String, text : String, children : List Node }


type alias State =
    {}


type InternalState
    = InternalState State


type alias Config msg =
    { toMsg : InternalState -> msg
    , onChecked : List String -> msg
    , useCheckbox : Bool
    }


treeView : Config msg -> Data -> Html msg
treeView config data =
    ul [ style [ ( "padding-left", "0" ) ] ]
        (List.map (unbox >> renderNodeValue config) data)


renderNodeValue : Config msg -> NodeValue -> Html msg
renderNodeValue config nodeValue =
    li [ style [ ( "list-style-type", "none" ), ( "", "" ) ] ]
        [ if config.useCheckbox then
            label [] [ input [ type_ "checkbox" ] [], text nodeValue.text ]
          else
            text nodeValue.text
        , ul [] (List.map (unbox >> renderNodeValue config) nodeValue.children)
        ]


unbox : Node -> NodeValue
unbox node =
    case node of
        Node nodeValue ->
            nodeValue


map : (NodeValue -> b) -> Node -> b
map f =
    unbox >> f
