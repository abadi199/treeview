module Demo exposing (main)

import Html exposing (..)
import TreeView exposing (Node(..))


main =
    TreeView.treeView config
        [ Node
            { id = "1"
            , text = "1. One"
            , children =
                [ Node { id = "1.1", text = "1.1 One-One", children = [] }
                , Node { id = "1.2", text = "1.2 One-Two", children = [] }
                ]
            }
        , Node
            { id = "2"
            , text = "2. Two"
            , children = []
            }
        ]


config : TreeView.Config Msg
config =
    { toMsg = TreeViewStateChange, onChecked = TreeViewChecked, useCheckbox = False }


type Msg
    = TreeViewStateChange TreeView.InternalState
    | TreeViewChecked (List String)
