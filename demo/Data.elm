module Data exposing (tree, stylesheet)

import TreeView exposing (Node(..))
import Html.Attributes exposing (..)
import Html exposing (..)


tree : List Node
tree =
    [ Node
        { id = "1"
        , text = "1. One"
        , checked = False
        , children =
            [ Node
                { id = "1.1"
                , text = "1.1 One-One"
                , checked = False
                , children =
                    [ Node { id = "1.1.1", text = "1.1.1 One-One-One ", checked = False, children = [] }
                    , Node { id = "1.1.2", text = "1.1.2 One-One-Two", checked = False, children = [] }
                    ]
                }
            , Node { id = "1.2", text = "1.2 One-Two", checked = False, children = [] }
            ]
        }
    , Node
        { id = "2"
        , text = "2. Two"
        , checked = True
        , children = []
        }
    ]


stylesheet =
    let
        tag =
            "link"

        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "demo.css"
            ]

        children =
            []
    in
        node tag attrs children
