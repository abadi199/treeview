module Demo exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import TreeView exposing (Node(..))


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


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { state : TreeView.InternalState }


init =
    ( { state = TreeView.initialState }, Cmd.none )


view model =
    div []
        [ stylesheet
        , TreeView.treeView config
            model.state
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
        ]


config : TreeView.Config Msg
config =
    { onState = StateChange, onChecked = Checked, useCheckbox = True }


type Msg
    = StateChange TreeView.InternalState
    | Checked (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StateChange internalState ->
            { model | state = internalState } ! []

        Checked list ->
            model ! []
