module Demo exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import TreeView exposing (Node(..))
import Data


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


config : TreeView.Config Msg
config =
    { onState = StateChange, onChecked = Checked, useCheckbox = True }


subscriptions : Model -> Sub Msg
subscriptions model =
    TreeView.subscriptions config model.state


type alias Model =
    { state : TreeView.InternalState }


init : ( Model, Cmd Msg )
init =
    ( { state = TreeView.initialState }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ Data.stylesheet
        , TreeView.treeView config
            model.state
            Data.tree
        ]


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
