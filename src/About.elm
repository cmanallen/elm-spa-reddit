module About exposing (Model, Msg, init, update, view)

import Html exposing (br, div, input, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)

import Page


type alias Model =
    { firstName : String
    , lastName : String
    }


type Msg
    = SetFirstName String
    | SetLastName String


view : Model -> Page.Details Msg
view model =
    { title = "About " ++ model.firstName
    , body = div []
        [ div [] [ text ("Hello " ++ model.firstName ++ " " ++ model.lastName) ]
        , br [] []
        , input [onInput SetFirstName, placeholder "Set First Name"] []
        , br [] []
        , input [onInput SetLastName, placeholder "Set Last Name"] []
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetFirstName value ->
            ( { model | firstName = value }, Cmd.none)
        SetLastName value ->
            ( { model | lastName = value }, Cmd.none)


init : String -> String -> ( Model, Cmd Msg )
init firstName lastName =
    ( { firstName = firstName, lastName = lastName }
    , Cmd.none)
