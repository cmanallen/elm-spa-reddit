module Page exposing (Details, view)

import Browser
import Html exposing (Html)


type alias Details msg =
    { title : String
    , body : Html msg
    }


view : (a -> msg) -> Details a -> Browser.Document msg
view toMsg details =
    { title = details.title
    , body = [ Html.map toMsg <| details.body ]
    }