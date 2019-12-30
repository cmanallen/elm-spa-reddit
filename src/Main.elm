module Main exposing (Model, Msg, init, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (href)
import Url
import Url.Parser as P exposing ((</>))

import About
import Page
import SubReddit


-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


-- MODEL


type alias Model = { key : Nav.Key, page : Page }


-- INIT


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    router url { key = key, page = Home }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AboutMsg About.Msg
    | SubRedditMsg SubReddit.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            router url model
        
        AboutMsg aboutMsg ->
            case model.page of
                About aboutModel ->
                    stepAbout model ( About.update aboutMsg aboutModel )
                _ ->
                    ( model, Cmd.none )
        
        SubRedditMsg subRedditMsg ->
            case model.page of
                SubReddit subRedditModel ->
                    stepSubReddit model ( SubReddit.update subRedditMsg subRedditModel )
                _ ->
                    ( model, Cmd.none )


stepAbout : Model -> ( About.Model, Cmd About.Msg ) -> ( Model, Cmd Msg )
stepAbout model (aboutModel, aboutMsg) =
    ( { model | page = About aboutModel }
    , Cmd.map AboutMsg aboutMsg
    )


stepSubReddit : Model -> ( SubReddit.Model, Cmd SubReddit.Msg ) -> ( Model, Cmd Msg )
stepSubReddit model (subRedditModel, subRedditMsg) =
    ( { model | page = SubReddit subRedditModel }
    , Cmd.map SubRedditMsg subRedditMsg
    )


-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        Home ->
            viewBody model
        NotFound ->
            view404
        About aboutModel ->
            Page.view AboutMsg ( About.view aboutModel )
        SubReddit subRedditModel ->
            Page.view SubRedditMsg ( SubReddit.view subRedditModel )


viewBody : Model -> Browser.Document Msg
viewBody model =
    { title = "Test"
    , body = 
        [ a [href "/"] [text "Index"]
        , a [href "/about"] [text "About"]
        ]
    }


view404 : Browser.Document Msg
view404 =
    { title = "404"
    , body = 
        [ a [href "/"] [text "Index"]
        , a [href "/about"] [text "About"]
        , a [href "/r/all"] [text "All"]
        ]
    }


-- ROUTER


type Page
    = Home
    | NotFound
    | About About.Model
    | SubReddit SubReddit.Model


router : Url.Url -> Model -> (Model, Cmd Msg)
router url model =
    let
        parser = P.oneOf
            [ route (P.s "about") (stepAbout model ( About.init "Colton" "Allen" ))
            , route (P.s "r" </> P.string)
                (\subreddit ->
                    stepSubReddit model (SubReddit.init subreddit)
                )
            ]
    in
    case P.parse parser url of
        Just answer ->
            answer
        Nothing ->
            ( { model | page = NotFound }
            , Cmd.none
            )


route : P.Parser a b -> a -> P.Parser (b -> c) c
route parser handler = P.map handler parser
