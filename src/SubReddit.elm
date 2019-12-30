module SubReddit exposing (Model, Msg, init, update, view)

import Browser
import Json.Decode as D
import Http
import Html exposing (..)
import Html.Attributes exposing (href)

import Page


-- MODEL


type alias Model =
    { subreddit : String
    , requestState : RequestState
    }


type RequestState
    = Loading
    | Failure Http.Error
    | Success Posts


-- INIT


init : String -> (Model, Cmd Msg)
init subreddit =
    ({ subreddit = subreddit, requestState = Loading }
    , fetchSubreddit subreddit)


-- UPDATE


type Msg
    = Fetch String
    | Receive (Result Http.Error Posts)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch subreddit ->
            ( { subreddit = subreddit, requestState = Loading }
            , fetchSubreddit subreddit )

        Receive result ->
            case result of
                Ok posts ->
                    ( { model | requestState = Success posts }
                    , Cmd.none )

                Err error ->
                    ( { model | requestState = Failure error }
                    , Cmd.none )


-- VIEW


view : Model -> Page.Details Msg
view model =
    case model.requestState of
        Success posts ->
            { title = model.subreddit
            , body = div [] [ renderPosts posts ]
            }

        Loading ->
            { title = "Loading..."
            , body = div [] [ text "Loading..." ]
            }

        Failure _ ->
            { title = "Request Failed."
            , body = div [] [ text "Invalid subreddit." ]
            }


renderPosts : Posts -> Html Msg
renderPosts posts =
    posts.posts
        |> List.map (\post -> renderPost post)
        |> ol []


renderPost : Post -> Html Msg
renderPost post =
    li []
        [ a [ href post.source ] [ text post.title ]
        , br [] []
        , span []
            [ text (renderAuthorMeta post ++ " | ")
            , renderCommentMeta post
            ]
        ]


renderAuthorMeta : Post -> String
renderAuthorMeta post =
    "Author: " ++ post.author


renderCommentMeta : Post -> Html Msg
renderCommentMeta post =
    a [ href ("https://www.reddit.com" ++ post.permalink) ]
        [ text (String.fromInt post.comments ++ " comments")
        ]


-- HTTP


type alias Posts =
    { posts : List Post }


postsDecoder : D.Decoder Posts
postsDecoder =
    D.map
        Posts
        (D.field "data" (D.field "children" (D.list (D.field "data" postDecoder))))


type alias Post =
    { title : String
    , author : String
    , source : String
    , permalink : String
    , ups : Int
    , downs : Int
    , comments : Int
    }


postDecoder : D.Decoder Post
postDecoder =
    D.map7
        Post
        (D.field "title" D.string)
        (D.field "author" D.string)
        (D.field "url" D.string)
        (D.field "permalink" D.string)
        (D.field "ups" D.int)
        (D.field "downs" D.int)
        (D.field "num_comments" D.int)


fetchSubreddit : String -> Cmd Msg
fetchSubreddit subreddit =
    Http.get
        { url = "https://www.reddit.com/r/" ++ subreddit ++ "/.json"
        , expect = Http.expectJson Receive postsDecoder
        }
