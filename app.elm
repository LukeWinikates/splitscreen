module Main exposing (..)

import Html exposing (Attribute, Html, a, body, button, div, form, h1, header, iframe, input, li, node, span, text, textarea, ul)
import Html.Attributes exposing (class, href, placeholder, src, style, value)
import Html.Events exposing (onInput, onClick, onSubmit, onMouseOver)
import String exposing (split, dropLeft, startsWith)
import Navigation
import List exposing (filter, head)
import Maybe exposing (withDefault)
import Http


valueFromQueryString : String -> String -> Maybe String
valueFromQueryString key queryString =
    queryString
        |> dropLeft 2 -- drop '#?'
        |> split "&"
        |> filter (\term -> startsWith (key ++ "=") term)
        |> head
        |> Maybe.map (dropLeft ((String.length key) + 1))
        |> Maybe.map Http.decodeUri
        |> Maybe.map (withDefault "")


main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location = (fromUrl location.hash, Cmd.none)


fromUrl : String -> Model
fromUrl s =
    { newModel | first = valueFromQueryString "first" s, second = valueFromQueryString "second" s }


accumParam : String -> String -> Maybe String -> String
accumParam memo key value =
    memo ++ withDefault "" (Maybe.map (Http.encodeUri >> ((++) (key ++ "="))) value)


toUrl : Model -> String
toUrl model =
    "#?" |> \a -> accumParam a "first" model.first |> \a -> accumParam a "&second" model.second

type alias Model =
    { first : Maybe String
    , second : Maybe String
    }


newModel : Model
newModel =
    { first = Nothing, second = Nothing }

-- UPDATE


type Msg
    = ChangeFirst String
    | ChangeSecond String
    | UrlChange Navigation.Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeFirst newContent ->
            ({ model | first = Just newContent }, Navigation.modifyUrl (toUrl { model | first = Just newContent }))
        ChangeSecond newContent ->
            ({ model | second = Just newContent }, Navigation.modifyUrl (toUrl { model | second = Just newContent }))
        UrlChange location ->
            (model, Cmd.none)



-- VIEW


iframeView : (String -> Msg) -> String -> Html Msg
iframeView f url =
    let
        positioning =
            [ ( "height", "calc(100vh - 20px)" ), ( "width", "calc(50vw - 10px)" ), ( "border", "none" ), ( "border-right", "1px solid white" ) ]
    in
        div [ style [ ( "display", "inline-block" ), ( "position", "relative" ) ] ]
            [ iframe [ src url, style positioning ] []
            , input
                [ class "show-on-hover"
                , placeholder "type a url here..."
                , value url
                , onInput f
                , style (List.append [ ( "position", "absolute" ), ( "top", "0" ), ( "left", "0" ), ( "text-align", "center" ), ( "font-size", "24pt" ) ] positioning)
                ]
                []
            ]


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ text ".show-on-hover { transition: all 1s; background-color: transparent; color: transparent; }\n         .show-on-hover:hover { background-color: #ccc; color: #111 }" ]
        , iframeView ChangeFirst (withDefault "" model.first)
        , iframeView ChangeSecond (withDefault "" model.second)
        ]
