module Splitscreen exposing (..)

import Html exposing (Attribute, Html, a, body, button, div, form, h1, header, iframe, input, li, node, span, text, textarea, ul)
import Html.Attributes exposing (class, href, placeholder, src, style, value)
import Html.Events exposing (onInput, onClick, onSubmit, onMouseOver)
import Navigation
import List exposing (filter, head)
import Maybe exposing (withDefault)
import Dict exposing (toList)
import Splitscreen.Model exposing (Model, fromUrl, toUrl)


-- TODO: test onload handler, feedback for failures to load
-- TODO: make code generally cleaner
-- TODO: add close button
-- TODO: harmonize different data structures for "layout" (list of ints, list of list of (int, int), string representation)
-- TODO: add a "play" button that turns columns into a carousel (possibly using css animations)


main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( fromUrl location.hash, Cmd.none )


type Msg
    = Change String String
    | UrlChange Navigation.Location
    | Layout (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change key newContent ->
            let
                newModel =
                    { model | urls = Dict.insert key newContent model.urls }
            in
                ( newModel, Navigation.modifyUrl (toUrl newModel) )

        Layout newLayout ->
            let
                newModel =
                    { model | layout = newLayout }
            in
                ( newModel, Navigation.modifyUrl (toUrl newModel) )

        UrlChange location ->
            ( model, Cmd.none )



--type alias Layout = List List (Int, Int)


modelToLayout : Model -> List (List ( Int, Int ))
modelToLayout model =
    List.indexedMap
        (\colNum rowCount -> List.map ((,) colNum) (List.range 0 (rowCount - 1)))
        model.layout


iframeView : String -> String -> Html Msg
iframeView key url =
    let
        positioning =
            [ ( "border", "none" ), ( "width", "100%" ), ( "height", "100%" ), ( "display", "block" ), ( "position", "absolute" ) ]
    in
        div [ style [ ( "position", "relative" ), ( "flex-grow", "1" ) ] ]
            [ iframe [ src url, style positioning ] []
            , input
                [ class "show-on-hover"
                , placeholder "type a url here..."
                , value url
                , onInput (Change key)
                , style [ ( "top", "0" ), ( "left", "0" ), ( "text-align", "center" ), ( "font-size", "24pt" ), ( "position", "absolute" ), ( "width", "100%" ), ( "border", "none" ), ( "padding", "0" ) ]
                ]
                []
            ]


appendToCol column modelLayout =
    List.indexedMap
        (\index count ->
            count
                + if index == column then
                    1
                  else
                    0
        )
        modelLayout


layoutView : Model -> List (List ( Int, Int )) -> Html Msg
layoutView model layout =
    div [ style [ ( "display", "flex" ), ( "border-right", "1px solid white" ), ( "width", "calc(100vw - 10px)" ) ] ]
        (List.append
            (List.indexedMap
                (\index col ->
                    span [ style [ ( "flex-grow", "1" ), ( "display", "flex" ), ( "flex-direction", "column" ), ( "height", "calc(100vh - 10px)" ) ] ]
                        (List.append
                            (List.map
                                (\( x, y ) ->
                                    let
                                        key =
                                            "x" ++ toString x ++ "y" ++ toString y
                                    in
                                        iframeView key (withDefault "" (Dict.get key model.urls))
                                )
                                col
                            )
                            [ button
                                [ onClick (Layout (appendToCol index model.layout))
                                , style [ ( "bottom", "0" ), ( "width", "100%" ) ]
                                ]
                                [ text "+" ]
                            ]
                        )
                )
                layout
            )
            [ button
                [ onClick (Layout (List.append model.layout [ 1 ]))
                , style [ ( "right", "0" ), ( "height", "100%" ), ( "height", "calc(100vh - 10px)")]
                ]
                [ text "+" ]
            ]
        )


view : Model -> Html Msg
view model =
    div []
        [ node "style"
            []
            [ text
                (".show-on-hover { transition: all 1s; background-color: transparent; color: transparent; }\n"
                    ++ ".show-on-hover:hover { background-color: #ccc; color: #111 }"
                )
            ]
        , layoutView model (modelToLayout model)
        ]
