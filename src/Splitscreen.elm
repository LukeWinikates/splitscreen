module Splitscreen exposing (..)

import Html exposing (Attribute, Html, a, body, button, div, form, h1, header, iframe, input, li, node, span, text, textarea, ul)
import Html.Attributes exposing (href, placeholder, src, style, value)
import Html.Events exposing (onInput, onClick, onSubmit, onMouseOver)
import Navigation
import List exposing (filter, head)
import Maybe exposing (withDefault)
import Dict exposing (toList)
import Splitscreen.Model exposing (Model, appendToCol, fromUrl, key, modelToLayout, removeFromCol, toUrl, urlFor)
import Css exposing (Mixin, absolute, backgroundColor, block, border, color, display, height, hex, hover, none, pct, position, property, pt, stylesheet, transparent, width, (.))
import Css.Namespace exposing (namespace)
import Css.Helpers
import Html.CssHelpers


-- TODO: test onload handler, feedback for failures to load
-- TODO: make code generally cleaner
-- TODO: harmonize different data structures for "layout" (list of ints, list of list of (int, int), string representation)
-- TODO: add a "play" button that turns columns into a carousel (possibly using css animations)
-- TODO: Make `urls` dictionary vs. UrlChange less confusing


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
    | LayoutChange (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        wrap model =
            ( model, model |> toUrl |> Navigation.modifyUrl )
    in
        case msg of
            Change key newContent ->
                wrap { model | urls = Dict.insert key newContent model.urls }

            LayoutChange newLayout ->
                wrap { model | layout = newLayout }

            UrlChange location ->
                ( model, Cmd.none )


type CssClasses
    = UrlContent
    | ShowOnHover


css =
    (stylesheet << namespace "splitscreen")
        [ (.) UrlContent
            [ border (pt 0)
            , width (pct 100)
            , height (pct 100)
            , display block
            , position absolute
            ]
        , (.) ShowOnHover
            [ property "transition" "1s"
            , backgroundColor transparent
            , color transparent
            , hover
                [ backgroundColor (hex "ccc"), color (hex "111") ]
            ]
        ]


{ id, class, classList } =
    Html.CssHelpers.withNamespace "splitscreen"


iframeView : ( Int, Int ) -> String -> Html Msg
iframeView coord url =
    div [ style [ ( "position", "relative" ), ( "flex-grow", "1" ) ] ]
        [ iframe [ src url, class [ UrlContent ] ] []
        , input
            [ class [ ShowOnHover ]
            , placeholder "type a url here..."
            , value url
            , onInput (coord |> key |> Change)
            , style [ ( "top", "0" ), ( "left", "0" ), ( "text-align", "center" ), ( "font-size", "24pt" ), ( "position", "absolute" ), ( "width", "100%" ), ( "border", "none" ), ( "padding", "0" ) ]
            ]
            []
        ]


layoutView : Model -> List (List ( Int, Int )) -> Html Msg
layoutView model layout =
    div [ style [ ( "display", "flex" ), ( "border-right", "1px solid white" ), ( "width", "calc(100vw - 10px)" ) ] ]
        (List.append
            (List.indexedMap
                (\index col ->
                    span [ style [ ( "flex-grow", "1" ), ( "display", "flex" ), ( "flex-direction", "column" ), ( "height", "calc(100vh - 10px)" ) ] ]
                        (List.append
                            (List.map
                                (\xy -> iframeView xy (urlFor model xy))
                                col
                            )
                            [ div []
                                [ button
                                    [ onClick (LayoutChange (removeFromCol index model.layout))
                                    , style [ ( "width", "5em" ), ( "display", "inline-block" ) ]
                                    ]
                                    [ text "-" ]
                                , button
                                    [ onClick (LayoutChange (appendToCol index model.layout))
                                    , style [ ( "width", "5em" ), ( "display", "inline-block" ) ]
                                    ]
                                    [ text "+" ]
                                ]
                            ]
                        )
                )
                layout
            )
            [ div []
                [ button
                    [ onClick (LayoutChange (List.take ((List.length model.layout) - 1) model.layout))
                    , style [ ( "right", "0" ), ( "width", "100%" ) ]
                    ]
                    [ text "-" ]
                , button
                    [ onClick (LayoutChange (List.append model.layout [ 1 ]))
                    , style [ ( "right", "0" ), ( "width", "100%" ) ]
                    ]
                    [ text "+" ]
                ]
            ]
        )


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ text (.css (Css.compile [ css ]))]
        , layoutView model (modelToLayout model)
        ]
