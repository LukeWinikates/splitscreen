module Splitscreen exposing (..)

import Html exposing (Attribute, Html, a, body, button, div, form, h1, header, iframe, input, li, node, span, text, textarea, ul)
import Html.Attributes exposing (href, placeholder, src, style, value)
import Html.Events exposing (onInput, onClick, onSubmit, onMouseOver)
import Navigation
import List exposing (filter, head)
import Maybe exposing (withDefault)
import Dict exposing (toList)
import Splitscreen.Model exposing (Model, appendToCol, fromUrl, key, modelToLayout, removeFromCol, toUrl, urlFor)
import Css exposing (Mixin, absolute, backgroundColor, block, margin, padding, border, borderColor, borderRadius, color, display, focus, height, hex, hover, none, pct, position, property, pt, px, stylesheet, textDecoration, transparent, width, (.), cursor, pointer, inlineBlock ,     textAlign ,center ,    fontSize, top, left)
import Css.Namespace exposing (namespace)
import Css.Helpers
import Html.CssHelpers


-- TODO: test onload handler, feedback for failures to load
-- TODO: make code generally cleaner
-- TODO: harmonize different data structures for "layout" (list of ints, list of list of (int, int), string representation)
-- TODO: add a "play" button that turns columns into a carousel (possibly using css animations)
-- TODO: Make `urls` dictionary vs. UrlChange less confusing
-- TODO: improve the spacing of the -/+ buttons. The top/bottom margin is uneven on the ones under the columns.
--      ... could count pixels, but don't want to tightly couple it to the calc(100% - 20px) for the column height
-- TODO: probably move CSS into its own module, if only because Css and Html imports don't play well together

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
    | Round
    | UrlBar


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
        , (.) Round
            [ borderRadius (pct 100)
            , property "transition" "background-color .3s"
            , margin (px 1)
            , borderColor (hex "111")
            , height (px 25)
            , width (px 25)
            , backgroundColor (hex "ccc")
            , focus [textDecoration none]
            , cursor pointer
            ,    display inlineBlock
            ,     textAlign center
             ,    fontSize (px 20)
             , hover
                     [ backgroundColor (hex "ddd")]
            ]
        , (.) UrlBar
            [ top (pt 0)
            , left (pt 0)
            , textAlign center
            , fontSize (pt 24)
            , position absolute
            , width (pct 100)
            , border (pt 0)
            , padding (pt 0)
            ]
        ]


{ id, class, classList } =
    Html.CssHelpers.withNamespace "splitscreen"


iframeView : ( Int, Int ) -> String -> Html Msg
iframeView coord url =
    div [ style [ ( "position", "relative" ), ( "flex-grow", "1" ) ] ]
        [ iframe [ src url, class [ UrlContent ] ] []
        , input
            [ class [ ShowOnHover, UrlBar ]
            , placeholder "type a url here..."
            , value url
            , onInput (coord |> key |> Change)
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
                            [ div [style [("vertical-align", "center")]]
                                [ a
                                    [ onClick (LayoutChange (removeFromCol index model.layout))
                                    , class [Round]
                                    ]
                                    [ text "-" ]
                                , a
                                    [ onClick (LayoutChange (appendToCol index model.layout))
                                    , class [Round]
                                    ]
                                    [ text "+" ]
                                ]
                            ]
                        )
                )
                layout
            )
            [ div [style [("width", "25px")]]
                [ a
                    [ onClick (LayoutChange (List.take ((List.length model.layout) - 1) model.layout))
                    , class [Round]
                    ]
                    [ text "-" ]
                , a
                    [ onClick (LayoutChange (List.append model.layout [ 1 ]))
                    , class [Round]
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
