module Splitscreen exposing (..)

import Html exposing (Attribute, Html, a, body, button, div, form, h1, header, iframe, input, li, node, span, text, textarea, ul)
import Html.Attributes exposing (href, placeholder, src, style, value)
import Html.Events exposing (onInput, onClick, onSubmit, onMouseOver)
import Navigation
import List exposing (filter, head)
import Maybe exposing (withDefault)
import Dict exposing (toList)
import Css.Namespace exposing (namespace)
import Css.Helpers
import Html.CssHelpers

import Splitscreen.Model exposing (Model, appendToCol, fromUrl, key, modelToLayout, removeFromCol, toUrl, urlFor)
import Splitscreen.Style exposing (..)


-- TODO: test onload handler, feedback for failures to load
-- TODO: make code generally cleaner
-- TODO: harmonize different data structures for "layout" (list of ints, list of list of (int, int), string representation)
-- TODO: add a "play" button that turns columns into a carousel (possibly using css animations)
-- TODO: Make `urls` dictionary vs. UrlChange less confusing
-- TODO: improve the spacing of the -/+ buttons. The top/bottom margin is uneven on the ones under the columns.
--      ... could count pixels, but don't want to tightly couple it to the calc(100% - 20px) for the column height
-- TODO: if there's no layout, show help text about what this does and how it works
-- TODO: if there's one column and no value for x0y0, call attention to typing stuff in


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



iframeView : ( Int, Int ) -> String -> Html Msg
iframeView coord url =
    div [ class [ Row ] ]
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
    div [ class [ ColumnGrid ] ]
        (List.append
            (List.indexedMap
                (\index col ->
                    span [class [ RowGrid ]]
                        (List.append
                            (List.map
                                (\xy -> iframeView xy (urlFor model xy))
                                col
                            )
                            [ div []
                                [ a
                                    [ onClick (LayoutChange (removeFromCol index model.layout))
                                    , class [ Round ]
                                    ]
                                    [ text "-" ]
                                , a
                                    [ onClick (LayoutChange (appendToCol index model.layout))
                                    , class [ Round ]
                                    ]
                                    [ text "+" ]
                                ]
                            ]
                        )
                )
                layout
            )
            [ div [ style [ ( "width", "25px" ) ] ]
                [ a
                    [ onClick (LayoutChange (List.take ((List.length model.layout) - 1) model.layout))
                    , class [ Round ]
                    ]
                    [ text "-" ]
                , a
                    [ onClick (LayoutChange (List.append model.layout [ 1 ]))
                    , class [ Round ]
                    ]
                    [ text "+" ]
                ]
            ]
        )


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ text Splitscreen.Style.compiled ]
        , layoutView model (modelToLayout model)
        ]
