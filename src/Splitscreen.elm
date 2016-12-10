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


type alias GridPosition =
    ( Int, Int )


iframeView : ( Int, Int ) -> String -> Html Msg
iframeView gridPosition url =
    div [ class [ Row ] ]
        [ iframe [ src url, class [ UrlContent ] ] []
        , input
            [ class [ ShowOnHover, UrlBar ]
            , placeholder "type a url here..."
            , value url
            , onInput (gridPosition |> key |> Change)
            ]
            []
        ]


type alias GridLayout =
    List (List GridPosition)


rowViews : Model -> List GridPosition -> List (Html Msg)
rowViews model positions =
    List.map
        (\gridPosition -> iframeView gridPosition (urlFor model gridPosition))
        positions


columnViews : Model -> GridLayout -> List (Html Msg)
columnViews model layout =
    (List.indexedMap
        (\columnIndex positionsForColumn ->
            span [ class [ RowGrid ] ]
                (List.append
                    (rowViews model positionsForColumn)
                    [ div []
                        [ a
                            [ onClick (LayoutChange (removeFromCol columnIndex model.layout))
                            , class [ Round ]
                            ]
                            [ text "-" ]
                        , a
                            [ onClick (LayoutChange (appendToCol columnIndex model.layout))
                            , class [ Round ]
                            ]
                            [ text "+" ]
                        ]
                    ]
                )
        )
        layout
    )


gridView : Model -> GridLayout -> Html Msg
gridView model layout =
    div [ class [ ColumnGrid ] ]
        (List.append
            (columnViews model layout)
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
        , gridView model (modelToLayout model)
        ]
