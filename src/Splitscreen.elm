module Splitscreen exposing (..)

import Html exposing (..)
import Html.Attributes exposing (disabled, href, placeholder, src, style, value)
import Html.Events exposing (onInput, onClick, onSubmit, onMouseOver)
import Navigation
import List exposing (filter, head)
import Maybe exposing (withDefault)
import Dict exposing (toList)
import Css.Namespace exposing (namespace)
import Css.Helpers
import Html.CssHelpers
import Splitscreen.Model exposing (..)
import Splitscreen.Style exposing (..)


-- TODO: give visual feedback about disabled buttons
-- TODO: provide some entertaining getting-started layouts

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


circleButton : String -> List ( String, String ) -> LayoutMutation -> Layout -> Html Msg
circleButton icon styles mutation layout =
    a
        [ style styles
        , onClick
            (LayoutChange
                (if (mutation.predicate layout) then
                    (mutation.map layout)
                 else
                    layout
                )
            )
        , class
            [ Round
            , if mutation.predicate layout then
                Disabled
              else
                Enabled
            ]
        ]
        [ text icon ]


addButton =
    circleButton "+"


removeButton =
    circleButton "-"


rowViews : Model -> List GridPosition -> List (Html Msg)
rowViews model positions =
    List.map
        (\gridPosition -> iframeView gridPosition (urlFor model gridPosition))
        positions


columnViews : Model -> GridLayout -> List (Html Msg)
columnViews model layout =
    case layout of
        [] ->
            [ basicTutorial ]

        _ ->
            (List.indexedMap
                (\columnIndex positionsForColumn ->
                    span [ class [ RowGrid ] ]
                        (List.append
                            (rowViews model positionsForColumn)
                            [ div []
                                [ removeButton [] (removeRow columnIndex) model.layout
                                , addButton [] (addRow columnIndex) model.layout
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
            [ div []
                [ removeButton [ ( "display", "block" ) ] removeColumn model.layout
                , addButton [ ( "display", "block" ) ] addColumn model.layout
                ]
            ]
        )


basicTutorial : Html Msg
basicTutorial =
    div [ class [ RowGrid, Prose ] ]
        [ h1 [] [ text "Splitscreen" ]
        , div [] [ text "Show multiple information radiator pages in one browser tab" ]
        , div [] [ text "or make a mosaic of images, gifs, or static content" ]
        , p [] [ text "Click the (+) button on the right to add a column with one row" ]
          --        , p []
          --            [ span [] [ text "or try these examples: " ]
          --            , a [] [ text "" ]
          --            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ text Splitscreen.Style.compiled ]
        , gridView model (modelToLayout model)
        ]
