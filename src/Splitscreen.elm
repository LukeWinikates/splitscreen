module Splitscreen exposing (..)

import Html exposing (Attribute, Html, a, body, button, div, form, h1, header, iframe, input, li, node, span, text, textarea, ul)
import Html.Attributes exposing (class, href, placeholder, src, style, value)
import Html.Events exposing (onInput, onClick, onSubmit, onMouseOver)
import Navigation
import List exposing (filter, head)
import Maybe exposing (withDefault)
import Dict exposing (toList)
import Splitscreen.Model exposing (Model, fromUrl, toUrl)


-- TODO: test onload handler
-- TODO: add actual generic layout handling, math
-- TODO: make code generally cleaner
-- TODO: add buttons that add panels to the layout
-- TODO: can't scroll -- make the textbox smaller (e.g. like an address bar?)

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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change key newContent ->
            let
                newModel =
                    { model | urls = Dict.insert key newContent model.urls }
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
            [ ( "height", "calc(100vh - 20px)" ), ( "border", "none" ), ("width", "100%")]
    in
        div [ style [( "position", "relative" ) ] ]
            [ iframe [ src url, style positioning ] []
            , input
                [ class "show-on-hover"
                , placeholder "type a url here..."
                , value url
                , onInput (Change key)
                , style (List.append [ ( "position", "absolute" ), ( "top", "0" ), ( "left", "0" ), ( "text-align", "center" ), ( "font-size", "24pt" ) ] positioning)
                ]
                []
            ]


columnView : Model -> List (List ( Int, Int )) -> Html Msg
columnView model layout =
    div [style [( "display", "flex" ), ( "border-right", "1px solid white" ), ("width", "calc(100vw - 10px)") ]]
        (List.map
            (\col ->
                span [style [("flex-grow", "1")]]
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
            )
            layout
        )


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ text ".show-on-hover { transition: all 1s; background-color: transparent; color: transparent; }\n         .show-on-hover:hover { background-color: #ccc; color: #111 }" ]
        , columnView model (modelToLayout model)
        ]
