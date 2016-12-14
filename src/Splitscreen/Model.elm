module Splitscreen.Model exposing (..)

import List exposing (filter, head)
import Maybe exposing (withDefault)
import String exposing (dropLeft, join, length, split, startsWith)
import Dict exposing (Dict, fromList, toList)
import Http


valueFromQueryString : String -> String -> Maybe String
valueFromQueryString key queryString =
    queryString
        |> dropLeft (length "#?")
        |> split "&"
        |> filter (\term -> startsWith (key ++ "=") term)
        |> head
        |> Maybe.map (dropLeft ((String.length key) + 1))
        |> Maybe.map Http.decodeUri
        |> Maybe.map (withDefault "")


parseLayout : Maybe String -> Maybe (List Int)
parseLayout =
    Maybe.map (String.toList >> List.map (String.fromChar >> String.toInt >> (Result.withDefault 0)))


tupleFromSplitting s =
    case split "=" s of
        [ a, b ] ->
            case Http.decodeUri b of
                Just url ->
                    ( a, url )

                _ ->
                    ( "", "" )

        _ ->
            ( "", "" )


parseUrls : String -> Dict String String
parseUrls queryString =
    queryString
        |> dropLeft (length "#?")
        |> split "&"
        |> filter (\term -> not (startsWith "layout=" term))
        |> List.map tupleFromSplitting
        |> fromList


fromUrl : String -> Model
fromUrl s =
    { layout = (withDefault []) <| parseLayout <| valueFromQueryString "layout" s
    , urls = parseUrls s
    }


accumParam : String -> String -> Maybe String -> String
accumParam memo key value =
    memo ++ withDefault "" (Maybe.map (Http.encodeUri >> ((++) (key ++ "="))) value)


encodeLayout : List Int -> String
encodeLayout =
    (List.map toString) >> String.join ""


toUrl : Model -> String
toUrl model =
    "#?layout="
        ++ (encodeLayout model.layout)
        ++ "&"
        ++ (join "&" <|
                List.map (\( pos, url ) -> pos ++ "=" ++ (Http.encodeUri url)) <|
                    (toList model.urls)
           )


type alias Model =
    { layout : Layout
    , urls : Dict String String
    }


newModel : Model
newModel =
    { layout = [], urls = Dict.empty }


modelToLayout : Model -> List (List ( Int, Int ))
modelToLayout model =
    List.indexedMap
        (\colNum rowCount -> List.map ((,) colNum) (List.range 0 (rowCount - 1)))
        model.layout


key : ( Int, Int ) -> String
key ( x, y ) =
    "x" ++ toString x ++ "y" ++ toString y


urlFor : Model -> ( Int, Int ) -> String
urlFor model xy =
    withDefault "" (Dict.get (key xy) model.urls)


mutateColumn : (Int -> Int) -> Int -> Layout -> Layout
mutateColumn transform columnIndex layout =
    (List.indexedMap
        (\index count ->
            if index == columnIndex then
                (transform count)
            else
                count
        )
        layout
    )


atColumnIndex : Int -> Layout -> Int
atColumnIndex columnIndex layout =
    List.drop columnIndex layout |> head |> withDefault 0


canAddRow : Int -> Layout -> Bool
canAddRow columnIndex layout =
    (atColumnIndex columnIndex layout) < 4


canRemoveRow : Int -> Layout -> Bool
canRemoveRow columnIndex layout =
    (atColumnIndex columnIndex layout) > 1


type alias Layout =
    List Int


type alias LayoutMutation =
    { predicate : Layout -> Bool, map : Layout -> Layout }


addColumn : LayoutMutation
addColumn =
    { predicate = \layout -> ((List.length layout) <= 4), map = \layout -> (List.append layout [ 1 ]) }


removeColumn : LayoutMutation
removeColumn =
    { predicate = \layout -> ((List.length layout) > 0), map = \layout -> (List.take ((List.length layout) - 1) layout) }


addRow : Int -> LayoutMutation
addRow columnIndex =
    { predicate = canAddRow columnIndex, map = (mutateColumn ((+) 1)) columnIndex }


removeRow : Int -> LayoutMutation
removeRow columnIndex =
    { predicate = canRemoveRow columnIndex, map = mutateColumn ((+) -1) columnIndex }
