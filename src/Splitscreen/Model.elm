module Splitscreen.Model exposing (..)

import List exposing (filter, head)
import Maybe exposing (withDefault)
import String exposing (dropLeft, join, length, split, startsWith)
import Dict exposing (Dict, fromList, toList)
import Http


valueFromQueryString : String -> String -> String
valueFromQueryString key queryString =
    queryString
        |> dropLeft (length "#?")
        |> split "&"
        |> filter (\term -> startsWith (key ++ "=") term)
        |> head
        |> Maybe.map (dropLeft ((String.length key) + 1))
        |> Maybe.map Http.decodeUri
        |> Maybe.map (withDefault "") -- TODO: remove these withdefaults and let it be a maybe?, then have fromUrl do the defaulting
        |> withDefault "11"


parseLayout : String -> List Int
parseLayout =
    String.toList >> List.map (String.fromChar >> String.toInt >> (Result.withDefault 0))


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
    { layout = parseLayout <| valueFromQueryString "layout" s
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
    { layout : List Int
    , urls : Dict String String
    }


newModel : Model
newModel =
    { layout = [ 1, 1 ], urls = Dict.empty }

--type alias Layout = List List (Int, Int)


modelToLayout : Model -> List (List ( Int, Int ))
modelToLayout model =
    List.indexedMap
        (\colNum rowCount -> List.map ((,) colNum) (List.range 0 (rowCount - 1)))
        model.layout

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

removeFromCol column modelLayout =
    List.indexedMap
        (\index count ->
            count
                + if index == column then
                    -1
                  else
                    0
        )
        modelLayout
