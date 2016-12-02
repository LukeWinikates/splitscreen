module Splitscreen.Model exposing (..)

import List exposing (filter, head)
import Maybe exposing (withDefault)
import String exposing (split, dropLeft, startsWith, join)
import Dict exposing (Dict, fromList, toList)
import Http


valueFromQueryString : String -> String -> String
valueFromQueryString key queryString =
    queryString
        |> dropLeft 2 -- drop '#?'
        |> split "&"
        |> filter (\term -> startsWith (key ++ "=") term)
        |> head
        |> Maybe.map (dropLeft ((String.length key) + 1))
        |> Maybe.map Http.decodeUri
        |> Maybe.map (withDefault "")
        |> withDefault ""


parseLayout : String -> { x : Int, y : Int }
parseLayout _ =
    { x = 2, y = 1 }

tupleFromSplitting s =
    case split "=" s of
        [a, b] -> case Http.decodeUri b of
           Just url -> (a, url)
           _ -> ("", "")
        _ -> ("", "")

parseUrls : String -> Dict String String
parseUrls queryString =
    queryString
        |> dropLeft 2 -- drop '#?'
        |> split "&"
        |> filter (\term -> not (startsWith "layout=" term))
        |> Debug.log "banana"
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


toUrl : Model -> String
toUrl model =
    "#?layout=x2y1&" ++ (join "&" <|
     List.map (\(pos, url) -> pos ++ "=" ++ (Http.encodeUri url)) <|
    (toList model.urls))


type alias Model =
    { layout : { x : Int, y : Int }
    , urls : Dict String String
    }


newModel : Model
newModel =
    { layout = { x = 1, y = 2 }, urls = Dict.empty }
