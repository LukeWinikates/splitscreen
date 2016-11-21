module Splitscreen.Model exposing (..)
import List exposing (filter, head)
import Maybe exposing (withDefault)
import String exposing (split, dropLeft, startsWith)
import Http

valueFromQueryString : String -> String -> Maybe String
valueFromQueryString key queryString =
    queryString
        |> dropLeft 2 -- drop '#?'
        |> split "&"
        |> filter (\term -> startsWith (key ++ "=") term)
        |> head
        |> Maybe.map (dropLeft ((String.length key) + 1))
        |> Maybe.map Http.decodeUri
        |> Maybe.map (withDefault "")

fromUrl : String -> Model
fromUrl s =
    { newModel | first = valueFromQueryString "first" s, second = valueFromQueryString "second" s }


accumParam : String -> String -> Maybe String -> String
accumParam memo key value =
    memo ++ withDefault "" (Maybe.map (Http.encodeUri >> ((++) (key ++ "="))) value)


toUrl : Model -> String
toUrl model =
    "#?" |> \a -> accumParam a "first" model.first |> \a -> accumParam a "&second" model.second

type alias Model =
    { first : Maybe String
    , second : Maybe String
    }

newModel : Model
newModel =
    { first = Nothing, second = Nothing }
