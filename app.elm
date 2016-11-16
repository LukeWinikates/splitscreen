import Html exposing (Attribute, Html, a, body, button, div, form, h1, header, iframe, input, li, node, span, text, textarea, ul)
import Html.App as App
import Html.Attributes exposing (class, href, placeholder, src, style, value)
import Html.Events exposing (onInput, onClick, onSubmit, onMouseOver)
import String exposing (split, dropLeft, startsWith)
import Navigation
import List exposing (filter, head)
import Maybe exposing (withDefault)
import Time exposing (Time, millisecond, second, every)
import Mouse exposing (Position)
import Http

valueFromQueryString : String -> String -> Maybe String
valueFromQueryString key queryString =
  queryString
    |> dropLeft 2 -- drop '#?'
    |> split "&"
    |> filter (\term -> startsWith (key++"=") term)
    |> head
    |> Maybe.map (dropLeft ((String.length key) + 1))
    |> Maybe.map Http.uriDecode

main =
  Navigation.program urlParser
  {
  init = init
     ,view = view
    , update = update
    , urlUpdate = urlUpdate
    , subscriptions = always Sub.none
    }


init : Result String Model -> (Model, Cmd Msg)
init result =
        urlUpdate result newModel

fromUrl : String -> Result String Model
fromUrl s =
    Ok { newModel | first = valueFromQueryString "first" s, second =  valueFromQueryString "second" s }

accumParam : String -> String -> Maybe String -> String
accumParam memo key value =
    memo ++ withDefault "" (Maybe.map (Http.uriEncode >> ((++) (key ++ "="))) value)

toUrl : Model -> String
toUrl model =
   "#?" |> \a -> accumParam a "first" model.first |> \a -> accumParam a "&second" model.second

urlParser : Navigation.Parser (Result String Model)
urlParser =
  Navigation.makeParser (fromUrl << .hash)

urlUpdate : Result String Model -> Model -> (Model, Cmd Msg)
urlUpdate result model =
  case result of
    Ok newModel ->
      (newModel, Cmd.none)
    Err _ ->
      (model, Navigation.modifyUrl (toUrl model))

type alias Model =
  { first : Maybe String,
    second : Maybe String
  }

newModel : Model
newModel =
  { first = Nothing, second = Nothing  }

-- UPDATE

type Msg
  = ChangeFirst String
  | ChangeSecond String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
 let
    newModel =
      case msg of
        ChangeFirst newContent ->
          { model | first = Just newContent }
        ChangeSecond newContent ->
          { model | second = Just newContent }
  in
      (newModel, Navigation.newUrl (toUrl newModel))

-- VIEW

iframeView : (String -> Msg) -> String -> Html Msg
iframeView f url =
    let positioning = [("height", "calc(100vh)"), ("width", "calc(50vw - 1px)"), ("border", "none"), ("border-right", "1px solid white")] in
        div [style [("display", "inline-block"), ("position", "relative") ]] [
            iframe [src url, style positioning] []
            ,input [class "show-on-hover"
                , placeholder "type a url here..."
            , value url
            ,onInput f
                , style (List.append [("position", "absolute"), ("top", "0"), ("left", "0"), ("text-align", "center"), ("font-size", "24pt")] positioning)]
                    []
        ]

view : Model -> Html Msg
view model =

       div [] [
        node "style" [] [text ".show-on-hover { transition: all 1s; background-color: transparent; color: transparent; }
         .show-on-hover:hover { background-color: #ccc; color: #111 }"]
        , iframeView ChangeFirst (withDefault "" model.first)
        ,iframeView ChangeSecond (withDefault "" model.second)
       ]
