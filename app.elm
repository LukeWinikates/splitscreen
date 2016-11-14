import Html exposing (Html, Attribute, div, input, text, button, form, iframe, header, body)
import Html.App as App
import Html.Attributes exposing (placeholder, src, style)
import Html.Events exposing (onInput, onClick, onSubmit)
import String exposing (split, dropLeft, startsWith)
import Navigation
import List exposing (filter, head)
import Maybe exposing (withDefault)

pagesFromQueryString : String -> Maybe String
pagesFromQueryString s =
  s
    |> dropLeft 2 -- drop '#?'
    |> split "&"
    |> filter (\term -> startsWith "pages=" term)
    |> head
    |> Maybe.map (dropLeft 6) -- drop 'foo='

main =
  Navigation.program urlParser
  {
  init = init
     ,view = view
    , update = update
    , urlUpdate = urlUpdate
    , subscriptions = subscriptions
    }


init : Result String Model -> (Model, Cmd Msg)
init result =
    let _ = (Debug.log "pages" model.pages) in
        urlUpdate result model

fromUrl : String -> Result String Model
fromUrl s =
    case pagesFromQueryString s of
     Nothing ->  Ok model
     Just pages -> Ok { model | pages = String.split "," pages }

toUrl : Model -> String
toUrl model =
  case model.pages of
    [] -> "#"
    _ -> "#?pages=" ++  String.join "," model.pages

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

subscriptions model =
  Sub.none

type alias Model =
  { pages : List String,
    currentText : String
  }

model : Model
model =
  { pages = [], currentText = "" }

-- UPDATE

type Msg
  = Change String
  | Close String
  | Add

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
 let
    newModel =
      case msg of
        Change newContent ->
          { model | currentText = newContent}
        Add ->
          { model | currentText = "", pages = model.currentText :: model.pages }
        Close url ->
          { model | pages = List.filter ((/=) url) model.pages }
  in
      (newModel, Navigation.newUrl (toUrl newModel))

-- VIEW

iframeView : String -> Html Msg
iframeView url =
    div [] [
        header [onClick (Close url)] [text "x"]
        ,iframe [src url, style [("height", "calc(100vh )"), ("width", "calc(100vw)"), ("overflow", "hidden") ]] []
    ]

view : Model -> Html Msg
view model =
  body [style [("overflow", "hidden")]] [
  form [onSubmit Add, style [("overflow", "hidden")]]
    [ input [ placeholder "Page To Add", onInput Change ] [text model.currentText]
      , button [] [text "Add"]
    , div [] (List.map iframeView model.pages)
    ]]