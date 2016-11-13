import Html exposing (Html, Attribute, div, input, text, button, form, iframe, header)
import Html.App as App
import Html.Attributes exposing (placeholder, src, style)
import Html.Events exposing (onInput, onClick, onSubmit)
import String
import Navigation

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
init result = urlUpdate result model

fromUrl : String -> Result String Model
fromUrl s = Ok model

toUrl : Model -> String
toUrl model =
  "#/" ++ String.join "," model.pages

urlParser : Navigation.Parser (Result String Model)
urlParser =
  Navigation.makeParser (fromUrl << .hash)

urlUpdate : Result String Model -> Model -> (Model, Cmd Msg)
urlUpdate result model = (model, Cmd.none)

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
        ,iframe [src url, style [("height", "calc(100vh - 20px)"), ("width", "calc(200vw - 20px)") ]] []
    ]

view : Model -> Html Msg
view model =
  form [onSubmit Add]
    [ input [ placeholder "Page To Add", onInput Change ] [text model.currentText]
      , button [] [text "Add"]
    , div [] (List.map iframeView model.pages)
    ]