module Main exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, int, string, float)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (class, style)

---defining api calls
randomCardUrl : String
randomCardUrl =
    "https://rws-cards-api.herokuapp.com/api/v1/cards/random"

---- MODEL ----


type alias Card =
    { name       : String
    , nameShort  : String
    , value      : String
    , valueInt   : Int
    , suit       : String
    , cardType   : String
    , meaningUp  : String
    , meaningRev : String
    , desc       : String
    }

type alias Model =
    Card

blankCard ={
    name = ""
    ,nameShort  = " "
    , value      = " "
    , valueInt   = 0
    , suit       = " "
    , cardType   = " "
    , meaningUp  = " "
    , meaningRev = " "
    , desc       = " "}


init : ( Model, Cmd Msg )

init =
    (blankCard, Cmd.none )

---- REQUESTS/DECODERS ----
decodeCard : Decode.Decoder Card
decodeCard =
    Decode.succeed Card
        |> required "name" string
        |> required "name_short" string
        |> required "value" string
        |> required "value_int" int
        |> required "suit" string
        |> required "type" string
        |> required "meaning_up" string
        |> required "meaning_rev" string
        |> required "desc" string

---- UPDATE ----
-- temp url to use
url : String
url =
    "http://localhost:3000/data"

type Msg
    = RequestCard
    | DataRecieved(Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestCard ->
           (model, Http.send DataRecieved (Http.getString url) )

        DataRecieved (Ok cardJson) ->
            case Decode.decodeString decodeCard cardJson of
                Ok card ->
                    (card, Cmd.none )
                Err _->
                    (model, Cmd.none )

        DataRecieved(Err _) ->
            (model, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view model =
    div [class "container"][
      h1 [] [ text "Elm Tarot App" ]
      ,button [class "btn btn-primary", onClick RequestCard][ text "Draw a Card!"]
      ,p[][text model.name]
      ,p[][text model.desc]
      ,p[][text model.meaningUp]
    ]


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }