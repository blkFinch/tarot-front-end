module Main exposing (..)
import Http
import Json.Decode as Decode
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (class, style)

---defining api calls
randomCardUrl : String
randomCardUrl =
    "https://rws-cards-api.herokuapp.com/api/v1/cards/random"

---- MODEL ----


-- type alias Card =
--     { name : String
--     , suit : String
--     , desc : String
--     }

type alias Model =
    {
        name : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model [], Cmd.none )

---- REQUESTS/DECODERS ----
cardDecoder : Decode.Decoder Model
cardDecoder =
    Decode.at ["cards"] Decode.string

---- UPDATE ----
-- temp url to use
url : String
url =
    "http://localhost:5019/nicknames"

type Msg
    = RequestCard
    | DataRecieved(Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestCard ->
           (model, Http.send DataRecieved (Http.getString url) )

        DataRecieved (Ok cardJson) ->
            case Decode.decodeString cardDecoder cardJson of
                Ok card ->
                    ({model | names = card}, Cmd.none )
                Err errorMsg ->
                    (Model [], Cmd.none)

        DataRecieved(Err _) ->
            (model, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view model =
    div [class "container"][
      h1 [] [ text "Elm Tarot App" ]
      ,button [class "btn btn-primary", onClick RequestCard][ text "Draw a Card!"]
      ,p[](List.map nameList model.names)
    ]

nameList : String -> Html Msg
nameList name =
    p[][text name]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }