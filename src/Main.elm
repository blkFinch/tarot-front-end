module Main exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, int, string, float, field, list)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (class, style)

---defining api calls
corsAnywhere : String
corsAnywhere =
    "https://cors-anywhere.herokuapp.com/"

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
    {nhits : Int
    ,cards : List Card
    }

blankCard ={
    name = " Card Name"
    ,nameShort  = " "
    , value      = " "
    , valueInt   = 0
    , suit       = " "
    , cardType   = " "
    , meaningUp  = " "
    , meaningRev = " Your card's meaning here"
    , desc       = " A description of the card"}



init : ( Model, Cmd Msg )

init =
    (Model 0 [blankCard], Cmd.none )

---- REQUESTS/DECODERS ----

decodeCardList : Decode.Decoder Model
decodeCardList =
    Decode.succeed Model
        |> optional "nhits" int 20
        |> required "cards" (list decodeCard)

decodeCard : Decode.Decoder Card
decodeCard =
    Decode.succeed Card
        |> optional "name" string "Name not found"
        |> optional "name_short" string " "
        |> optional "value" string " "
        |> optional "value_int" int 9
        |> optional "suit" string " "
        |> optional "type" string " "
        |> optional "meaning_up" string " "
        |> optional "meaning_rev" string " "
        |> optional "desc" string "desc not found"

---- UPDATE ----
-- temp url to use
url : String
url =
    corsAnywhere ++ randomCardUrl

type Msg
    = RequestCard
    | DataRecieved(Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestCard ->
           (Model 7 [{blankCard | name = "card requested"}], Http.send DataRecieved (Http.getString url) )

        DataRecieved (Ok cardJson) ->
            case Decode.decodeString decodeCardList cardJson of
                Ok cards ->
                    (cards, Cmd.none )
                Err _->
                    (Model model.nhits [{blankCard | name = "err parsing: " ++ cardJson}], Cmd.none )

        DataRecieved(Err _) ->
            (Model 3 [blankCard], Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view model =
    div [class "container"][
      h1 [] [ text "Elm Tarot App" ]
      ,div[] (List.map cardList model.cards)
      ,button [class "btn btn-primary", onClick RequestCard][ text "Draw a Card!"]
    ]

cardList : Card -> Html Msg
cardList card =
    div[]
        [h3[][text card.name]
        ,p[][text card.desc]
        ,h4[][text "Meaning"]
        ,p[][text card.meaningUp]
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