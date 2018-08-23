module DimedOut exposing (main)

import Browser
import List
import Http

import Html.Attributes as Attr
import Html.Events exposing (..)
import Html exposing (..)

import Json.Decode as Decode
import Url.Builder as Url

thumbsUrl = "../thumbs.json"

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias State =
  { thumbs : (List String)
  , selected : (List String)
  }

init : () -> (State, Cmd Msg)
init _ =
  ( State ([]) ([])
  , getThumbs thumbsUrl
  )

type Msg
  = LoadThumbs
    | InsertThumb
    | InsertThumbs
    | Highlight
    | ShowMenu
    | NewThumbs (Result Http.Error (List String))

update : Msg -> State -> (State, Cmd Msg)
update msg state =
  case msg of
    LoadThumbs ->
      ( state
      , getThumbs thumbsUrl
      )

    Highlight -> ( state, Cmd.none )
    ShowMenu -> ( state, Cmd.none )
    InsertThumb -> ( state , Cmd.none)
    InsertThumbs -> ( state , Cmd.none)

    NewThumbs result ->
      case result of
        Ok newThumbs ->
          ( { state | thumbs = newThumbs }
          , Cmd.none
          )

        Err _ ->
          ( state
          , Cmd.none
          )

subscriptions : State -> Sub Msg
subscriptions state =
  Sub.none

view : State -> Html Msg
view state =
  div []
    [ h2 [] [ text "Dimed Out Studio" ]
    , span []
      [ input [ Attr.placeholder "% to Load"
              , Attr.type_ "number"
              , Attr.size 5 ] []
      , button [ onClick InsertThumbs ] [ text "Insert Thumbs >>" ]
      ]
    , ul [ Attr.id "unsel" ] (List.map (\thumb -> li [ onClick InsertThumb ] [ text thumb ]) state.thumbs)
    , ul [ Attr.id "sel" ] (List.map (\thumb -> li [ onClick Highlight, onMouseEnter ShowMenu ] [ img [ Attr.src thumb ] [] ]) state.selected)
    ]

getThumbs : String -> Cmd Msg
getThumbs file =
  Http.send NewThumbs (Http.get (Url.relative [ file ] []) thumbDecoder)

thumbDecoder : Decode.Decoder (List String)
thumbDecoder =
  Decode.list Decode.string
