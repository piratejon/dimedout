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

type alias Thumb =
  { path : String
  , label : String
  , selected : Bool
  }

type alias State =
  { thumbs : (List Thumb)
  }

init : () -> (State, Cmd Msg)
init _ =
  ( State ([])
  , getThumbs thumbsUrl
  )

type Msg
  = LoadThumbs
    | InsertAllThumbs
    | RemoveAllThumbs
    | InsertThumb
    | InsertThumbs
    | SelectToggle
    | ShowMenu
    | NewThumbs (Result Http.Error (List String))

update : Msg -> State -> (State, Cmd Msg)
update msg state =
  case msg of
    LoadThumbs ->
      ( state
      , getThumbs thumbsUrl
      )

    InsertAllThumbs -> ( { state | thumbs = (setThumbsSelected state.thumbs True)}, Cmd.none )
    RemoveAllThumbs -> ( { state | thumbs = (setThumbsSelected state.thumbs False)}, Cmd.none )

    SelectToggle -> ( state, Cmd.none )
    ShowMenu -> ( state, Cmd.none )
    InsertThumb -> ( state , Cmd.none )
    InsertThumbs -> ( state , Cmd.none )

    NewThumbs result ->
      case result of
        Ok newThumbs ->
          ( { state | thumbs = (List.map newThumb newThumbs) }
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
  [ h2 [] [ text "Dimed Out Studio - Quilt Designer 9000 Edition" ]
  {-
  , span []
    [ input [ Attr.placeholder "% to Load"
            , Attr.type_ "number"
            , Attr.size 5 ] []
    , button [ onClick InsertThumbs ] [ text "Insert Thumbs >>" ]
    ]
  -}
  , div [Attr.id "ctrl"]
    [ button [ onClick InsertAllThumbs ] [ text "Insert All >>" ]
    , button [ onClick RemoveAllThumbs ] [ text "Remove All <<" ]
    ]
  , div [Attr.id "uls"]
    [ ul [ Attr.id "list" ] (List.map (\thumb -> li [ onClick InsertThumb, Attr.class (if thumb.selected then "sel" else "nosel") ] [ text thumb.label ]) state.thumbs)
    , ul [ Attr.id "thumbs" ] (List.map (\thumb -> li [ onClick SelectToggle, onMouseEnter ShowMenu ] (if thumb.selected then ([img [ Attr.src thumb.path ] []]) else [])) state.thumbs)
    ]
  , Html.node "link" [ Attr.rel "stylesheet", Attr.href "dimedout.css" ] []
  ]

getThumbs : String -> Cmd Msg
getThumbs file =
  Http.send NewThumbs (Http.get (Url.relative [ file ] []) thumbDecoder)

thumbDecoder : Decode.Decoder (List String)
thumbDecoder =
  Decode.list Decode.string

thumbNameFromPath : String -> String
thumbNameFromPath path =
  case List.head (List.reverse (String.split "/" path)) of
    Nothing -> path
    Just name -> name

newThumb : String -> Thumb
newThumb path =
  {label=(thumbNameFromPath path), path=path, selected=True}

setThumbsSelected : List Thumb -> Bool -> List Thumb
setThumbsSelected thumbs selected =
  List.map (\t -> {path=t.path, label=t.label, selected=selected}) thumbs

