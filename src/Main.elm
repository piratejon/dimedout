module DimedOut exposing (main)

import Browser
import Regex
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
  , id : String
  }

type alias State =
  { thumbs : (List Thumb)
  , hovered : String
  }

init : () -> (State, Cmd Msg)
init _ =
  ( State [] ""
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
  , div [Attr.id "ctrl"]
    [ button [ onClick InsertAllThumbs ] [ text "Insert All >>" ]
    , button [ onClick RemoveAllThumbs ] [ text "Remove All <<" ]
    ]
  , div [Attr.id "uls"]
    [ ul [ Attr.id "list" ] (
      List.map (\thumb -> li [ onClick InsertThumb, Attr.class (if thumb.selected then "sel" else "nosel"), Attr.id thumb.id ] [ text thumb.label ]) state.thumbs
      )
    , ul [ Attr.id "thumbs" ] (
      List.map (\thumb -> li [ onClick SelectToggle, onMouseOver ShowMenu, Attr.id thumb.id ] (if thumb.selected then ([img [ Attr.src thumb.path ] []]) else [])) state.thumbs
      )
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

-- makeId = Regex.replace Regex.Any (Regex.regex "[^a-zA-Z0-9]") (\_ -> "")
userReplace : String -> (Regex.Match -> String) -> String -> String
userReplace userRegex replacer string =
  case Regex.fromString userRegex of
    Nothing -> string
    Just regex -> Regex.replace regex replacer string

makeId : String -> String
makeId string =
  userReplace "[^A-Za-z0-9]" (\_ -> "") string

newThumb : String -> Thumb
newThumb path =
  {label=(thumbNameFromPath path), path=path, selected=True, id=(makeId path)}

setThumbsSelected : List Thumb -> Bool -> List Thumb
setThumbsSelected thumbs selected =
  List.map (\t -> {path=t.path, label=t.label, selected=selected, id=(makeId t.path)}) thumbs

