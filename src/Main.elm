module DimedOut exposing (main)

import Browser
-- import Basics
import Regex
import List
import Http

import Html.Attributes as Attr
import Html.Events exposing (..)
import Html exposing (..)

import Json.Decode as Json
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
  , original : (List String)
  , hcrop : Int
  , vcrop : Int
  }

init : () -> (State, Cmd Msg)
init _ =
  ( State [] "" [] 0 0
  , getThumbs thumbsUrl
  )

type Msg
  = LoadThumbs
    | StartOver
    | BlankSlate
    | InsertThumb
    | InsertThumbs
    | SelectToggle
    | ShowMenu
    | HCropIncrease
    | HCropDecrease
    | VCropIncrease
    | VCropDecrease
    | NewThumbs (Result Http.Error (List String))

update : Msg -> State -> (State, Cmd Msg)
update msg state =
  case msg of
    LoadThumbs ->
      ( state
      , getThumbs thumbsUrl
      )

    StartOver -> ( { state | thumbs = (selectThumbs state.original)}, Cmd.none )
    BlankSlate -> ( { state | thumbs = []}, Cmd.none )

    SelectToggle -> ( state, Cmd.none )
    ShowMenu -> ( state, Cmd.none )
    InsertThumb -> ( state , Cmd.none )
    InsertThumbs -> ( state , Cmd.none )

    NewThumbs result ->
      case result of
        Ok newThumbs ->
          ( { state | original = newThumbs, thumbs = (selectThumbs newThumbs) }
          , Cmd.none
          )

        Err _ ->
          ( state
          , Cmd.none
          )

    -- todo enable/disable buttons when we are at an extreme
    -- TODO should we just hard-code the base image dimensions or what?
    HCropIncrease -> ( { state | hcrop = state.hcrop + 1 }, Cmd.none)
    HCropDecrease -> ( { state | hcrop = state.hcrop - 1 }, Cmd.none)
    VCropIncrease -> ( { state | vcrop = state.vcrop + 1 }, Cmd.none)
    VCropDecrease -> ( { state | vcrop = state.vcrop - 1 }, Cmd.none)

subscriptions : State -> Sub Msg
subscriptions state =
  Sub.none

view : State -> Html Msg
view state =
  div []
  [ h1 [] [ text "Dimed Out Studio - Quilt Designer 9000 G.O.L.D. Edition" ]
  , div [Attr.id "ctrl"]
    [ button [ onClick StartOver ] [ text "Reset to Original" ]
    , button [ onClick BlankSlate ] [ text "Remove All <<" ]
    , span [] [ text "hcrop"
      , button [ onClick HCropDecrease ] [ text "--" ]
      , button [ onClick HCropIncrease ] [ text "++" ]
      ]
    , span [] [ text "vcrop"
      , button [ onClick VCropDecrease ] [ text "--" ]
      , button [ onClick VCropIncrease ] [ text "++" ]
      ]
    ]
  , div [Attr.id "uls"]
    [ div []
      [ h2 [] [ text "Frames" ]
      , ul [ Attr.id "list" ] (
        -- try this:
        -- https://github.com/evancz/elm-todomvc/blob/07e3d4e5259f337d5eba781319b3a916e28aca99/src/Main.elm#L242
        List.map (\thumb -> li [ onClick InsertThumb, Attr.class (if thumb.selected then "sel" else "nosel"), Attr.id thumb.id ] [ text thumb.label ]) state.thumbs
        )
    ]
    , div []
      [ h2 [] [ text "Quilt" ]
      , ul [ Attr.id "thumbs" ] (
        List.map (\thumb -> li [ onClick SelectToggle, onMouseOver ShowMenu, Attr.id thumb.id ]
          (if thumb.selected then ([img [ Attr.src thumb.path, Attr.style "margin" (imgStyle state) ] []]) else [])) state.thumbs
        )
      ]
    ]
  , Html.node "link" [ Attr.rel "stylesheet", Attr.href "dimedout.css" ] []
  ]

imgStyle : State -> String
imgStyle s =
  (String.fromInt s.vcrop) ++ "px " ++ (String.fromInt s.hcrop) ++ "px"

getThumbs : String -> Cmd Msg
getThumbs file =
  Http.send NewThumbs (Http.get (Url.relative [ file ] []) thumbDecoder)

thumbDecoder : Json.Decoder (List String)
thumbDecoder =
  Json.list Json.string

thumbNameFromPath : String -> String
thumbNameFromPath path =
  case List.head (List.reverse (String.split "/" path)) of
    Nothing -> path
    Just name -> name

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

selectThumbs : List String -> List Thumb
selectThumbs thumbs =
  List.map (\t -> newThumb t) thumbs

onTargetedMouseOver : (String -> msg) -> Attribute msg
onTargetedMouseOver tagger =
  on "mouseover" (Json.map tagger targetValue)

