module DimedOut exposing (main)

import Browser
-- import Basics
import Regex
import List
import Http

import Html.Attributes as Attr
import Html.Events exposing (..)
import Html exposing (..)

import Char exposing (toCode)

import Json.Decode as Json
import Url.Builder as Url

import Hex

thumbsUrl = "../thumbs.json"

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Thumb =
  { id : String -- id derived from path
  , path : String -- relative url to image
  , label : String -- displayed in list to left
  , visible : Bool -- whether this tile appears in the quilt
  , selected : Bool -- this tile is checkmarked in the quilt
  }

type alias State =
  { thumbs : (List Thumb)
  , hovered : String
  , hcrop : Int
  , vcrop : Int
  }

init : () -> (State, Cmd Msg)
init _ =
  ( State [] "" 0 0
  , getThumbs thumbsUrl
  )

type Msg
  = LoadThumbs
    | StartOver
    | RemoveUnselected
    | BlankSlate
    | InsertThumb
    | InsertThumbs
    | SelectToggle
    | ShowMenu String
    | HCropIncrease
    | HCropDecrease
    | VCropIncrease
    | VCropDecrease
    | InsertLeft String
    | RemoveThumb String
    | ToggleThumb String
    | InsertRight String
    | NewThumbs (Result Http.Error (List String))

update : Msg -> State -> (State, Cmd Msg)
update msg state =
  case msg of
    LoadThumbs ->
      ( state
      , getThumbs thumbsUrl
      )

    StartOver -> ( { state | thumbs = (List.map (\t -> {t | visible=True}) state.thumbs)}, Cmd.none )
    RemoveUnselected -> ( { state | thumbs = (List.filter (\t -> t.selected) state.thumbs) }, Cmd.none )
    BlankSlate -> ( { state | thumbs = (List.map (\t -> {t | visible=False}) state.thumbs)}, Cmd.none )

    SelectToggle -> ( state, Cmd.none )
    ShowMenu id ->
      -- let _ = Debug.log "id" id in
      ( { state | hovered = id }, Cmd.none )
    InsertThumb -> ( state , Cmd.none )
    InsertThumbs -> ( state , Cmd.none )

    NewThumbs result ->
      case result of
        Ok newThumbs ->
          ( { state | thumbs = (selectAllThumbs newThumbs) }
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

    InsertLeft _ -> (state, Cmd.none)
    RemoveThumb id ->
      let _ = Debug.log "remove id" id in
      ({state | thumbs = (List.filter (\t -> t.id /= id) state.thumbs) }, Cmd.none)
    ToggleThumb id ->
      let _ = Debug.log "toggle id" id in
      ({state | thumbs = (List.map (\t -> if t.id == id then {t | selected = (not t.selected)} else t) state.thumbs) }, Cmd.none)
    InsertRight _ -> (state, Cmd.none)

subscriptions : State -> Sub Msg
subscriptions state =
  Sub.none

view : State -> Html Msg
view state =
  div []
  [ h1 [] [ text "Dimed Out Studio - Quilt Designer 9000 QDOTY Edition", em [] [text "Deluxe"] ]
  , div [Attr.id "ctrl"]
    [ button [ onClick StartOver ] [ text "Reset to Original" ]
    , button [ onClick BlankSlate ] [ text "Remove All <<" ]
    , button [ onClick RemoveUnselected ] [ text "Remove Unselected" ]
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
        List.map (\t -> li
          [ onClick InsertThumb
          , Attr.class ((if t.visible then "visy" else "visn") ++ " " ++ (if t.selected then "sely" else "seln"))
          , Attr.id ("thumb" ++ t.id)] [ text t.label ]) state.thumbs
        )
    ]
    , div []
      [ h2 [] [ text "Quilt" ]
      , ul [ Attr.id "thumbs" ] (generateThumbnailLI state)
      ]
    ]
  , Html.node "link" [ Attr.rel "stylesheet", Attr.href "dimedout.css" ] []
  ]

generateThumbnailLI : State -> (List (Html Msg))
generateThumbnailLI state = List.map (\t -> li
    [ onClick SelectToggle
    , Attr.id t.id
    , Attr.class (if state.hovered == t.id then "hovered" else "")
    ]
    (if t.visible && state.hovered == t.id then (
      [ img [ onTargetedMouseOver ShowMenu, Attr.src t.path, Attr.style "margin" (imgStyle state) ] []
        , div [ Attr.id "thumbctrl" ]
          [ span []
            [ span [onTargetedClick InsertLeft (eventAncestorId 3)] [text "<+"]
            , span [onTargetedClick ToggleThumb (eventAncestorId 3), Attr.class (if t.selected then "sel sely" else "sel seln")] [text "✔"]
            , span [onTargetedClick RemoveThumb (eventAncestorId 3)] [text "X"]
            , span [onTargetedClick InsertRight (eventAncestorId 3)] [text "+>"]
            ]
          ]
        ]
        ) else if t.visible then ( [ img [ onTargetedMouseOver ShowMenu, Attr.src t.path, Attr.style "margin" (imgStyle state) ] []]
      ) else [])
  ) state.thumbs

-- newThumbnailLI : State -> (List (Html Msg))

imgStyle : State -> String
imgStyle s =
  (String.fromInt s.vcrop) ++ "px " ++ (String.fromInt s.hcrop) ++ "px"

getThumbs : String -> Cmd Msg
getThumbs file =
  Http.send NewThumbs (Http.get (Url.relative [ file ] []) thumbDecoder)

thumbDecoder : Json.Decoder (List String)
thumbDecoder =
  Json.list Json.string

-- filename component of path
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

idFromPath: String -> String
idFromPath path =
  "id" ++ String.concat (List.map (\c ->
    let ord = (toCode c) in
        if ((ord >= 0x41 && ord <= 0x5A)
        || (ord >= 0x61 && ord <= 0x7A)
        || (ord >= 0x30 && ord <= 0x39)) then (String.fromChar c)
        else ("_" ++ (Hex.toString ord))
    ) (String.toList path))

newThumb : String -> Thumb
newThumb path =
  {label=(thumbNameFromPath path), path=path, visible=True, id=(idFromPath path), selected=False}

selectAllThumbs : List String -> List Thumb
selectAllThumbs thumbs =
  List.map (\t -> newThumb t) thumbs

eventAncestorId : Int -> (List String)
eventAncestorId n =
  ["target"] ++ (List.repeat n "parentElement") ++ ["id"]

onTargetedClick : (String -> msg) -> (List String) -> Attribute msg
onTargetedClick tagger attrs =
  on "click" (Json.map tagger (Json.at attrs Json.string))

onTargetedMouseOver : (String -> msg) -> Attribute msg
onTargetedMouseOver tagger =
  on "mouseover" (Json.map tagger (Json.at (eventAncestorId 1) Json.string))

