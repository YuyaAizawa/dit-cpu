module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onChange)

import MomoRisc.Cpu as Cpu exposing (Cpu)
import MomoRisc.Inst as Inst exposing (Inst, ParseErr)
import MomoRisc.Memory as Memory exposing (Memory)
import MomoRisc.Dudit as Dudit exposing (Dudit)
import MomoRisc.Program as Program exposing (Program, DebugInfo, Errors, LineNum)


main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }



-- MODEL --

type alias Model =
  { cpu : Cpu
  , program : Program
  , memory : Memory
  , source : String
  , errors : Errors
  , debug : DebugInfo
  , lastCycle :
    { cpu : Cpu
    , memory : Memory
    }
  , tab : Tab
  }


type Tab
  = EditTab
  | CheckTab


init : Model
init =
  let
    ( program, debug, errors ) =
      Program.compile ""
  in
    { cpu = Cpu.init
    , program = program
    , memory = Memory.zeros
    , source = sampleCode
    , errors = errors
    , debug = debug
    , lastCycle =
      { cpu = Cpu.init
      , memory = Memory.zeros
      }
    , tab = EditTab
    }

sampleCode : String
sampleCode =
  """LDI B 10
ST A A
ADI A A 1
BLT A B 1
HLT
"""



-- UPDATE --

type Msg
  = SourceEdited String
  | TabClicked Tab
  | Step

update : Msg -> Model -> Model
update msg model =
  case msg of
    SourceEdited str ->
      { model | source = str }

    TabClicked tab ->
      if tab == model.tab then
        model
      else
        case tab of
          CheckTab ->
            let
              ( program, debug, errors ) =
                Program.compile model.source
            in
              { model
              | program = program
              , debug = debug
              , errors = errors
              , cpu = Cpu.init
              , memory = Memory.zeros
              , lastCycle =
                { cpu = Cpu.init
                , memory = Memory.zeros
                }
              , tab = tab
              }

          _ ->
            { model | tab = tab }

    Step ->
      let
        ( cpu, memory ) =
          model.cpu
            |> Cpu.step model.program model.memory
      in
        { model
        | cpu = cpu
        , memory = memory
        , lastCycle =
          { cpu = model.cpu
          , memory = model.memory
          }
        }


-- VIEW --

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "elm-erea"]
    [ Html.h2 [] [ Html.text "CPU Emulator" ]
    , Html.div [ Attr.id "emulator", Attr.class "section-container flex-container" ]
      [ Html.div [ Attr.class "program-pane" ]
          [ Html.h3 [] [ Html.text "Program" ]
          , sourceView model
          ]
      , Html.div [ Attr.class "data-pane" ]
        [ Html.h3 [] [ Html.text "Register" ]
        , Html.div [ Attr.class "registers" ]
          [ registerView "PC" model.cpu.pc model.lastCycle.cpu.pc
          , registerView "A" model.cpu.a model.lastCycle.cpu.a
          , registerView "B" model.cpu.b model.lastCycle.cpu.b
          , registerView "C" model.cpu.c model.lastCycle.cpu.c
          , registerView "D" model.cpu.d model.lastCycle.cpu.d
          ]
        , Html.h3 [] [ Html.text "Memory" ]
        , memoryView model.memory model.lastCycle.memory
        ]
      ]
    ]



-- PROGRAM PANE --

sourceView : Model -> Html Msg
sourceView model =
  let
    tabView tab text =
      Html.button
        [ Attr.class "tab"
        , ariaSelected (model.tab == tab)
        , onClick (TabClicked tab)
        ]
        [ Html.text text ]
  in
    Html.div [ Attr.class "tab-container" ]
      [ Html.div [ Attr.class "tab-nav" ]
        [ Html.div [ Attr.class "tabs" ]
          [ tabView EditTab "Edit"
          , tabView CheckTab "Check & Run"
          ]
        ]
      , Html.div [ Attr.class "tab-content" ]
          ( case model.tab of
            EditTab ->
              [ editView model.source ]
            CheckTab ->
              [ checkView model.source model.errors model.cpu
              , Html.button [ onClick Step ][ Html.text "Step" ]
              ]
          )
      ]


editView : String -> Html Msg
editView source =
  Html.textarea
    [ Attr.class "program"
    , onChange SourceEdited
    , Attr.value source
    , Attr.spellcheck False
    ]
    []


checkView : String -> Dict LineNum ParseErr -> Cpu -> Html msg
checkView source errors cpu =
  (source ++ "\n")
    |> String.lines
    |> List.indexedMap (\lineNum str -> lineView (lineNum == Dudit.toInt cpu.pc) str (Dict.get lineNum errors))
    |> Html.div [ Attr.class "program" ]


lineView : Bool -> String -> Maybe ParseErr -> Html msg
lineView cursor str maybeErr =
  let
    line =
      case maybeErr of
        Nothing ->
          [ Html.text str ]

        Just err ->
          case err of
            Inst.OpName _ ->
              wordErr 0 str

            Inst.ArgsLength _ _ ->
              [ errSpan str ]

            Inst.ArgsFormat idx ->
              wordErr idx str

    line_ =
      line
        |> List.intersperse (Html.text " ")

    line__ =
      if cursor then
        (Html.span [ Attr.class "cursor" ] [ Html.text "◤" ]) :: line_
      else
        line_

  in
    line__
      |> Html.div [ Attr.class "line" ]


wordErr : Int -> String -> List (Html msg)
wordErr i str =
  str
    |> String.words
    |> List.indexedMap (\j word ->
      if i == j then
        errSpan word
      else
        Html.text word)


errSpan : String -> Html msg
errSpan str =
  Html.span [ Attr.class "error" ] [ Html.text str ]


ariaSelected : Bool -> Html.Attribute msg
ariaSelected bool =
    Attr.attribute "aria-selected" (if bool then "true" else "false")



-- DATA PANE --

registerView : String -> Dudit -> Dudit -> Html msg
registerView label value prev =
  Html.div [ Attr.class "register" ]
    [ Html.div [ Attr.class "register-label" ] [ Html.text (label ++ ":") ]
    , Html.div [ Attr.class "register-value" ] [ highlightChange value prev ]
    ]


memoryView : Memory -> Memory -> Html msg
memoryView memory prev =
  Html.table [ Attr.class "memory" ]
    [ Html.thead [] [ headerRow ]
    , Html.tbody [] (( memory, prev ) |> Memory.to2dList |> List.indexedMap rowView)
    ]


headerRow : Html msg
headerRow =
  Html.tr []
    (Html.th [] [] :: (List.range 0 9 |> List.map intTh))


rowView : Int -> List ( Dudit, Dudit ) -> Html msg
rowView rowIdx row =
  let
    header =
      intTh (rowIdx * 10)

    cells =
      row |> List.map cellView
  in
    Html.tr [] (header :: cells)


intTh : Int -> Html msg
intTh int =
  Html.th [] [ Html.text <| String.fromInt <| int ]


cellView : ( Dudit, Dudit ) -> Html msg
cellView ( value, prev ) =
  Html.td [] [ highlightChange value prev ]


highlightChange : Dudit -> Dudit -> Html msg
highlightChange value prev =
  let
    inner = Html.text (Dudit.toString value)
  in
    if Dudit.eq value prev then
      inner
    else
      Html.span [ Attr.class "highlight" ] [ inner ]