module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onChange)

import MomoRisc.Cpu as Cpu exposing (Cpu, MemoryAccessPhaseAction(..))
import MomoRisc.Device as Device exposing (Chario)
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



-----------
-- MODEL --
-----------

type alias Model =
  { cpu : Cpu
  , program : Program
  , memory : Memory
  , chario : Chario
  , source : String
  , errors : Errors
  , debug : DebugInfo
  , lastCycle : Cpu
  , wroteAddr : Maybe Dudit
  , tab : Tab
  }


type Tab
  = EditTab
  | CheckAndRunTab


init : Model
init =
  let
    ( program, debug, errors ) =
      Program.compile sampleCode
  in
    { cpu = Cpu.init
    , program = program
    , memory = Memory.zeros
    , chario = Device.charioInit |> Device.setInput sampleInput
    , source = sampleCode
    , errors = errors
    , debug = debug
    , lastCycle = Cpu.init
    , wroteAddr = Nothing
    , tab = CheckAndRunTab
    }


sampleCode : String
sampleCode =
  """; Read the input and output it
; in reverse order.
LDI B 91
LDI C 01
LD A B
BEQ A D 07
ST C A
ADI C C 01
JPI A 02
ADI C C 99
LD A C
BEQ A D 12
ST B A
JPI A 07
HLT
"""


sampleInput : String
sampleInput = "KSIRomoM"



------------
-- UPDATE --
------------

type Msg
  = SourceEdited String
  | TabClicked Tab
  | Step
  | CharioInputEdited String
  | CharioOutputClear


type alias MemoryAccessPhaseResult =
  { cpu : Cpu
  , memory : Memory
  , chario : Chario
  , wroteAddr : Maybe Dudit
  }


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
          CheckAndRunTab ->
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
              , lastCycle = Cpu.init
              , wroteAddr = Nothing
              , tab = tab
              }

          _ ->
            { model | tab = tab }

    Step ->
      let
        result =
          case Cpu.step model.program model.cpu of
            NoAccessNeeded cpu ->
              MemoryAccessPhaseResult cpu model.memory model.chario Nothing

            ReadNeeded addr fun ->
              let
                ( data, chario ) =
                  case Dudit.toInt addr of
                    90 -> Device.readNumber model.chario
                    91 -> Device.readChar model.chario
                  --92 -> plottouchGetX
                  --93 -> plottouchGetY
                    x ->
                      if x < 90 then
                        ( Memory.read addr model.memory, model.chario )
                      else
                        ( Dudit.zero, model.chario )

                cpu = fun data
              in
                MemoryAccessPhaseResult cpu model.memory chario Nothing

            WriteNeeded addr data cpu ->
              let
                ( memory, chario ) =
                  case Dudit.toInt addr of
                    90 -> ( model.memory, Device.writeNumber data model.chario )
                    91 -> ( model.memory, Device.writeChar data model.chario )
                  --92 -> plottouchSetX
                  --93 -> plottouchSetY
                  --94 -> plottouchExecute
                    x ->
                      if x < 90 then
                        ( Memory.write addr data model.memory, model.chario )
                      else
                        ( model.memory, model.chario )
              in
                MemoryAccessPhaseResult cpu memory chario (Just addr)
      in
        { model
        | cpu = result.cpu
        , memory = result.memory
        , chario = result.chario
        , lastCycle = model.cpu
        , wroteAddr = result.wroteAddr
        }

    CharioInputEdited str ->
      { model | chario = Device.setInput str model.chario }

    CharioOutputClear ->
      { model | chario = Device.clearOutput model.chario }



----------
-- VIEW --
----------

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "elm-erea" ]
    [ Html.div [ Attr.class "flex-container" ]
      [ Html.div [ Attr.class "program-pane" ]
          [ Html.h3 [] [ Html.text "Program" ]
          , programView model
          ]
      , Html.div [ Attr.class "data-pane" ]
        [ Html.h3 [] [ Html.text "Register" ]
        , Html.div [ Attr.class "registers box-frame" ]
          [ registerView "PC" model.cpu.pc model.lastCycle.pc
          , registerView "A" model.cpu.a model.lastCycle.a
          , registerView "B" model.cpu.b model.lastCycle.b
          , registerView "C" model.cpu.c model.lastCycle.c
          , registerView "D" model.cpu.d model.lastCycle.d
          ]
        , Html.h3 [] [ Html.text "Memory" ]
        , memoryView model.wroteAddr model.memory
        ]
      ]
    , charioView model.chario
    ]



-- PROGRAM VIEW --

programView : Model -> Html Msg
programView model =
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
          , tabView CheckAndRunTab "Check & Run"
          ]
        ]
      , Html.div [ Attr.class "tab-content" ]
          ( case model.tab of
            EditTab ->
              [ editView model.source ]
            CheckAndRunTab ->
              [ checkAndRunView model.source model.debug model.errors model.cpu
              , Html.button [ onClick Step ][ Html.text "Step" ]
              ]
          )
      ]


editView : String -> Html Msg
editView source =
  Html.textarea
    [ Attr.class "program editor-content box-frame"
    , Attr.placeholder "Program"
    , onChange SourceEdited
    , Attr.value source
    , Attr.spellcheck False
    ]
    []


checkAndRunView : String -> Dict LineNum Dudit -> Dict LineNum ParseErr -> Cpu -> Html msg
checkAndRunView source debug errors cpu =
  let
    addrAndCodeView : LineNum -> String -> ( Html msg, Html msg )
    addrAndCodeView lineNum str =
      let
        addr =
          Dict.get lineNum debug

        running =
          addr
            |> Maybe.map (Dudit.eq cpu.pc)
            |> Maybe.withDefault False

        error =
          Dict.get lineNum errors
      in
        ( addrView running addr, lineView str error )

    ( lineNums, code ) =
      (source ++ "\n")
        |> String.lines
        |> List.indexedMap addrAndCodeView
        |> List.unzip
  in
    Html.div [ Attr.class "program check-and-run-content box-frame" ]
      [ Html.div [ Attr.class "code-with-addr" ]
        [ Html.div [ Attr.class "addr" ] lineNums
        , Html.div [ Attr.class "code" ] code
        ]
      ]


addrView : Bool -> Maybe Dudit -> Html msg
addrView running maybeAddr =
  let
    content =
      maybeAddr
        |> Maybe.map Dudit.toString
        |> Maybe.withDefault ""
        |> brIfEmpty

    cursor =
      Html.span [ Attr.class "cursor" ] [ Html.text "◤" ]
  in
    if running then
      Html.div [] [ content, cursor ]
    else
      Html.div [] [ content ]


lineView : String -> Maybe ParseErr -> Html msg
lineView str maybeErr =
  let
    line =
      case maybeErr of
        Nothing ->
          [ brIfEmpty str ]

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
  in
    line_
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


brIfEmpty : String -> Html msg
brIfEmpty str =
  case str of
    "" ->
      Html.br [] []

    _ ->
      Html.text str


ariaSelected : Bool -> Html.Attribute msg
ariaSelected bool =
    Attr.attribute "aria-selected" (if bool then "true" else "false")



-- REGISTER VIEW --

registerView : String -> Dudit -> Dudit -> Html msg
registerView label value prev =
  let
    text = Html.text (Dudit.toString value)

    inner =
      if Dudit.eq value prev then
        text
      else
        Html.span [ Attr.class "highlight" ] [ text ]
  in
    Html.div [ Attr.class "register" ]
      [ Html.div [ Attr.class "register-label" ] [ Html.text (label ++ ":") ]
      , Html.div [ Attr.class "register-value" ] [ inner ]
      ]



-- MEMORY VIEW --

memoryView : Maybe Dudit -> Memory -> Html msg
memoryView wroteAddr memory =
  let
    celler coords =
      case coords of
        ( 0, 0 ) ->
          Html.th [] []

        ( 0, x ) ->
          Html.th [] [ Html.text <| String.fromInt <| x - 1 ]

        ( y, 0 ) ->
          Html.th [] [ Html.text <| String.fromInt <| (y - 1) * 10 ]

        ( x, y ) ->
          let
            addr =
              (y - 1) * 10 + (x - 1)

            text =
              case addr of
                90 -> "NM"
                91 -> "CH"
              --92 -> "PX"
              --93 -> "PY"
              --94 -> "PE"
                _ ->
                  if addr < 90 then
                    memory
                      |> Memory.read (Dudit.fromInt addr)
                      |> Dudit.toString
                  else
                    "--"

            highlight =
              wroteAddr
                |> Maybe.map (Dudit.eq (Dudit.fromInt addr))
                |> Maybe.withDefault False

            inner =
              if highlight then
                Html.span [ Attr.class "highlight" ] [ Html.text text ]
              else
                Html.text text
          in
            Html.td [] [ inner ]
  in
    table [ Attr.class "memory" ] 11 11 celler


table : List (Html.Attribute msg) -> Int -> Int -> (( Int, Int ) -> Html msg) -> Html msg
table attributes columns rows celler =
  let
    tbody =
      List.range 0 (rows - 1)
        |> List.map (\y ->
        List.range 0 (columns - 1)
          |> List.map (\x -> celler ( x, y ))
          |> Html.tr [])
        |> Html.tbody []
  in
    Html.table attributes [ tbody ]



-- CHARIO VIEW --

charioView : Chario -> Html Msg
charioView chario =
  let
    outputs =
      chario
        |> Device.getOutput
        |> String.lines
        |> List.map (brIfEmpty >> List.singleton >> Html.div [])
  in
    Html.div []
      [ Html.h3 [] [ Html.text "Input" ]
      , Html.input
        [ Attr.type_ "text"
        , Attr.class "chario box-frame"
        , Attr.placeholder "Program input here..."
        , Attr.value (Device.getInput chario)
        , onChange CharioInputEdited
        ] []
      , Html.div [ Attr.class "h-block" ]
        [ Html.h3 [] [ Html.text "Output" ]
        , Html.button [ onClick CharioOutputClear ]
          [ Html.text "clear" ]
        ]
      , Html.div [ Attr.class "chario box-frame" ] outputs
      ]
