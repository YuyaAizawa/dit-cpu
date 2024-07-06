module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onChange)

import MomoRisc.Cpu as Cpu exposing (Cpu, MemoryAccessPhaseAction(..))
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
  , source : String
  , errors : Errors
  , debug : DebugInfo
  , lastCycle : Cpu
  , wroteAddr : Maybe Dudit
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
    , lastCycle = Cpu.init
    , wroteAddr = Nothing
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


------------
-- UPDATE --
------------

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
              , lastCycle = Cpu.init
              , wroteAddr = Nothing
              , tab = tab
              }

          _ ->
            { model | tab = tab }

    Step ->
      let
        ( cpu, memory, wroteAddr ) =
          case Cpu.step model.program model.cpu of
            NoAccessNeeded cpu_ ->
              ( cpu_, model.memory, Nothing )

            ReadNeeded addr fun ->
              let
                cpu_ =
                  model.memory
                    |> Memory.read addr
                    |> fun
              in
                ( cpu_, model.memory, Nothing )

            WriteNeeded addr data cpu_ ->
              let
                memory_ = model.memory |> Memory.write addr data
              in
                ( cpu_, memory_, Just addr )
      in
        { model
        | cpu = cpu
        , memory = memory
        , lastCycle = model.cpu
        , wroteAddr = wroteAddr
        }


----------
-- VIEW --
----------

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "elm-erea" ]
    [ Html.h2 [] [ Html.text "CPU Emulator" ]
    , Html.div [ Attr.class "section-container" ]
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
      ]
    ]



-- PROGRAM PANE --

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
          , tabView CheckTab "Check & Run"
          ]
        ]
      , Html.div [ Attr.class "tab-content" ]
          ( case model.tab of
            EditTab ->
              [ editView model.source ]
            CheckTab ->
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
        ( addrView addr, lineView running str error )

    ( lineNums, code ) =
      (source ++ "\n")
        |> String.lines
        |> List.indexedMap addrAndCodeView
        |> List.unzip
  in
    Html.div [ Attr.class "program check-and-run-content box-frame" ]
      [ Html.div [ Attr.class "code-with-addr" ]
        [ Html.div [ Attr.class "addr" ] lineNums
        , Html.div [] code
        ]
      ]


lineView : Bool -> String -> Maybe ParseErr -> Html msg
lineView cursor str maybeErr =
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

    line__ =
      if cursor then
        (Html.span [ Attr.class "cursor" ] [ Html.text "◤" ]) :: line_
      else
        line_

  in
    line__
      |> Html.div [ Attr.class "line" ]


addrView : Maybe Dudit -> Html msg
addrView maybeAddr =
  let
    content =
      maybeAddr
        |> Maybe.map Dudit.toString
        |> Maybe.withDefault ""
        |> brIfEmpty
  in
    Html.div [] [ content ]


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



-- DATA PANE --

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
                |> Dudit.fromInt

            text =
              memory
                |> Memory.read addr
                |> Dudit.toString
                |> Html.text

            highlight =
              wroteAddr
                |> Maybe.map (Dudit.eq addr)
                |> Maybe.withDefault False

            inner =
              if highlight then
                Html.span [ Attr.class "highlight" ] [ text ]
              else
                text
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
