module MomoRisc.Program exposing
  ( Program
  , LineNum
  , DebugInfo
  , Errors
  , fetch
  , compile
  )

import Array exposing (Array)
import Dict exposing (Dict)

import MomoRisc.Dudit as Dudit exposing (Dudit)
import MomoRisc.Inst as Inst exposing (Inst, ParseErr)



type Program = Program (Array Inst)

type alias LineNum = Int
type alias DebugInfo = Dict LineNum Dudit
type alias Errors = Dict LineNum ParseErr


fetch : Dudit -> Program -> Inst
fetch addr (Program insts) =
  insts
    |> Array.get (Dudit.toInt addr)
    |> Maybe.withDefault Inst.HLT -- never happen



-- COMPILE --

compile : String -> ( Program, DebugInfo, Errors )
compile source =
  let
    lineByLine =
      compileLineByLine source

    program =
      lineByLine
        |> List.map (Tuple.first >> Result.withDefault Inst.HLT)
        |> Array.fromList
        |> fillRest 100 Inst.HLT
        |> Program

    debugInfo =
      lineByLine
        |> List.indexedMap (\addr ( _, lineNum ) -> ( lineNum, Dudit.fromInt addr ))
        |> Dict.fromList

    errors =
      lineByLine
        |> List.filterMap (\( result, lineNum ) ->
          case result of
            Ok _ -> Nothing
            Err e -> Just ( lineNum, e ))
        |> Dict.fromList
  in
    ( program, debugInfo, errors )


fillRest : Int -> a -> Array a -> Array a
fillRest size pad array =
  let
    pads =
      Array.repeat (size - Array.length array) pad
  in
    Array.append array pads


compileLineByLine : String -> List ( Result ParseErr Inst, LineNum )
compileLineByLine source =
  source
    |> String.lines
    |> List.indexedMap (\idx line -> ( idx, trim line ))
    |> List.filterMap (\( idx, line ) ->
      case line of
        "" -> Nothing
        _ -> Just ( (Inst.parse line), idx ))


trim : String -> String
trim line =
  line
    |> String.split "//"
    |> List.head
    |> Maybe.withDefault ""
    |> String.trim
