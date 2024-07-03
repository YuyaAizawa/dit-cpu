module MomoRisc.Memory exposing
  ( Memory
  , zeros
  , load
  , store
  , to2dList
  )

import Array exposing (Array)

import MomoRisc.Dudit as Dudit exposing (Dudit)
import MomoRisc.Inst as Inst exposing (Inst)



type Memory = Memory (Array Dudit)


zeros : Memory
zeros =
  Memory (Array.repeat 100 Dudit.zero)


load : Dudit -> Memory -> Dudit
load addr (Memory array) =
  array
    |> Array.get (Dudit.toInt addr)
    |> Maybe.withDefault Dudit.zero -- never happen


store : Dudit -> Dudit -> Memory -> Memory
store addr data (Memory array) =
  array
    |> Array.set (Dudit.toInt addr) data
    |> Memory


to2dList : ( Memory, Memory ) -> List (List ( Dudit, Dudit ))
to2dList ( Memory array, Memory prev ) =
  let
    list =
      List.map2 Tuple.pair (Array.toList array) (Array.toList prev)
  in
    to2dListHelp list []


to2dListHelp : List ( Dudit, Dudit ) -> List (List ( Dudit, Dudit )) -> List (List ( Dudit, Dudit ))
to2dListHelp rest acc =
  case rest of
    [] ->
      acc |> List.reverse

    list ->
      to2dListHelp (list |> List.drop 10) ((list |> List.take 10) :: acc)
