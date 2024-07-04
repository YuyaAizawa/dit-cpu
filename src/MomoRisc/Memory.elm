module MomoRisc.Memory exposing
  ( Memory
  , zeros
  , load
  , store
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
