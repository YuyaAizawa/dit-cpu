module MomoRisc.Memory exposing
  ( Memory
  , zeros
  , read
  , write
  )

import Array exposing (Array)

import MomoRisc.Dudit as Dudit exposing (Dudit)
import MomoRisc.Inst as Inst exposing (Inst)



type Memory = Memory (Array Dudit)


zeros : Memory
zeros =
  Memory (Array.repeat 100 Dudit.zero)


read : Dudit -> Memory -> Dudit
read addr (Memory array) =
  array
    |> Array.get (Dudit.toInt addr)
    |> Maybe.withDefault Dudit.zero -- never happen


write : Dudit -> Dudit -> Memory -> Memory
write addr data (Memory array) =
  array
    |> Array.set (Dudit.toInt addr) data
    |> Memory
