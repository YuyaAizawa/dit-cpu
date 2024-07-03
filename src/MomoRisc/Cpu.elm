module MomoRisc.Cpu exposing
  ( Cpu
  , init
  , step
  )

import MomoRisc.Dudit as Dudit exposing (Dudit, zero, one, add, sub, toInt)
import MomoRisc.Inst as Inst exposing (Inst(..))
import MomoRisc.Memory as Memory exposing (Memory, load, store)
import MomoRisc.Program as Program exposing (Program)
import MomoRisc.Reg as Reg exposing (Reg(..))



type alias Cpu =
  { pc : Dudit
  , a : Dudit
  , b : Dudit
  , c : Dudit
  , d : Dudit
  }


init : Cpu
init =
  { pc = zero
  , a = zero
  , b = zero
  , c = zero
  , d = zero
  }


step : Program -> Memory -> Cpu -> ( Cpu, Memory )
step prog mem cpu =
  case Program.fetch cpu.pc prog of
    LDI rd imm ->
      let
        cpu_ =
          cpu
            |> write rd imm
            |> incPc
      in
        ( cpu_, mem )

    LD rd rs ->
      let
        addr = read rs cpu
        data = load addr mem
        cpu_ =
          cpu
            |> write rd data
            |> incPc
      in
        ( cpu_, mem )

    ST rd rs ->
      let
        data = read rs cpu
        addr = read rd cpu
        mem_ = store addr data mem
        cpu_ = incPc cpu
      in
        ( cpu_, mem_ )

    ADD rd rs1 rs2 ->
      let
        o1 = read rs1 cpu
        o2 = read rs2 cpu
        r = add o1 o2
        cpu_ =
          cpu
            |> write rd r
            |> incPc
      in
        ( cpu_, mem )

    ADI rd rs imm ->
      let
        o1 = read rs cpu
        o2 = imm
        r = add o1 o2
        cpu_ =
          cpu
            |> write rd r
            |> incPc
      in
        ( cpu_, mem )

    SUB rd rs1 rs2 ->
      let
        o1 = read rs1 cpu
        o2 = read rs2 cpu
        r = sub o1 o2
        cpu_ =
          cpu
            |> write rd r
            |> incPc
      in
        ( cpu_, mem )

    BEQ rs1 rs2 imm ->
      let
        v1 = read rs1 cpu |> toInt
        v2 = read rs2 cpu |> toInt
        pc =
          if v1 == v2
          then imm
          else add cpu.pc one
        cpu_ = { cpu | pc = pc }
      in
        ( cpu_, mem )

    BLT rs1 rs2 imm ->
      let
        v1 = read rs1 cpu |> toInt
        v2 = read rs2 cpu |> toInt
        pc =
          if v1 < v2
          then imm
          else add cpu.pc one
        cpu_ = { cpu | pc = pc }
      in
        ( cpu_, mem )

    JPI rd imm ->
      let
        cpu_ = write rd (add cpu.pc one) cpu
        cpu__ = { cpu_ | pc = imm }
      in
        ( cpu__, mem )

    JP rd rs ->
      let
        cpu_ = write rd (add cpu.pc one) cpu
        addr = read rs cpu_
        cpu__ = { cpu_ | pc = addr }
      in
        ( cpu__, mem )

    HLT ->
      ( cpu, mem )


read : Reg -> Cpu -> Dudit
read reg cpu =
  case reg of
    A -> cpu.a
    B -> cpu.b
    C -> cpu.c
    D -> cpu.d


write : Reg -> Dudit -> Cpu -> Cpu
write reg value cpu =
  case reg of
    A -> { cpu | a = value }
    B -> { cpu | b = value }
    C -> { cpu | c = value }
    D -> { cpu | d = value }


incPc : Cpu -> Cpu
incPc cpu =
  { cpu | pc = add cpu.pc one }
