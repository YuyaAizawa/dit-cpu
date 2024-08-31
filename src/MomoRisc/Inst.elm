module MomoRisc.Inst exposing
  ( Inst(..)
  , ParseErr(..)
  , parse
  )

import MomoRisc.Reg as Reg exposing (Reg)
import MomoRisc.Dudit as Imm exposing (Dudit)



type Inst
  = LDI Reg Imm      -- Reg <- Imm
  | LD Reg Reg       -- Reg1 <- mem[Reg2]
  | ST Reg Reg       -- mem[Reg1] <- Reg2
  | ADD Reg Reg Reg  -- Reg1 <- Reg2 + Reg3
  | ADI Reg Reg Imm  -- Reg1 <- Reg2 + Imm
  | SUB Reg Reg Reg  -- Reg1 <- Reg2 - Reg3
  | MUL Reg Reg Reg  -- Reg1 <- L(Reg2 * Reg3)
  | MUH Reg Reg Reg  -- Reg1 <- H(Reg2 * Reg3)
  | BEQ Reg Reg Imm  -- PC <- Imm if Reg1 = Reg2
  | BLT Reg Reg Imm  -- PC <- Imm if Reg1 < Reg2
  | JPI Reg Imm      -- Reg <- PC + 1, PC <- Imm
  | JP Reg Reg       -- Reg1 <- PC + 1, PC <- Reg2
  | HLT


type alias Imm = Dudit


type ParseErr
  = OpName String
  | ArgsLength Int Int -- expect actual
  | ArgsFormat Int -- index

parse : String -> Result ParseErr Inst
parse str =
  case str |> String.toUpper |> String.words of
    [] ->
      Err <| OpName ""

    op :: args ->
      case op of
        "LDI" ->
          args
            |> checkLength 2 (\args0 -> args0
            |> reg 1 (\r args1 -> args1
            |> imm 2 (\i args2 -> Ok (LDI r i))))

        "LD" ->
          args
            |> checkLength 2 (\args0 -> args0
            |> reg 1 (\r1 args1 -> args1
            |> reg 2 (\r2 args2 -> Ok (LD r1 r2))))

        "ST" ->
          args
            |> checkLength 2 (\args0 -> args0
            |> reg 1 (\r1 args1 -> args1
            |> reg 2 (\r2 args2 -> Ok (ST r1 r2))))

        "ADD" ->
          args
            |> checkLength 3 (\args0 -> args0
            |> reg 1 (\r1 args1 -> args1
            |> reg 2 (\r2 args2 -> args2
            |> reg 3 (\r3 args3 -> Ok (ADD r1 r2 r3)))))

        "ADI" ->
          args
            |> checkLength 3 (\args0 -> args0
            |> reg 1 (\r1 args1 -> args1
            |> reg 2 (\r2 args2 -> args2
            |> imm 3 (\i  args3 -> Ok (ADI r1 r2 i)))))

        "SUB" ->
          args
            |> checkLength 3 (\args0 -> args0
            |> reg 1 (\r1 args1 -> args1
            |> reg 2 (\r2 args2 -> args2
            |> reg 3 (\r3 args3 -> Ok (SUB r1 r2 r3)))))

        "MUL" ->
          args
            |> checkLength 3 (\args0 -> args0
            |> reg 1 (\r1 args1 -> args1
            |> reg 2 (\r2 args2 -> args2
            |> reg 3 (\r3 args3 -> Ok (MUL r1 r2 r3)))))

        "MUH" ->
          args
            |> checkLength 3 (\args0 -> args0
            |> reg 1 (\r1 args1 -> args1
            |> reg 2 (\r2 args2 -> args2
            |> reg 3 (\r3 args3 -> Ok (MUH r1 r2 r3)))))

        "BEQ" ->
          args
            |> checkLength 3 (\args0 -> args0
            |> reg 1 (\r1 args1 -> args1
            |> reg 2 (\r2 args2 -> args2
            |> imm 3 (\i  args3 -> Ok (BEQ r1 r2 i)))))

        "BLT" ->
          args
            |> checkLength 3 (\args0 -> args0
            |> reg 1 (\r1 args1 -> args1
            |> reg 2 (\r2 args2 -> args2
            |> imm 3 (\i  args3 -> Ok (BLT r1 r2 i)))))

        "JPI" ->
          args
            |> checkLength 2 (\args0 -> args0
            |> reg 1 (\r args1 -> args1
            |> imm 2 (\i args2 -> Ok (JPI r i))))

        "JP" ->
          args
            |> checkLength 2 (\args0 -> args0
            |> reg 1 (\r1 args1 -> args1
            |> reg 2 (\r2 args2 -> Ok (JP r1 r2))))

        "HLT" ->
          args
            |> checkLength 0 (\args0 -> Ok (HLT))

        _ ->
          Err (OpName op)


checkLength : Int -> (List String -> Result ParseErr Inst) -> List String -> Result ParseErr Inst
checkLength expeted fn list =
  let
    actual = List.length list
  in
    if actual == expeted
    then fn list
    else Err (ArgsLength expeted actual)


reg : Int -> (Reg -> List String -> Result ParseErr Inst) -> List String -> Result ParseErr Inst
reg idx fn args =
  case args of
    [] ->
      Err (OpName "") -- never happen

    hd :: tl ->
      Reg.fromString hd
        |> Result.fromMaybe (ArgsFormat idx)
        |> Result.andThen (\r -> fn r tl)


imm : Int -> (Imm -> List String -> Result ParseErr Inst) -> List String -> Result ParseErr Inst
imm idx fn args =
  case args of
    [] ->
      Err <| (OpName "") -- never happen

    hd :: tl ->
      Imm.fromString hd
        |> Result.fromMaybe (ArgsFormat idx)
        |> Result.andThen (\i -> fn i tl)
