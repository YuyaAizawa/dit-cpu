module MomoRisc.Reg exposing
  ( Reg(..)
  , fromString
  , toString
  )



type Reg
  = A
  | B
  | C
  | D


fromString : String -> Maybe Reg
fromString str =
  case str of
    "A" -> Just A
    "B" -> Just B
    "C" -> Just C
    "D" -> Just D
    _   -> Nothing


toString : Reg -> String
toString reg =
  case reg of
    A -> "A"
    B -> "B"
    C -> "C"
    D -> "D"