module MomoRisc.Dudit exposing
  ( Dudit
  , zero
  , one
  , eq
  , fromInt
  , toInt
  , fromString
  , toString
  , add
  , sub
  )



type Dudit = Dudit Int


zero : Dudit
zero = Dudit 0


one : Dudit
one = Dudit 1


eq : Dudit -> Dudit -> Bool
eq (Dudit v1) (Dudit v2) =
  v1 == v2


fromInt : Int -> Dudit
fromInt int =
  Dudit (modBy 100 int)


toInt : Dudit -> Int
toInt (Dudit int) =
  int


fromString : String -> Maybe Dudit
fromString str =
  String.toInt str
    |> Maybe.andThen(\ int ->
      if int == clamp 0 99 int
      then Just (Dudit int)
      else Nothing)


toString : Dudit -> String
toString (Dudit int) =
  if int < 10 then
    "0" ++ String.fromInt int
  else
    String.fromInt int


add : Dudit -> Dudit -> Dudit
add (Dudit a) (Dudit b) =
  let
    c = a + b
    d = if c < 100 then c else c - 100
  in
    Dudit d


sub : Dudit -> Dudit -> Dudit
sub (Dudit a) (Dudit b) =
  let
    c = a - b
    d = if 0 < c then c else c + 100
  in
    Dudit d
