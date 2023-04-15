module Parser (mkMonkeys, Monkey, Id, WorryLevel) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Result (Result(..))
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.String.Utils as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt

type Id = Int
type WorryLevel = BigInt
type Monkey =
  { id :: Id
  , items :: Array WorryLevel
  , op :: WorryLevel -> WorryLevel
  , test :: WorryLevel -> Tuple Id WorryLevel
  , devisor :: Int
  , itemsCount :: BigInt
  }

isDivisibleBy :: Int -> BigInt -> Boolean
isDivisibleBy x y = y `mod` BigInt.fromInt x == BigInt.fromInt 0

parseLastInt :: String -> Maybe Int
parseLastInt str = (Just $ String.words str) >>= Array.last >>= Int.fromString

parseItems :: String -> Array WorryLevel
parseItems input = Array.mapMaybe BigInt.fromString $ Array.drop 2 $ String.words $ String.filter (\s -> s /= ",") input

parseOp :: String -> Maybe (WorryLevel -> WorryLevel)
parseOp input =
  case opFn of
    Just fn ->
      case valInt of
        (Just valInt') -> Just (fn valInt')
        (Nothing) -> Just (\old -> fn old old)
    _ -> Nothing
  where
  val = Array.last $ String.words input
  valInt = BigInt.fromString =<< val
  opFn = case Array.index (String.words input) 5 of
    Just "*" -> Just (*)
    Just "+" -> Just (+)
    _ -> Nothing

parseTest :: String -> String -> String -> Result String (Tuple Int (WorryLevel -> Tuple Id WorryLevel))
parseTest devisorL trueIdL falseIdL =
  case ([ trueId, falseId, devisor ]) of
    [ Just t, Just f, Just d ] -> Ok (Tuple d testFn)
      where
      testFn worryLevel =
        if isDivisibleBy d (worryLevel) then Tuple t worryLevel
        else Tuple f worryLevel

    [ Nothing, _, _ ] -> Error trueIdL
    [ _, Nothing, _ ] -> Error falseIdL
    [ _, _, Nothing ] -> Error devisorL
    _ -> Error (devisorL <> "\n  |>" <> trueIdL <> "\n  |>" <> falseIdL)
  where
  devisor = parseLastInt devisorL
  trueId = parseLastInt trueIdL
  falseId = parseLastInt falseIdL

mkMonkey :: Tuple Int String -> Result String Monkey
mkMonkey (Tuple index input) =
  let
    lines = String.lines input
  in
    case lines of
      [ _, itemsLine, opLine, tl1, tl2, tl3 ] ->
        let
          items = parseItems itemsLine
          op = parseOp opLine
          test = parseTest tl1 tl2 tl3
        in
          case op of
            Just op' ->
              case test of
                Ok test' -> Ok
                  { id: index
                  , items: items
                  , op: op'
                  , devisor: fst test'
                  , test: snd test'
                  , itemsCount: BigInt.fromInt 0
                  }
                Error line ->
                  Error $ "Monkey: " <> show index <> "\n  " <> "Malformed input: could not parse Test:" <> "\n  |>" <> line
            Nothing ->
              Error $ "Monkey: " <> show index <> "\n  " <> "Malformed input: could not parse Operation:" <> "\n  |>" <> opLine
      _ -> Error $ "Monkey: " <> show index <> "\n  " <> "Malformed input: 6 lines expected to make Monkey"

mkMonkeys :: String -> Result String (Array Monkey)
mkMonkeys fileContents = traverse mkMonkey
  $ Array.mapWithIndex (\i monkey -> Tuple i monkey)
  $ split (Pattern "\n\n")
  $ String.stripMargin fileContents
