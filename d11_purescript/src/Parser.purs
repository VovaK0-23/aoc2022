module Parser (mkMonkeys, Monkey, Id, WorryLevel) where

import Prelude

import Data.Array as Array
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Result (Result(..))
import Data.String (split)
import Data.String.Extra as StringEx
import Data.String.Pattern (Pattern(..))
import Data.String.Utils as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)

type Id = Int
type WorryLevel = BigInt
type Monkey =
  { id :: Id
  , items :: Array WorryLevel
  , itemsInspected :: Array (Tuple WorryLevel Id)
  , op :: WorryLevel -> WorryLevel
  , test :: WorryLevel -> Tuple WorryLevel Id
  , devisor :: Int
  , itemsCount :: BigInt
  }

isDivisibleBy :: Int -> BigInt -> Boolean
isDivisibleBy x y = y `mod` BigInt.fromInt x == BigInt.fromInt 0

parseLastInt :: String -> Maybe Int
parseLastInt str = (Just $ StringEx.words str) >>= Array.last >>= Int.fromString

parseItems :: String -> Array WorryLevel
parseItems input = Array.mapMaybe BigInt.fromString $ Array.drop 2 $ StringEx.words input

parseOp :: String -> Maybe (WorryLevel -> WorryLevel)
parseOp input =
  let
    op = Array.index (String.words input) 5
    val = Array.last $ StringEx.words input
    valInt = BigInt.fromString =<< val
    opFn = case op of
      Just "*" -> Just (\val' old -> old * val')
      Just "+" -> Just (\val' old -> old + val')
      _ -> Nothing
  in
    case opFn of
      Just fn ->
        case Tuple val valInt of
          Tuple (Just _) (Just valInt') -> Just (fn valInt')
          Tuple (Just _) (Nothing) -> Just (\old -> fn old old)
          _ -> Nothing
      Nothing -> Nothing

parseTest :: String -> String -> String -> Result String (Tuple Int (WorryLevel -> Tuple WorryLevel Id))
parseTest devisorL trueIdL falseIdL = do
  let
    devisor = parseLastInt devisorL
    trueId = parseLastInt trueIdL
    falseId = parseLastInt falseIdL
  case (devisor) of
    Just d ->
      case (Tuple trueId falseId) of
        Tuple (Just t) (Just f) -> Ok
          ( Tuple d
              ( \worryLevel ->
                  if isDivisibleBy d (worryLevel) then Tuple (worryLevel) t
                  else Tuple (worryLevel) f
              )
          )
        Tuple (Nothing) (_) -> Error trueIdL
        Tuple (_) (Nothing) -> Error falseIdL
    _ -> Error devisorL

mkMonkey :: Tuple Int String -> Result String Monkey
mkMonkey (Tuple index input) =
  let
    lines = split (Pattern "\n") input
  in
    case lines of
      [ _, itemsLine, opLine, tl1, tl2, tl3 ] ->
        let
          items = parseItems itemsLine
          op = parseOp opLine
          test = parseTest tl1 tl2 tl3
        in
          case op of
            Just op ->
              case test of
                Ok test -> Ok
                  { id: index
                  , items: items
                  , itemsInspected: []
                  , op: op
                  , devisor: fst test
                  , test: snd test
                  , itemsCount: BigInt.fromInt 0
                  }
                Error line -> Error $ "Monkey: " <> show index <> "\n  " <> "Malformed input: could not parse Test:\n  |>" <> line
            Nothing -> Error $ "Monkey: " <> show index <> "\n  " <> "Malformed input: could not parse Operation:\n  |>" <> opLine
      _ -> Error $ "Monkey: " <> show index <> "\n  " <> "Malformed input: 6 lines expected to make Monkey"

mkMonkeys :: String -> Result String (Array Monkey)
mkMonkeys fileContents = traverse mkMonkey
  $ Array.mapWithIndex (\i monkey -> Tuple i monkey)
  $ split (Pattern "\n\n")
  $ String.stripMargin fileContents
