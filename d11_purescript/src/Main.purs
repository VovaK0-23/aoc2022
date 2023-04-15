module Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Result (Result(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (error, log)
import Effect.Exception (message, try)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (argv)
import Parser (Monkey, mkMonkeys)

{-
Why I used BigInt?
Well, I figured why bother with modular calculations when
I can just summon the infinite power of BigInts to do my bidding?
Besides, who needs moduli when you have an infinitely large integer?

In the end, I used both modular calculations and BigInt.
Without modular calculations, it would have taken forever, but unfortunately,
I couldn't make the numbers any smaller than a regular Int in PureScript.
I guess I'll just have to settle for being a mere mortal programmer instead of a wizard.
-}
moveItem :: Array Monkey -> Int -> Array Monkey
moveItem monkeys from = fromMaybe monkeys $ do
  fromMonkey <- Array.find (\m -> m.id == from) monkeys
  Tuple worryLevel id <- Array.head fromMonkey.itemsInspected
  toMonkey <- Array.find (\m -> m.id == id) monkeys
  let
    newFromMonkey = fromMonkey { itemsInspected = Array.drop 1 fromMonkey.itemsInspected }
    newToMonkey = toMonkey { items = toMonkey.items <> [ worryLevel ] }
  traverse (\m -> if m.id == id then Just newToMonkey else Just m) monkeys
    >>= traverse (\m -> if m.id == from then Just newFromMonkey else Just m)

inspectItems1 :: Monkey -> Monkey
inspectItems1 monkey =
  Array.foldl
    ( \monkey' item ->
        monkey'
          { itemsCount = monkey'.itemsCount + BigInt.fromInt 1
          , items = Array.drop 1 monkey'.items
          , itemsInspected = monkey'.itemsInspected <>
              [ (monkey'.test $ BigInt.fromInt ((fromMaybe 0 $ Int.fromString $ BigInt.toString (monkey'.op item)) / 3)) ]
          }
    )
    monkey
    monkey.items

inspectItems2 :: Monkey -> BigInt -> Monkey
inspectItems2 monkey modulo' =
  Array.foldl
    ( \monkey' item ->
        monkey'
          { itemsCount = monkey'.itemsCount + BigInt.fromInt 1
          , items = Array.drop 1 monkey'.items
          , itemsInspected = monkey'.itemsInspected <>
              [ (monkey'.test (((monkey'.op item) `mod` modulo'))) ]
          }
    )
    monkey
    monkey.items

modulo :: Array Monkey -> Int
modulo monkeys = Array.foldl (\acc m -> m.devisor * acc) 1 monkeys

runRounds :: Int -> Array Monkey -> Int -> Array Monkey
runRounds numRounds monkeys part = Array.foldr (\_ ms -> runRound ms part) monkeys (Array.replicate numRounds unit)

runRound :: Array Monkey -> Int -> Array Monkey
runRound monkeys part =
  Array.foldl
    ( \monkeys' monkey ->
        let
          monkeyCurrent = fromMaybe monkey $ Array.find (\m -> m.id == monkey.id) monkeys'
          monkeyInspected = if part == 1 then inspectItems1 monkeyCurrent else inspectItems2 monkeyCurrent (BigInt.fromInt (modulo monkeys'))
          itemsInspected = monkeyInspected.itemsInspected
          monkeys'' = map (\m -> if m.id == monkey.id then monkeyInspected else m) monkeys'
        in
          Array.foldl
            ( \ms _ ->
                moveItem ms monkey.id
            )
            monkeys''
            itemsInspected
    )
    monkeys
    monkeys

monkeyBusiness :: Array Monkey -> BigInt
monkeyBusiness monkeys =
  Array.foldl (*) (BigInt.fromInt 1) $ Array.take 2 $ Array.reverse $ Array.sort $ map (\m -> m.itemsCount) monkeys

solve :: String -> Effect Unit
solve fileContents = do
  case mkMonkeys fileContents of
    Error err -> error $ "Error parsing input:\n " <> err
    Ok monkeys -> do
      let part1 = show $ monkeyBusiness $ runRounds 20 monkeys 1
      let part2 = show $ monkeyBusiness $ runRounds 10000 monkeys 2
      log $ "Part 1: " <> part1
      log $ "Part 2: " <> part2

main :: Effect Unit
main = do
  args <- argv
  case args of
    [ _, _, filePath ] -> do
      fileContentsEither <- try $ (readTextFile UTF8 filePath)

      case fileContentsEither of
        Left err -> error $ message $ err
        Right fileContents -> solve fileContents
    _ -> log "Usage: spago run -b '<input-path>'"
