module Main where

import Prelude

import Data.Array (drop, foldl, replicate, reverse, sort, take, (!!))
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.Result (Result(..))
import Data.Tuple (uncurry)
import Effect (Effect)
import Effect.Console (error, log)
import Effect.Exception (message, try)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (argv)
import Parser (Id, WorryLevel, Monkey, mkMonkeys)

{-
Why I used BigInt?
Well, I figured why bother with modular calculations when
I can just summon the infinite power of BigInts to do my bidding?
Besides, who needs moduli when you have an infinitely large integer?

In the end, I used both modular calculations and BigInt.
Without modular calculations, it would have taken forever to run, but unfortunately,
I couldn't make the numbers small enough to fit a regular Int in PureScript.
I guess I'll just have to settle for being a mere mortal programmer instead of a wizard.
-}

main :: Effect Unit
main = do
  args <- argv
  case args of
    [ _, _, filePath ] -> do
      fileContentsEither <- try $ readTextFile UTF8 filePath

      case fileContentsEither of
        Left err -> error $ message $ err
        Right fileContents -> solve fileContents
    _ -> log "Usage: spago run -b '<input-path>'"

solve :: String -> Effect Unit
solve fileContents = do
  case mkMonkeys fileContents of
    Error err -> error $ "Error parsing input:\n " <> err
    Ok monkeys -> do
      let part1 = show $ monkeyBusiness $ runRounds 20 monkeys 1
      let part2 = show $ monkeyBusiness $ runRounds 10000 monkeys 2
      log $ "Part 1: " <> part1
      log $ "Part 2: " <> part2
  where
  monkeyBusiness monkeys =
    foldl (*) (BigInt.fromInt 1) $ take 2 $ reverse $ sort $ map (\m -> m.itemsCount) monkeys

-- This function runs the specified number of rounds of the monkey business game,
-- using the provided array of Monkey objects and either the part 1 or part 2 rules.
runRounds :: Int -> Array Monkey -> Int -> Array Monkey
runRounds numRounds monkeys part =
  -- First, compute the modulo value by multiplying the devisors of all the monkeys.
  let modulo = BigInt.fromInt $ foldl (\acc m -> m.devisor * acc) 1 monkeys
      -- Define an inspectFn function that modifies the WorryLevel of each Monkey.
      inspectFn wl =
        if part == 1 then
          -- For part 1, divide the WorryLevel by 3 and convert it to a BigInt.
          BigInt.fromInt $ (fromMaybe 0 $ Int.fromString $ BigInt.toString wl) / 3
        else
          -- For part 2, just return the original WorryLevel.
          wl
      -- Define a runRound function that applies the inspectItems function to each monkey.
      runRound ms' = foldl (inspectItems inspectFn modulo) ms' ms'
  -- Finally, apply the runRound function numRounds times to the monkeys array.
  in foldl (\ms _ -> runRound ms) monkeys $ replicate numRounds unit

inspectItems :: (WorryLevel -> WorryLevel) -> BigInt -> Array Monkey -> Monkey -> Array Monkey
inspectItems fn modulo monkeys monkey =
  -- `items` is an array of the items held by `monkey`
  let items = (fromMaybe monkey $ monkeys !! monkey.id).items
  -- Apply `inspectItem` to each item in `items` and update the `monkeys` array
  in foldl inspectItem monkeys items
  where
  -- `inspectItem` takes in an array of `Monkey` objects `monkeys'` and an `Item` object `item`.
  -- It returns the updated `monkeys'` array with `item` possibly moved to another monkey.
  inspectItem monkeys' item =
    -- Apply the function `fn` to the item's `WorryLevel`, then take the modulus with `modulo`.
    -- Use the result to determine which monkey (if any) to move the item to.
    uncurry (moveItem monkeys' monkey.id) $ monkey.test $ fn $ monkey.op item `mod` modulo

-- | Move an item from one monkey to another, returning a new array of monkeys.
moveItem :: Array Monkey -> Id -> Id -> WorryLevel -> Array Monkey
moveItem monkeys from to item = fromMaybe monkeys do
-- Use fromMaybe to handle the case where the array index is out of bounds
-- Get the monkey at the "from" index and the monkey at the "to" index
  fromMonkey <- monkeys !! from
  toMonkey <- monkeys !! to
-- Create new versions of the from and to monkeys, with the appropriate changes
  let fromMonkey' = fromMonkey { items = drop 1 fromMonkey.items, itemsCount = fromMonkey.itemsCount + BigInt.fromInt 1 }
  let toMonkey' = toMonkey { items = toMonkey.items <> [ item ] }
-- Create a new array of monkeys with the updated from and to monkeys
  pure $ map (\m -> if m.id == from then fromMonkey' else if m.id == to then toMonkey' else m) monkeys
