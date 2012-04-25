module Attica.Dice 
(
	d,
	rollDice
)
where

import Control.Monad
import System.Random

data Dice = Dice Int Int Int -- Number of Dice, Number of Sides, +/-

d :: Int -> Int -> Int -> Dice
d num sides modifier = Dice num sides modifier

--  | Resolves a die roll
rollDice :: Dice -> IO Int
rollDice (Dice num sides modifier) = do
	diceVals <- forM (replicate num sides) rollDie
	let summedDice = foldl (+) 0 diceVals
	return $ summedDice + modifier

--  | Rolls an n sided die
rollDie :: Int -> IO Int
rollDie n = getStdRandom (randomR(1, n)) :: IO Int
