module Attica.Dice 
(
	d,
	rollDice
)
where

import Control.Monad
import System.Random
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Control.Monad.IO.Class (MonadIO, liftIO)

data Dice = Dice Int Int -- Number of Dice, Number of Sides

d :: String -> Dice
d str = case (parse diceParser "Bad dice value" str) of
	Left _ -> Dice 0 0 
	Right r -> r

intP :: GenParser Char st Int
intP = do
	res <- many1 digit
	return (read res::Int)

diceParser :: GenParser Char st Dice
diceParser = do
	numDice <- intP
	char 'd'
	numSides <- intP
	return $ Dice numDice numSides

--  | Resolves a die roll
rollDice :: MonadIO m => Dice -> m Int
rollDice (Dice num sides) = do
	diceVals <- forM (replicate num sides) rollDie
	return $ foldl (+) 0 diceVals

--  | Rolls an n sided die
rollDie :: MonadIO m => Int -> m Int
rollDie n = liftIO $ getStdRandom (randomR(1, n)) 

