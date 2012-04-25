import System.IO
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT)
import Attica.IO
import Attica.Locations
import Attica.Dice

-- You are trying to use pattern matching with a boolean type to trigger actions
-- Why not make the actions BE the state
-- Game has State plus NextAction

printHeader :: MonadIO m => m ()
printHeader = do
   gamePrintLn "Attica"
   gamePrintLn "A Game of \"Fun\""
   gamePrintLn "Copyright 2012 - Joseph Tilley"
   gamePrintLn ""

combatLoop :: IO ()
combatLoop = do
   inp <- singleLineInput "Type q to quit> "
   if inp == "q" 
      then return()
      else combatLoop

intro :: IO ()
intro = do
   printHeader
   gamePrintLn "Welcome to Attica."
   gamePrintLn "You are a skilled swordsman, seeking adventure."

rollTestLoop :: IO ()
rollTestLoop = do
   inp <- singleLineInput "(enter q to quit) What dice shall I roll today? "
   if inp == "q" 
      then return()
      else do
         let dice = d inp
         v <- rollDice dice
         putStrLn $ "The bones show " ++ (show v)
         rollTestLoop

main :: IO ()
main = do
   rollTestLoop
   intro
   goAnywhere
   
