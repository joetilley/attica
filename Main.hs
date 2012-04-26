import System.IO
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT)
import Attica.IO
import Attica.Locations
import Attica.Dice
import Attica.Game
import Attica.Player

-- You are trying to use pattern matching with a boolean type to trigger actions
-- Why not make the actions BE the state
-- Game has State plus NextAction

printHeader :: MonadIO m => m ()
printHeader = do
   gamePrintLn "Attica"
   gamePrintLn "A Game of \"Fun\""
   gamePrintLn "Copyright 2012 - Joseph Tilley"
   gamePrintLn ""

intro :: IO ()
intro = do
   printHeader
   gamePrintLn "Welcome to Attica."
   gamePrintLn "You are a skilled swordsman, seeking adventure."

main :: IO ()
main = do
   let thePlayer = player 50
   intro
   runGame goAnywhere $ thePlayer
   return ()