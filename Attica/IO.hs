module Attica.IO 
(
   gamePrintLn,
   singleLineInput,
   getInput,
   choice,
   makeChoice,
   runChoice
   )
where

import System.IO
import Control.Monad.IO.Class (MonadIO, liftIO)

gamePrintLn :: MonadIO m => String -> m ()
gamePrintLn s = liftIO $ putStrLn s

singleLineInput :: String -> IO String
singleLineInput str = do
   putStr str
   hFlush stdout
   getLine

getInput :: String -> [String] -> IO String
getInput prompt valids = do
   inp <- singleLineInput prompt
   if elem inp valids 
      then return inp
      else do
         gamePrintLn "Invalid Input"
         getInput prompt valids

printStrings :: [String] -> IO ()
printStrings [] = return ()
printStrings (x:xs) = do
   gamePrintLn x
   printStrings xs
         
numStrList :: Int -> [String]
numStrList 0 = []
numStrList n = (show n) : numStrList (n-1)
         
makeChoice :: Show a => [a] -> String -> IO a
makeChoice choices prompt = do
   printStrings $ zipWith (\x y -> (show x) ++ ") " ++ (show y)) [1 .. 1000] choices
   strN <- getInput prompt (numStrList (length choices))
   let n = read strN :: Int
   return (choices !! (n-1))
  
data Choice = Choice String (IO ())

instance Show Choice where
   show (Choice prompt _) = prompt

choice :: String -> IO () -> Choice
choice = Choice

runChoice :: Choice -> IO ()
runChoice (Choice _ action) = action
