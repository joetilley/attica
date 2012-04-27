{-# LANGUAGE ExistentialQuantification #-}

module Attica.IO 
(
   Choice,
   gamePrintLn,
   singleLineInput,
   getInput,
   choice,
   makeChoice,
   runChoice,
   choiceAction
   )
where

import System.IO
import Control.Monad.IO.Class (MonadIO, liftIO)

gamePrintLn :: MonadIO m => String -> m ()
gamePrintLn s = liftIO $ putStrLn s

singleLineInput :: MonadIO m => String -> m String
singleLineInput str = do
   liftIO $ putStr str
   liftIO $ hFlush stdout
   liftIO $ getLine

getInput :: MonadIO m => String -> [String] -> m String
getInput prompt valids = do
   inp <- singleLineInput prompt
   if elem inp valids 
      then return inp
      else do
         gamePrintLn "Invalid Input"
         getInput prompt valids

printStrings :: MonadIO m => [String] -> m ()
printStrings [] = return ()
printStrings (x:xs) = do
   gamePrintLn x
   printStrings xs
         
numStrList :: Int -> [String]
numStrList 0 = []
numStrList n = (show n) : numStrList (n-1)
         
makeChoice :: MonadIO m => Show a => [a] -> String -> m a
makeChoice choices prompt = do
   gamePrintLn ""
   printStrings $ zipWith (\x y -> (show x) ++ ") " ++ (show y)) [1 .. 1000] choices
   strN <- getInput prompt (numStrList (length choices))
   let n = read strN :: Int
   gamePrintLn ""
   return (choices !! (n-1))
  
data Choice m = MonadIO m => Choice String (m ())

instance Show (Choice m) where
   show (Choice prompt _) = prompt

choice :: MonadIO m => String -> m () -> Choice m
choice = Choice

choiceAction :: MonadIO m => Choice m -> m ()
choiceAction (Choice _ a) = a

runChoice :: MonadIO m => Choice m -> m ()
runChoice (Choice _ action) = action
