import System.IO

printHeader :: IO ()
printHeader = do
   putStrLn "Attica"
   putStrLn "A Game of \"Fun\""
   putStrLn "Copyright 2012 - Joseph Tilley"
   putStrLn ""

singleLineInput :: String -> IO String
singleLineInput str = do
   putStr str
   hFlush stdout
   getLine

getInput :: String -> [String] -> IO String
getInput prompt valids = do
   inp <- singleLineInput "Where to? "
   if elem inp valids 
      then return inp
      else do
         putStrLn "Invalid Input"
         getInput prompt valids

printStrings :: [String] -> IO ()
printStrings [] = return ()
printStrings (x:xs) = do
   putStrLn x
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

runChoice :: Choice -> IO ()
runChoice (Choice _ action) = action

sewers = Choice "The Sewers" $ putStrLn "You encounter a rat.\nYou easily defeat it.\n\nYou Win!"
graveyard = Choice "The Graveyard" $ putStrLn "You encounter a skeleton.\nYou are cleaved in twain.\n\nYou Lose!"
dragonsLair = Choice "The Dragon's Lair" $ putStrLn "You die immediately.\n\nYou Lose!"

main = do
   printHeader
   name <- singleLineInput "What is your name? "
   putStrLn ""
   putStrLn $ "Welcome to Attica, " ++ name
   putStrLn "You are a skilled swordsman, seeking adventure"
   putStrLn "Select a location to explore"
   location <- makeChoice [sewers, graveyard, dragonsLair] "Where to?" 
   runChoice location
