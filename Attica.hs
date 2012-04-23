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
         
runChoice :: [String] -> String -> IO Int
runChoice choices prompt = do
   printStrings $ zipWith (\x y -> (show x) ++ ") " ++ y) [1 .. 1000] choices
   strN <- getInput prompt (numStrList (length choices))
   let n = read strN :: Int
   return n
  
main = do
   printHeader
   name <- singleLineInput "What is your name? "
   putStrLn ""
   putStrLn $ "Welcome to Attica, " ++ name
   putStrLn "You are a skilled swordsman, seeking adventure"
   putStrLn "Select a location to explore"
   location <- runChoice ["The Sewers", "The Graveyard"] "Where to?" 
   if location == 1
      then putStrLn "You encounter a rat.\nYou easily defeat it.\n\nYou Win!"
      else putStrLn "You encounter a skeleton.\nYou are cleaved in twain.\n\nYou Lose!"