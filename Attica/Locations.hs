module Attica.Locations 
(
	goAnywhere
)
where

import System.Random
import Attica.IO
import Attica.Monster
import Attica.Player
import Attica.Combat

data Location = Location String [Monster]

sewers = Location "The Sewers" [ (monster "Rat" "You catch rabies."), (monster "CHUD" "You are eaten.")]
graveyard = Location "The Graveyard" [ (monster "Skeleton" "You are boned."), (monster "Vampire" "You are exsanguinated.")]
dragonsLair = Location "The Dragon's Lair" [(monster "Dragon" "You are killed instantly.")]

all_locs = [sewers, graveyard, dragonsLair]

-- | Changes locations into choices that execute a random encounter
locToChoice :: Location -> Choice
locToChoice (Location name mons) = choice name $ do
											r <- getStdRandom (randomR(0, (length mons)-1))
											let m = mons !! r
											gamePrintLn $ "You encounter a " ++ (monsterName m)
											playerHits <- hits (player 100) m
											if playerHits 
												then gamePrintLn $ "You slay the " ++ (monsterName m)
												else gamePrintLn $ monsterCombatResult m

-- | Takes a list of locations, goes to one and executes it
gotoLocation :: [Location] -> IO ()
gotoLocation locs = do
   gamePrintLn "Select a location to explore"
   loc <- makeChoice (map locToChoice locs) "Where to? "
   runChoice loc

goAnywhere :: IO () 
goAnywhere = gotoLocation all_locs