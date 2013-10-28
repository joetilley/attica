module Attica.Locations 
(
	goAnywhere
)
where

import System.Random
import Attica.Combat
import Attica.Game
import Attica.IO
import Attica.Monster
import Attica.Player
import Control.Monad.IO.Class (MonadIO, liftIO)

data Location = Location String [Game Monster]

sewers = Location "The Sewers" [ (monster "Rat" "2d4" "You catch rabies." "1d3"), (monster "CHUD" "2d4" "You are eaten." "2d3")]
dungeon = Location "The Castle Dungeon" [(monster "Cultist" "3d4" "You are sacrificed." "1d6")]
graveyard = Location "The Graveyard" [ (monster "Skeleton" "2d6"  "You are boned." "2d5"), (monster "Vampire" "3d8" "You are exsanguinated." "3d5")]
dragonsLair = Location "The Dragon's Lair" [(monster "Dragon" "3d12" "You are fried." "5d6")]

all_locs = [sewers, dungeon, graveyard, dragonsLair]

randomEncounter :: [Game Monster] -> Game ()
randomEncounter mons = do
	r <- liftIO $ getStdRandom (randomR(0, (length mons)-1))
	m <- mons !! r
	gamePrintLn $ "You encounter a " ++ (monsterName m)
	(res, _) <- combat m
	liftIO $ putStrLn res
	return ()

-- | Changes locations into choices that execute a random encounter
locToChoice :: Location -> Choice Game
locToChoice (Location name mons) = choice name $ randomEncounter mons

-- | Takes a list of locations, goes to one and executes it
gotoLocation :: [Location] -> Game ()
gotoLocation locs = do
   gamePrintLn "Select a location to explore"
   loc <- makeChoice (map locToChoice locs) "Where to? "
   runChoice loc

goAnywhere :: Game () 
goAnywhere = gotoLocation all_locs