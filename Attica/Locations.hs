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
import Attica.Game
import Control.Monad.IO.Class (MonadIO, liftIO)

data Location = Location String [Game Monster]

sewers = Location "The Sewers" [ (monster "Rat" "2d4" "You catch rabies." 1), (monster "CHUD" "2d4" "You are eaten." 3)]
graveyard = Location "The Graveyard" [ (monster "Skeleton" "2d6"  "You are boned." 2), (monster "Vampire" "3d8" "You are exsanguinated." 4)]
dragonsLair = Location "The Dragon's Lair" [(monster "Dragon" "3d12" "You are fried." 10)]

all_locs = [sewers, graveyard, dragonsLair]

randomEncounter :: [Game Monster] -> Game ()
randomEncounter mons = do
	r <- liftIO $ getStdRandom (randomR(0, (length mons)-1))
	m <- mons !! r
	gamePrintLn $ "You encounter a " ++ (monsterName m)
	combat m
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