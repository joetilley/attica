{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Attica.Game
(
	GameState,
	Game,
	runGame,
	getPlayer,
	setPlayer,
	damagePlayer,
	setMonster,
	getMonster,
	damageMonster
)
where

import Control.Monad.State
import Attica.Core
import Attica.Player
import Attica.Monster

data GameState = GameState Player Monster

newtype Game a = GameM {
	runGameM :: StateT GameState IO a
} deriving (Monad, MonadIO, MonadState GameState)

runGame :: Game a -> Player -> IO (a, GameState)
runGame g p = runStateT (runGameM g) $ GameState p noMonster

getPlayer :: Game Player
getPlayer = do
	(GameState p _) <- get
	return p

setPlayer :: Player -> Game ()
setPlayer p = do
	(GameState _ m) <- get
	put $ GameState p m
	return ()

damagePlayer :: Int -> Game ()
damagePlayer amt = do 
	p <- getPlayer
	liftIO $ putStrLn $ "You take " ++ (show amt) ++ " damage"
	setPlayer $ damage p amt

setMonster :: Monster -> Game ()
setMonster m = do
	(GameState p _) <- get
	put $ GameState p m

getMonster :: Game Monster
getMonster = do
	(GameState _ m) <- get
	return m

damageMonster :: Int -> Game ()
damageMonster amt = do
	m <- getMonster
	liftIO $ putStrLn $ "The monster takes " ++ (show amt) ++ " damage"
	setMonster $ damage m amt
