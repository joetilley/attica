{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Attica.Game
(
	GameState,
	runGame,
	Game
)
where

import Control.Monad.State
import Attica.Core
import Attica.Player
import Attica.Monster

data GameState = GameState Player (Maybe Monster)

newtype Game a = GameM {
	runGameM :: StateT GameState IO a
} deriving (Monad, MonadIO, MonadState GameState)

runGame :: Game a -> Player -> IO (a, GameState)
runGame g p = runStateT (runGameM g) $ GameState p Nothing

thePlayer :: Game a -> Game Player
thePlayer g = do
	(GameState p _) <- get
	return p

setPlayer :: Game a -> Player -> Game ()
setPlayer g p = do
	(GameState _ m) <- get
	put $ GameState p m
	return ()

damagePlayer :: Game a -> Int -> Game ()
damagePlayer g amt = do 
	p <- thePlayer g
	setPlayer g $ damage p amt

setMonster :: Game a -> Monster -> Game ()
setMonster g m = do
	(GameState p _) <- get
	put $ GameState p $ Just m

getMonster :: Game a -> Game (Maybe Monster)
getMonster g = do
	(GameState _ m) <- get
	return m

damageMonster :: Game a -> Int -> Game ()
damageMonster g amt = do
	mm <- getMonster g
	case mm of
		Just m -> setMonster g $ damage m amt
		Nothing -> return ()
