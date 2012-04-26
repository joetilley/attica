{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Attica.Game
(
	GameState,
	runGame,
	Game
)
where

import Control.Monad.State
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

