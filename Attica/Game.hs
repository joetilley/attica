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

data GameState = GameState Player

newtype Game a = GameM {
	runGameM :: StateT GameState IO a
} deriving (Monad, MonadIO, MonadState GameState)

runGame :: Game a -> Player -> IO (a, GameState)
runGame g p = runStateT (runGameM g) $ GameState p
