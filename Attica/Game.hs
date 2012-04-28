{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Attica.Game
(
	Game(..),
	runGame,
	getPlayer,
	setPlayer,
	damagePlayer,
)
where

import Control.Monad.State
import Attica.Core
import Attica.Player

newtype Game a = Game {
	runGameM :: StateT Player IO a
} deriving (Monad, MonadIO, MonadState Player)

runGame :: Game a -> Player -> IO (a, Player)
runGame g p = runStateT (runGameM g) p

getPlayer :: Game Player
getPlayer = get

setPlayer :: Player -> Game ()
setPlayer = put

damagePlayer :: Int -> Game ()
damagePlayer amt = do 
	p <- getPlayer
	setPlayer $ damage p amt
	p <- getPlayer
	liftIO $ putStrLn $ "You take " ++ (show amt) ++ " damage. " ++ (show $ hp p) ++ " remaining."

