{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Attica.Combat
(
	hits,
	combat,
)
where

import System.Random
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State
import Attica.Game
import Attica.IO
import Attica.Core
import Attica.Monster

newtype Combat a = Combat {
	runCombatM :: StateT Monster Game a
} deriving (Monad, MonadIO, MonadState Monster)

liftGame :: Game a -> Combat a
liftGame g = Combat $ StateT $ \s -> do
									a <- g
									return (a, s) 

runCombat :: Combat a -> Monster -> Game (a, Monster)
runCombat c m =  runStateT (runCombatM c) m

combat :: Monster -> Game Monster
combat mi= do
 (_, mo) <- runCombat doCombat mi
 return mo

combatActions = [choice "Attack" attack ]

-- : Runs a round of combat decides whether to continue or not
doCombat :: Combat ()
doCombat = do
	c <- makeChoice combatActions "What now? " -- Get Player Input
   	choiceAction c
	attackPlayer
	doCombat

attackPlayer :: Combat ()
attackPlayer = do
	m <- getMonster
	p <- liftGame getPlayer
	h <- hits m p
	if h
		then liftGame $ damagePlayer (monsterAttackDamage m)
		else liftIO $ putStrLn $ "The " ++ (monsterName m) ++ " misses you."

getMonster :: Combat Monster
getMonster = get

setMonster :: Monster -> Combat ()
setMonster = put

damageMonster :: Int -> Combat ()
damageMonster amt = do
	m <- getMonster
	liftIO $ putStrLn $ "The monster takes " ++ (show amt) ++ " damage"
	setMonster $ damage m amt

attack :: Combat ()
attack = do
	m <- getMonster
	p <- liftGame $ getPlayer
	h <- hits p m 
	if h
		then damageMonster (attackDamage p)
		else liftIO $ putStrLn $ "You miss the " ++ (monsterName m)


hits :: MonadIO m => Combatant a => Combatant b => a -> b -> m Bool
hits a b = do
	r <- liftIO $ getStdRandom (randomR(1, 20))
	if (r + attackBonus a) > (toHit b)
		then return True
		else return False