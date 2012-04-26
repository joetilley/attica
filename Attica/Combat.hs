module Attica.Combat
(
	hits,
	combatLoop
)
where

import System.Random
import Control.Monad.IO.Class (MonadIO, liftIO)
import Attica.Game
import Attica.IO
import Attica.Core
import Attica.Monster

combatActions = [choice "Attack" attack ]

attack :: Game()
attack = do
	m <- getMonster
	p <- getPlayer
	h <- hits p m 
	if h
		then damageMonster (attackDamage p)
		else liftIO $ putStrLn $ "You miss the " ++ (monsterName m)

combatLoop :: Game ()
combatLoop = do
	c <- makeChoice combatActions "What now? "
   	choiceAction c
   	combatLoop


hits :: MonadIO m => Combatant a => Combatant b => a -> b -> m Bool
hits a b = do
	r <- liftIO $ getStdRandom (randomR(1, 20))
	if (r + attackBonus a) > (toHit b)
		then return True
		else return False