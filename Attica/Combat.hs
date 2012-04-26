module Attica.Combat
(
	hits
)
where

import System.Random
import Control.Monad.IO.Class (MonadIO, liftIO)
import Attica.Game
import Attica.IO
import Attica.Core

combatLoop :: Game ()
combatLoop = do
   inp <- singleLineInput "Type q to quit> "
   if inp == "q" 
      then return()
      else combatLoop


hits :: MonadIO m => Combatant a => Combatant b => a -> b -> m Bool
hits a b = do
	r <- liftIO $ getStdRandom (randomR(1, 20))
	if (r + attackBonus a) > (toHit b)
		then return True
		else return False