module Attica.Combat
(
	Combatant,
	toHit,
	damage,
	attackBonus,
	attackDamage,
	hits
)
where

import System.Random
import Control.Monad.IO.Class (MonadIO, liftIO)

class Combatant c where
	attackBonus :: c -> Int
	toHit :: c -> Int
	attackDamage :: c -> Int
	damage :: c -> Int -> c

hits :: MonadIO m => Combatant a => Combatant b => a -> b -> m Bool
hits a b = do
	r <- liftIO $ getStdRandom (randomR(1, 20))
	if (r + attackBonus a) > (toHit b)
		then return True
		else return False