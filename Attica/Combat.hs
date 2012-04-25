module Attica.Combat
(
	Combatant,
	damage,
	hits
)
where

import System.Random

class Combatant c where
	damage :: c -> Int -> c

hits :: Combatant a => Combatant b => a -> b -> IO Bool
hits a b = do
	r <- getStdRandom (randomR(0, 1)) :: IO Int
	if r < 1
		then return True
		else return False