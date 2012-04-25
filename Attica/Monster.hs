module Attica.Monster
(
	monster,
	monsterName,
	monsterCombatResult,
	Monster
)
where

import Attica.Combat

data Monster = Monster { 
	monsterName :: String,
	monsterHP :: Int,
	monsterCombatResult :: String
} deriving (Show) -- For debugging

instance Combatant Monster where
	damage m amt = m { monsterHP = (monsterHP m) - amt }

monster :: String -> String -> Monster
monster n c = Monster { monsterName=n, monsterCombatResult=c, monsterHP=50 }

	