module Attica.Monster
(
	monster,
	monsterName,
	monsterCombatResult,
	Monster
)
where

import Attica.Combat
import Attica.Dice

data Monster = Monster { 
	monsterName :: String,
	monsterHP :: Int,
	monsterCombatResult :: String
} deriving (Show) -- For debugging

instance Combatant Monster where
	damage m amt = m { monsterHP = (monsterHP m) - amt }

monster :: String -> String-> String -> IO Monster
monster n hp c = do
	mhp <- rollDice $ d hp
	return Monster { monsterName=n, monsterCombatResult=c, monsterHP=mhp}