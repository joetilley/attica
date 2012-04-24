module Attica.Monster
(
	monster,
	monsterName,
	monsterCombatResult,
	Monster
)
where

data Monster = Monster { 
	monsterName :: String,
	monsterCombatResult :: String
} deriving (Show) -- For debugging

monster :: String -> String -> Monster
monster n c = Monster { monsterName=n, monsterCombatResult=c }