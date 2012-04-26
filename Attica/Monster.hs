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
import Control.Monad.IO.Class (MonadIO, liftIO)

data Monster = Monster { 
	monsterName :: String,
	monsterHP :: Int,
	monsterCombatResult :: String,
	monsterToHit :: Int,
	monsterAttackBonus :: Int,
	monsterAttackDamage :: Int
} deriving (Show) -- For debugging

instance Combatant Monster where
	damage m amt = m { monsterHP = (monsterHP m) - amt }
	attackBonus m = monsterAttackBonus m
	attackDamage m = monsterAttackDamage m
	toHit m = monsterToHit m

monster :: MonadIO m => String -> String-> String -> Int -> m Monster
monster n hp c ad = do
	mhp <- rollDice $ d hp
	return Monster { monsterName=n, monsterCombatResult=c, monsterHP=mhp,
					 monsterToHit=15, monsterAttackBonus=0, monsterAttackDamage=ad}