module Attica.Monster
(
	monster,
	monsterName,
	monsterCombatResult,
	noMonster,
	monsterHP,
	monsterToHit,
	monsterAttackBonus,
	monsterAttackDamageDice,
	Monster
)
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Attica.Core
import Attica.Dice

data Monster = Monster { 
	monsterName :: String,
	monsterHP :: Int,
	monsterCombatResult :: String,
	monsterToHit :: Int,
	monsterAttackBonus :: Int,
	monsterAttackDamageDice :: Dice
}

instance Combatant Monster where
	damage m amt = m { monsterHP = (monsterHP m) - amt }
	attackBonus m = monsterAttackBonus m
	attackDamageDice m = monsterAttackDamageDice m
	toHit m = monsterToHit m
	hp m = monsterHP m

monster :: MonadIO m => String -> String-> String -> String -> m Monster
monster n hp c ad = do
	mhp <- rollDice $ d hp
	return Monster { monsterName=n, monsterCombatResult=c, monsterHP=mhp,
					 monsterToHit=15, monsterAttackBonus=0, monsterAttackDamageDice=d ad}

noMonster :: Monster
noMonster = Monster { monsterName="", monsterCombatResult="", monsterHP=0,
				      monsterToHit=0, monsterAttackBonus=0, monsterAttackDamageDice=d "0d0"}