module Attica.Core
(
	Combatant,
	toHit,
	attackBonus,
	attackDamageDice,
	damage,
	hp
)
where

import Attica.Dice

class Combatant c where
	toHit :: c -> Int
	attackBonus :: c -> Int
	attackDamageDice :: c -> Dice
	damage :: c -> Int -> c
	hp :: c -> Int
