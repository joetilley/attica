module Attica.Core
(
	Combatant,
	toHit,
	damage,
	attackBonus,
	attackDamage,
)
where

class Combatant c where
	attackBonus :: c -> Int
	toHit :: c -> Int
	attackDamage :: c -> Int
	damage :: c -> Int -> c

