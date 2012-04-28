module Attica.Player
(
	Player,
	player
)
where

import Attica.Core
import Attica.Dice

data Player = Player Int

instance Combatant Player where
	hp (Player p) = p
	damage (Player hp) amt = Player $ hp - amt
	attackBonus _ = 0
	attackDamageDice _ = d "2d4"
	toHit _ = 15

player :: Int -> Player
player hp = Player hp

