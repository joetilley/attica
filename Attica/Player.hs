module Attica.Player
(
	Player,
	player
)
where

import Attica.Core

data Player = Player Int

instance Combatant Player where
	damage (Player hp) amt = Player $ hp - amt
	attackBonus _ = 0
	attackDamage _ = 3
	toHit _ = 15

player :: Int -> Player
player hp = Player hp