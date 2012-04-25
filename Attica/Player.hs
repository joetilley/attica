module Attica.Player
(
	Player,
	player
)
where

import Attica.Combat

data Player = Player Int

instance Combatant Player where
	damage (Player hp) amt = Player $ hp - amt

player :: Int -> Player
player hp = Player hp