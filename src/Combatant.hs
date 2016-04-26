{-
hs-turn-based-battle, a small browser game.
Copyright (C) 2016  Emily A. Bellows

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE BangPatterns #-}
module Combatant (
    State(..),
    Combatant(..),
    mkCombatant,
    strength,
    defense,
    defenseBonus,
    speed,
    moveArray,
    moveAvailable,
    alive,
    dead,
    friendsOf,
    foesOf,
    friends,
    foes,
    canHaveActiveTurn,
    clockTick,
    increaseAP,
    payAP,
    payTurnCT,
    toDefaultState,
    toDefendState,
    ) where

import           Class      (Class (..))
import qualified Class
import qualified Data.List  as List
import           Data.Maybe (Maybe (..))
import           Data.Ord   (min)
import           Id         (Id (..))
import           Move       (Move)
import           Player     (Player (..))
import           Prelude    hiding (id)

data State = Default
           | Defending

data Combatant =
       Combatant
         { id           :: !Id
         , player       :: !Player
         , name         :: !String
         , cclass       :: !Class
         , hitPoints    :: !Double
         , actionPoints :: !Int
         , chargeTime   :: !Int
         , state        :: !State
         }

mkCombatant :: Class -> Combatant
mkCombatant !cls =
  let def =
             Combatant
               { id = Id 0
               , player = User
               , name = "missingname"
               , cclass = Warrior
               , hitPoints = 0.0
               , actionPoints = 0
               , chargeTime = 0
               , state = Default
               }
  in case cls of
    Warrior ->
      def { cclass = cls, hitPoints = 100.0 }
    Thief ->
      def { cclass = cls, hitPoints = 70.0 }
    Cleric ->
      def { cclass = cls, hitPoints = 80.0 }

strength :: Combatant -> Double
strength !cmbt = Class.strength (cclass cmbt)

defense :: Combatant -> Double
defense !cmbt = Class.defense (cclass cmbt) * defenseBonus cmbt

defenseBonus :: Combatant -> Double
defenseBonus !cmbt =
  case state cmbt of
    Defending -> 0.5
    _         -> 1.0

speed :: Combatant -> Int
speed !cmbt = Class.speed (cclass cmbt)

moveArray :: Combatant -> [Move]
moveArray !cmbt = Class.moveArray (cclass cmbt)

moveAvailable :: Move -> Combatant -> Bool
moveAvailable !move !cmbt = move `List.elem` moveArray cmbt

alive :: Combatant -> Bool
alive !cmbt = hitPoints cmbt > 0.0

dead :: Combatant -> Bool
dead !cmbt = not (alive cmbt)

friendsOf :: Player -> Combatant -> Bool
friendsOf !player' !cmbt = player cmbt == player'

foesOf :: Player -> Combatant -> Bool
foesOf !player' !cmbt = player cmbt /= player'

friends :: Combatant -> Combatant -> Bool
friends !cmbtA !cmbtB = player cmbtA == player cmbtB

foes :: Combatant -> Combatant -> Bool
foes !cmbtA !cmbtB = player cmbtA /= player cmbtB

canHaveActiveTurn :: Combatant -> Bool
canHaveActiveTurn !cmbt = alive cmbt && chargeTime cmbt >= 100

clockTick :: Combatant -> Combatant
clockTick !cmbt =
  if alive cmbt
    then cmbt { chargeTime = chargeTime cmbt + speed cmbt }
    else cmbt

increaseAP :: Combatant -> Combatant
increaseAP !cmbt =
  cmbt { actionPoints = min (actionPoints cmbt + 1) 5 }

payAP :: Int -> Combatant -> Maybe Combatant
payAP !amount !cmbt =
  if actionPoints cmbt >= amount
    then Just $
      cmbt { actionPoints = actionPoints cmbt - amount }
    else Nothing

payTurnCT :: Combatant -> Combatant
payTurnCT !cmbt =
  cmbt { chargeTime = chargeTime cmbt - 100 }

toDefaultState :: Combatant -> Combatant
toDefaultState !cmbt =
  cmbt { state = Default }

toDefendState :: Combatant -> Combatant
toDefendState !cmbt =
  cmbt { state = Defending }
