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
module Simulation where

import           Combatant          (Combatant)
import qualified Combatant
import           Command            (Command (..))
import qualified Command
import           Control.Category   ((>>>))
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Maybe         (fromMaybe)
import           Data.Monoid        ((<>))
import           Id                 (Id (..), fromId)
import           Move               (Move (..))
import qualified Move
import           Player             (Player (..))
import           Prelude            hiding (id)

data Simulation =
       Simulation
         { combatants      :: !(IntMap Combatant)
         , activeCombatant :: !(Maybe Id)
         , combatLog       :: ![String]
         }

lost :: Player -> Simulation -> Bool
lost !player !sim =
  let
    reducer cmbt x =
      if Combatant.player cmbt == player
         then x && Combatant.dead cmbt
         else x
  in foldr reducer True (combatants sim)

gameOver :: Simulation -> Bool
gameOver !sim =
  lost AI sim || lost User sim

party :: Player -> Simulation -> IntMap Combatant
party !player !sim =
  IntMap.filter (\cmbt -> Combatant.player cmbt == player) (combatants sim)

enemies :: Player -> Simulation -> IntMap Combatant
enemies !player !sim =
  IntMap.filter (\cmbt -> Combatant.player cmbt /= player) (combatants sim)

findActiveCmbt :: Simulation -> Maybe Combatant
findActiveCmbt !sim =
  let
    recur i =
       case IntMap.lookup i (combatants sim) of
         Just cmbt ->
           if Combatant.canHaveActiveTurn cmbt
             then Just cmbt
             else recur (i + 1)
         Nothing ->
                     Nothing
  in
    recur 0

activeCmbt :: Simulation -> Maybe Combatant
activeCmbt !sim =
  case activeCombatant sim of
    Just id ->
      Just (combatantByIdMustExist id sim)
    Nothing ->
      Nothing

activeCmbtMustExist :: Simulation -> Combatant
activeCmbtMustExist !sim =
  fromMaybe (error "this should not be possible") (activeCmbt sim)

doIHaveActiveTurn :: Id -> Simulation -> Bool
doIHaveActiveTurn !id !sim =
  case activeCmbt sim of
    Just cmbt ->
      Combatant.id cmbt == id
    Nothing ->
      False

dropActiveTurn :: Simulation -> Simulation
dropActiveTurn !sim =
  let cmbt =
              activeCmbtMustExist sim
      nextCmbt =
                  Combatant.payTurnCT cmbt
  in clockTickUntilTurn $ sim
    { combatants = IntMap.insert ((fromId . Combatant.id) cmbt) nextCmbt (combatants sim)
    , activeCombatant = Nothing
    }

clockTickUntilTurn :: Simulation -> Simulation
clockTickUntilTurn !initialSim =
  if gameOver initialSim
    then
      initialSim
    else
      let
        recur sim =
          case findActiveCmbt sim of
            Just cmbt ->
              let
                nextCmbt = Combatant.increaseAP cmbt
                nextCombatants = IntMap.insert ((fromId . Combatant.id) nextCmbt) nextCmbt (combatants sim)
              in
                sim
                  { activeCombatant = (Just . Combatant.id) nextCmbt
                  , combatants = nextCombatants
                  }
            Nothing ->
              recur (clockTick sim)
         in
          case activeCombatant initialSim of
            Just _ ->
              initialSim
            Nothing ->
              recur initialSim

whosTurn :: Simulation -> Maybe Player
whosTurn !sim =
  fmap Combatant.player (activeCmbt sim)

clockTick :: Simulation -> Simulation
clockTick !sim =
  sim { combatants = fmap Combatant.clockTick (combatants sim) }

turnOrderArray :: Simulation -> [Combatant]
turnOrderArray !initialSim =
  let
    recur i sim acc =
      if i > 0
        then
          case activeCmbt sim of
            Just cmbt ->
              recur (i - 1) (clockTickUntilTurn (dropActiveTurn sim)) (cmbt : acc)
            Nothing ->
              recur (i - 1) (clockTickUntilTurn sim) acc
        else
          acc
  in
    reverse $ recur 12 initialSim []

combatantById :: Id -> Simulation -> Maybe Combatant
combatantById !(Id id) !sim =
  IntMap.lookup id (combatants sim)

combatantByIdMustExist :: Id -> Simulation -> Combatant
combatantByIdMustExist !id !sim =
  fromMaybe (error "combatant with this ID must exist!") (combatantById id sim)

modifyById :: (Combatant -> Combatant) -> Id -> Simulation -> Simulation
modifyById !f !i@(Id id) !sim =
  case combatantById i sim of
    Just cmbt ->
      sim { combatants = IntMap.insert id (f cmbt) (combatants sim) }
    Nothing ->
      sim

existsAndAlive :: Id -> Simulation -> Bool
existsAndAlive !id !sim =
  case combatantById id sim of
    Just cmbt ->
      Combatant.alive cmbt
    Nothing ->
      False

targetReaction :: Combatant -> Move -> Combatant -> (Combatant, [String])
targetReaction !user !mv !target =
  case mv of
    Attack ->
      let
        dmg = (ceiling >>> fromIntegral) (Combatant.strength user * Combatant.defense target * 20.0)
        msg = Combatant.name user ++ " deals " ++ show dmg ++ " damage to " ++ Combatant.name target ++ "."
      in
        (target { Combatant.hitPoints = Combatant.hitPoints target - dmg }, [ msg ])
    Heal ->
      let
        msg = Combatant.name user ++ " heals " ++ Combatant.name target ++ " for 45 hitpoints."
      in
        (target { Combatant.hitPoints = Combatant.hitPoints target + 45.0 }, [ msg ])
    _ ->
      error "not a single target move"

selfReaction :: Combatant -> Move -> (Combatant, [String])
selfReaction !user !mv =
  case mv of
    Defend ->
      (Combatant.increaseAP (Combatant.toDefendState user),
        [ Combatant.name user ++ " has started defending" ])
    _ ->
      error "not a self targetting move"

simulate :: Command -> Simulation -> Maybe Simulation
simulate !cmd !initialSim =
  let
    tryPay sim mv cmbt =
      case Combatant.payAP (Move.cost mv) cmbt of
        Just nextCmbt ->
          Just (nextCmbt, sim { combatants = IntMap.insert ((fromId . Combatant.id) nextCmbt) nextCmbt (combatants sim)} )
        Nothing ->
          Nothing
    with sim cmbt =
      case cmd of
        SingleTarget mv tid ->
          case tryPay sim mv cmbt of
            Just (nextCmbt, nextSim) ->
              if Combatant.moveAvailable mv nextCmbt && existsAndAlive tid nextSim
                then
                  let
                    target = combatantByIdMustExist tid nextSim
                    (newTarget, logStuff) = targetReaction nextCmbt mv (combatantByIdMustExist tid nextSim)
                    nextNextSim = nextSim { combatants = IntMap.insert (fromId tid) newTarget (combatants nextSim), combatLog = logStuff ++ combatLog nextSim }
                  in Just
                    (dropActiveTurn nextNextSim)
                else Nothing
            Nothing ->
              Nothing
        SelfTarget mv ->
          -- TODO: Ignoring because no self target move that costs AP
          if Combatant.moveAvailable mv cmbt
            then
              let
                (newUser, logStuff) = selfReaction cmbt mv
                nextSim = sim { combatants = IntMap.insert ((fromId . Combatant.id) cmbt) newUser (combatants sim), combatLog = logStuff ++ combatLog sim }
              in
                Just (dropActiveTurn nextSim)
            else Nothing
  in case activeCmbt initialSim of
    Just cmbt ->
      let
        sim = modifyById Combatant.toDefaultState (Combatant.id cmbt) initialSim
        nextCmbt =
          fromMaybe (error "should never happen") (activeCmbt sim)
      in
        with sim nextCmbt
    Nothing ->
      Nothing
