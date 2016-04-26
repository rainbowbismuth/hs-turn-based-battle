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

module AI.AlphaBeta where

import           Combatant     (Combatant)
import qualified Combatant
import           Command       (Command (..), CommandType (..))
import qualified Command
import           Data.Foldable (foldr)
import qualified Data.IntMap   as IntMap
import qualified Data.List     as List
import           Data.Maybe    (Maybe (..), mapMaybe)
import           Data.Ord      (max, min)
import           Move          (Move)
import           Player        (Player (..))
import           Prelude
import           Simulation    (Simulation)
import qualified Simulation

scoreCombatant :: Combatant -> Double
scoreCombatant !cmbt =
  let
    bonus =
      if Combatant.alive cmbt then
        50.0
      else
        0.0

    cScore =
      max 0.0 (Combatant.hitPoints cmbt) + bonus
  in
    if Combatant.player cmbt == AI then
      cScore
    else
      -cScore


score :: Simulation -> Double
score !sim =
  foldr (\x y -> scoreCombatant x + y) 0.0 (Simulation.combatants sim)


targetsForMove :: Simulation -> Move -> [Command]
targetsForMove !sim !mv =
  case Command.typeOfMove mv of
    SingleTargetType ->
      Simulation.combatants sim
        |> IntMap.elems
        |> mapMaybe
            (\target ->
              if Combatant.alive target then
                Just (SingleTarget mv (Combatant.id target))
              else
                Nothing
            )

    SelfTargetType ->
      [ SelfTarget mv ]


availableMoves :: Simulation -> [Command]
availableMoves !sim =
  Combatant.moveArray (Simulation.activeCmbtMustExist sim)
    |> (=<<) (targetsForMove sim)


inf :: Double
inf =
  256000.0


evaluatePosition :: Simulation -> Int -> Double
evaluatePosition !sim !depth =
  alphabeta sim depth (-inf) inf


alphabeta :: Simulation -> Int -> Double -> Double -> Double
alphabeta !sim !depth !a !b =
  if depth == 0 || Simulation.gameOver sim then
    score sim
  else
    case Simulation.whosTurn sim of
      Just AI ->
        alphabetaMaximizing (availableMoves sim) sim depth a b (-inf)

      Just User ->
        alphabetaMinimizing (availableMoves sim) sim depth a b inf

      Nothing ->
        alphabeta (Simulation.clockTick sim) depth a b


alphabetaMaximizing :: [Command] -> Simulation -> Int -> Double -> Double -> Double -> Double
alphabetaMaximizing !moves !sim !depth !a !b !v =
  case moves of
    m : ms ->
      case Simulation.simulate m sim of
        Just nextSim ->
          let
            nextV =
              max v (alphabeta nextSim (depth - 1) a b)

            nextA =
              max a nextV
          in
            if b < nextA then
              nextV
            else
              alphabetaMaximizing ms sim depth nextA b nextV

        Nothing ->
          alphabetaMaximizing ms sim depth a b v

    [] ->
      v


alphabetaMinimizing :: [Command] -> Simulation -> Int -> Double -> Double -> Double -> Double
alphabetaMinimizing !moves !sim !depth !a !b !v =
  case moves of
    m : ms ->
      case Simulation.simulate m sim of
        Just nextSim ->
          let
            nextV =
              min v (alphabeta nextSim (depth - 1) a b)

            nextB =
              min b nextV
          in
            if nextB < a then
              nextV
            else
              alphabetaMinimizing ms sim depth a nextB nextV

        Nothing ->
          alphabetaMinimizing ms sim depth a b v

    [] ->
      v

x |> f = f x

playAI :: Simulation -> Simulation
playAI !sim =
  let
    explore cmd =
      case Simulation.simulate cmd sim of
        Just nextSim ->
          let
            nextNextSim =
              Simulation.clockTickUntilTurn nextSim
          in
            Just (cmd, nextNextSim, evaluatePosition nextNextSim 4)

        Nothing ->
          Nothing

    (_, s, _) =
      availableMoves sim
        |> mapMaybe explore
        |> List.sortBy (\(_, _, x) (_, _, y) -> (-x) `compare` (-y))
        |> head
  in
    s
