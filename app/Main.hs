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

module Main where

import           Class         (Class (..))
import           Combatant     (Combatant)
import qualified Combatant
import           Id            (Id (..), fromId)
import           Player        (Player (..))
import           Simulation    (Simulation (..))
import qualified Simulation
import           UserInterface hiding (main)
import qualified UserInterface

import           Data.IntMap   (IntMap)
import qualified Data.IntMap   as IntMap

initialCombatants :: IntMap Combatant
initialCombatants =
  let
    warrior =
      Combatant.mkCombatant Warrior

    thief =
      Combatant.mkCombatant Thief

    cleric =
      Combatant.mkCombatant Cleric
  in
    IntMap.fromList $
      (\c -> (((fromId . Combatant.id)) c, c)) <$>
        [ warrior { Combatant.player = User, Combatant.id = Id 0, Combatant.name = "Alpha" }
        , thief { Combatant.player = User, Combatant.id = Id 1, Combatant.name = "Beta" }
        , cleric { Combatant.player = User, Combatant.id = Id 2, Combatant.name = "Gamma" }
        , warrior { Combatant.player = AI, Combatant.id = Id 3, Combatant.name = "Delta" }
        , thief { Combatant.player = AI, Combatant.id = Id 4, Combatant.name = "Epsilon" }
        , cleric { Combatant.player = AI, Combatant.id = Id 5, Combatant.name = "Zeta" }
        ]

initialSim :: Simulation
initialSim =
  Simulation.clockTickUntilTurn $ Simulation
    { Simulation.combatLog = ["This", "is", "only", "a", "test"]
    , Simulation.activeCombatant = Nothing
    , Simulation.combatants = initialCombatants
    }

main :: IO ()
main = UserInterface.main initialSim
