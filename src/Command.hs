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
module Command (Command(..), CommandType(..), typeOfMove, commandType) where

import           Id (Id)
import           Move (Move(..))

data Command = SingleTarget Move Id
             | SelfTarget Move

data CommandType = SingleTargetType
                 | SelfTargetType

typeOfMove :: Move -> CommandType
typeOfMove Attack = SingleTargetType
typeOfMove Heal = SingleTargetType
typeOfMove Defend = SelfTargetType

commandType :: Command -> CommandType
commandType (SingleTarget _ _) = SingleTargetType
commandType (SelfTarget _) = SelfTargetType
