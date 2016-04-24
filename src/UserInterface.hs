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
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TemplateHaskell       #-}

module UserInterface where

import           AI.AlphaBeta  as AlphaBeta
import           Combatant     (Combatant)
import qualified Combatant
import           Command       (Command (..), CommandType (..))
import qualified Command
import           Control.Monad (join)
import qualified Data.IntMap   as IntMap
import           Id            (Id)
import           Move          (Move)
import qualified Move
import           Player        (Player (..))
import           Reflex.Dom
import           Simulation    (Simulation)
import qualified Simulation

main :: Simulation -> IO ()
main initialSim = mainWidget . divClass "app" $ do
  let initial = Model { sim = initialSim, mov = Nothing }
  rec changes <- view model
      model <- foldDyn update initial changes
  return ();


data Model = Model
  { sim :: Simulation
  , mov :: Maybe Move
  }


data Action
  = SelectMove Move
  | SelectTarget Id
  | CancelSelection
  deriving (Show)

update :: Action -> Model -> Model
update action model =
  case action of
    SelectMove mv ->
      case Command.typeOfMove mv of
        SingleTargetType ->
          model { mov = Just mv }

        SelfTargetType ->
          case Simulation.simulate (SelfTarget mv) (sim model) of
            Just sim ->
              model { sim = Simulation.clockTickUntilTurn (aiIfNecessary sim), mov = Nothing }

            Nothing ->
              error "this should not be possible"

    SelectTarget id ->
      case mov model of
        Just mov ->
          case Simulation.simulate (SingleTarget mov id) (sim model) of
            Just sim ->
              model { sim = Simulation.clockTickUntilTurn (aiIfNecessary sim), mov = Nothing }

            Nothing ->
              error "this should not be possible"

        Nothing ->
          error "this should not be possible"

    CancelSelection ->
      model { mov = Nothing }


aiIfNecessary :: Simulation -> Simulation
aiIfNecessary sim =
  if Simulation.gameOver sim then
    sim
  else
    case Simulation.whosTurn (Simulation.clockTickUntilTurn sim) of
      Just AI ->
        aiIfNecessary (AlphaBeta.playAI sim)

      Just User ->
        sim

      Nothing ->
        error "this should not be possible"


view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
view model = do
  ev <- divClass "game" $ do
    ev' <- divClass "main" $ do
      divClass "ai-party" $ do
        viewParty AI model;
      ev'' <- divClass "user-party" $ do
        evw' <- viewParty User model;
        return evw'
      log <- mapDyn (Simulation.combatLog . sim) model;
      viewCombatLog log;
      return ev''
    viewCtBar model;
    return ev';
  return ev;

viewCombatLog :: MonadWidget t m => Dynamic t [String] -> m ()
viewCombatLog log =
  divClass "combat-log" $ do
    shortLog <- mapDyn (take 10) log;
    simpleList shortLog viewCombatLogLine;
    return ();

viewCombatLogLine :: MonadWidget t m => Dynamic t String -> m ()
viewCombatLogLine line =
  divClass "combat-log-line" $ do
    dynText line

viewCtBar :: MonadWidget t m => Dynamic t Model -> m ()
viewCtBar model =
  divClass "ct-bar" $ do
    text "Turn Order";
    turnOrder <- mapDyn (Simulation.turnOrderArray . sim) model;
    simpleList turnOrder viewCtBarUnit;
    return ();

viewCtBarUnit :: MonadWidget t m => Dynamic t Combatant -> m ()
viewCtBarUnit cmbt =
  divClass "ct-bar-unit" $ do
    name <- mapDyn Combatant.name cmbt;
    dynText name;


viewParty :: MonadWidget t m => Player -> Dynamic t Model -> m (Event t Action)
viewParty player model = do
  ev <- divClass "party" $ do
    party <- mapDyn (IntMap.elems . Simulation.party player . sim) model;
    list <- simpleList party (viewCombatant player model);
    ev <- mapDyn leftmost list;
    return $ switchPromptlyDyn ev;
  return ev;


viewCombatantStatusBar :: MonadWidget t m => Player -> Dynamic t Model -> Dynamic t Combatant -> m ()
viewCombatantStatusBar player model cmbt =
  divClass "combatant-status-bar" $ do
    elClass "span" "combatant-name" $ do
      name <- mapDyn Combatant.name cmbt;
      dynText name;
    elClass "span" "combatant-class" $ do
      cclass <- mapDyn (show . Combatant.cclass) cmbt;
      dynText cclass;
    divClass "combatant-hp" $ do
      elClass "span" "combatant-hp-label" $ do
        text "HP";
      let int = (id :: Int -> Int);
      hp <- mapDyn (show . int . ceiling . Combatant.hitPoints) cmbt;
      dynText hp;
    viewCombatantAP cmbt;
    divClass "combatant-ct" $ do
      elClass "span" "combatant-ct-label" $ do
        text "CT";
      ct <- mapDyn (show . Combatant.chargeTime) cmbt;
      dynText ct;


viewCombatantMoves :: MonadWidget t m => Player -> Dynamic t Model -> Dynamic t Combatant -> m (Event t Action)
viewCombatantMoves player model cmbt = do
  haveAT <- [mkDyn| Simulation.doIHaveActiveTurn (Combatant.id $cmbt) (sim $model) |];
  component <- [mkDyn| chooseA player $haveAT (mov $model) |];
  ev <- dyn component;
  w <- flattenEv ev;
  return w;
  where
    chooseA User True (Just mv) = viewTargets player model
    chooseA User True Nothing = viewMoves cmbt
    chooseA _ _ _ = do
      blank;
      return never;


viewCombatant :: MonadWidget t m => Player -> Dynamic t Model -> Dynamic t Combatant -> m (Event t Action)
viewCombatant player model cmbt = do
  component <- [mkDyn| chooseC (Combatant.alive $cmbt) |];
  ev <- dyn component;
  w <- flattenEv ev;
  return w;
  where
    chooseC True =
      divClass "combatant combatant-alive" $ do
        viewCombatantStatusBar player model cmbt;
        act <- viewCombatantMoves player model cmbt;
        return act;
    chooseC False =
      divClass "combatant combatant-dead" $ do
        viewCombatantStatusBar player model cmbt;
        act <- viewCombatantMoves player model cmbt;
        return act;


viewCombatantAP :: MonadWidget t m => Dynamic t Combatant -> m ()
viewCombatantAP cmbt =
  divClass "combatant-ap" $ do
    elClass "span" "combatant-ap-label" $ do
      text "AP";
    elClass "span" "combatant-ap-filled" $ do
      dots <- [mkDyn| replicate (Combatant.actionPoints $cmbt) '•'|];
      dynText dots;
    elClass "span" "combatant-ap-empty" $ do
      dots <- [mkDyn| replicate (5 - Combatant.actionPoints $cmbt) '•'|];
      dynText dots;


viewMoves :: MonadWidget t m => Dynamic t Combatant -> m (Event t Action)
viewMoves cmbt = do
  ev <- divClass "combatant-move-list" $ do
    arr <- mapDyn Combatant.moveArray cmbt;
    list <- simpleList arr (viewMove cmbt);
    ev <- mapDyn leftmost list;
    return (switchPromptlyDyn ev);
  return ev;

viewMove :: MonadWidget t m => Dynamic t Combatant -> Dynamic t Move -> m (Event t Action)
viewMove unit mv = do
  cost <- [mkDyn| replicate (Move.cost $mv) '•' |];
  label <- [mkDyn| show $mv ++ " " ++ $cost |];
  component <- [mkDyn| chooseB $unit $mv $label |];
  ev <- dyn component;
  w <- flattenEv ev;
  return w;
  where
    chooseB :: MonadWidget t m => Combatant -> Move -> String -> m (Event t Action)
    chooseB unit' mv' label' =
      if Combatant.actionPoints unit' >= Move.cost mv'
        then divClass "combatant-move" $ do
          onClick <- button label';
          return $ fmap (const $ SelectMove mv') onClick;
        else divClass "combatant-move combatant-move-unusable" $ do
          onClick <- button label';
          return never;

flattenEv :: MonadWidget t m => Event t (Event t Action) -> m (Event t Action)
flattenEv = switchPromptly never

viewTargets :: MonadWidget t m => Player -> Dynamic t Model -> m (Event t Action)
viewTargets player model =
  divClass "combatant-target-list" $ do
    ev1 <- divClass "combatant-target-party" $ do
      arr <- [mkDyn| filter (Combatant.foesOf player) $ (IntMap.elems . Simulation.combatants) (sim $model) |];
      list <- simpleList arr viewTarget;
      ev <- mapDyn leftmost list;
      return (switchPromptlyDyn ev);
    ev2 <- divClass "combatant-target-party" $ do
      arr <- [mkDyn| filter (Combatant.friendsOf player) $ (IntMap.elems . Simulation.combatants) (sim $model) |];
      list <- simpleList arr viewTarget;
      ev <- mapDyn leftmost list;
      return (switchPromptlyDyn ev);
    ev3 <- divClass "combatant-target-cancel" $ do
      onClick <- button "Cancel";
      return $ fmap (const CancelSelection) onClick;
    return $ leftmost [ev1, ev2, ev3];

viewTarget :: MonadWidget t m => Dynamic t Combatant -> m (Event t Action)
viewTarget cmbt = do
  component <- [mkDyn| chooseD (Combatant.alive $cmbt) (Combatant.name $cmbt) (Combatant.id $cmbt) |];
  ev <- dyn component;
  w <- flattenEv ev;
  return w;
  where
    chooseD True name id_ = do
      divClass "combatant-target combatant-target-alive" $ do
        onClick <- button name;
        return $ fmap (const $ SelectTarget id_) onClick;

    chooseD False name _ =
      divClass "combatant-target combatant-target-dead" $ do
        button name;
        return never;
