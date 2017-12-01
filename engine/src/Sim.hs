{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NondecreasingIndentation #-}
module Sim (
    sim, sim'
  , simGrabMine
  , simKillHero
  , simMoveHero
  ) where

import qualified Data.List as List
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.PQueue.Prio.Min as MinPQueue
import qualified Data.Set as Set
import qualified Control.Lens as Lens
import qualified Prelude as Prelude
import Prelude hiding (break)

import Imports
import Types

data SimState = SimState {
    _ss_killed :: Set HeroId
  , _ss_game :: Game
  } deriving(Show)

$(makeLenses ''SimState)

initState g = SimState Set.empty g

type Sim a = State SimState a

incHP val hp = (((hp+val)`min`100)`max`1)

incGold val gld = (gld+val)`max`0

tiles :: Lens' SimState (HashMap Pos Tile)
tiles = board.bo_tiles

mines :: Lens' SimState (HashMap HeroId (HashSet Pos))
mines = board.bo_heroMines

hero :: HeroId -> Lens' SimState Hero
hero hid = ss_game.gameHeroes.(idx hid)

board :: Lens' SimState Board
board = ss_game.gameBoard

moveHero :: HeroId -> Pos -> Sim ()
moveHero hid p' = do
  p <- use ((hero hid).heroPos)
  t <- use (tiles.(idx p))
  when (t == (HeroTile hid)) $ do
    tiles.(idx p) %= const FreeTile
  tiles.(idx p') %= const (HeroTile hid)
  (hero hid).heroPos %= const p'

simMoveHero :: Game -> Hero -> Pos -> Game
simMoveHero g h pos = (execState (moveHero (h.>heroId) pos) (SimState mempty g)).>ss_game

grabMine :: HeroId -> Pos -> Sim ()
grabMine hid p = do
  mine <- (HashMap.!p) <$> use tiles
  case mine of
    MineTile (Just hid')
      | hid == hid' ->
        return ()
      | otherwise -> do
          uses ((hero hid).heroLife) (<=20) >>= \case
            True -> do
              killHero Nothing hid
            False -> do
              (hero hid').heroMineCount %= (+(-1))
              mines %= HashMap.adjust (HashSet.delete p) hid'
              (hero hid).heroLife %= incHP (-20)
              (hero hid).heroMineCount %= (+1)
              tiles %= HashMap.insert p (MineTile (Just hid))
              mines %= HashMap.insertWith (<>) hid (HashSet.singleton p)
              return ()
    MineTile Nothing -> do
      uses ((hero hid).heroLife) (<=20) >>= \case
        True -> killHero Nothing hid
        False -> do
          (hero hid).heroLife %= incHP (-20)
          (hero hid).heroMineCount %= (+1)
          tiles %= HashMap.insert p (MineTile (Just hid))
          mines %= HashMap.insertWith (<>) hid (HashSet.singleton p)
    _ -> assert 3 False


simGrabMine :: Game -> Hero -> Pos -> Game
simGrabMine g h pos = (execState (grabMine (h.>heroId) pos) (SimState mempty g)).>ss_game

-- | A @killer@ kills Hero @hid@ (a victim), the result may include chain death
-- via telefragging
killHero :: Maybe HeroId -> HeroId -> Sim ()
killHero killer hid = do
  ss_killed %= Set.insert hid
  (hero hid).heroLife %= const 100
  (hero hid).heroMineCount %= const 0
  ms <- (HashMap.! hid) <$> use mines
  forM_ ms $ \p -> do
    tiles %= HashMap.insert p (MineTile killer)
  mines %= HashMap.insert hid HashSet.empty
  case killer of
    Nothing -> return ()
    Just hid' -> do
      mines %= HashMap.insertWith mappend hid' ms
      (hero hid').heroMineCount %= (+(fromIntegral $ HashSet.size ms))
  sp <- use ((hero hid).heroSpawnPos)
  t <- (HashMap.! sp) <$> use tiles
  case t of
    HeroTile hid'
      | hid' == hid -> return ()
      | otherwise -> do
          dead_hid' <- uses ss_killed (Set.member hid')
          when (not dead_hid') $ do
            killHero (Just hid) hid'
    FreeTile -> return ()
    _ -> assert 2 False
  moveHero hid sp

simKillHero :: Game -> Maybe Hero -> Hero -> Game
simKillHero g killer h = (execState (killHero ((.>heroId)<$>killer) (h.>heroId)) (SimState mempty g)).>ss_game


sim :: HeroId -> Dir -> Game -> Game
sim hid d g = sim' hid (Just d) False g

sim' :: HeroId -> Maybe Dir -> Bool -> Game -> Game
sim' hid mdir crashed g | g^.gameFinished = g
                        | otherwise =
  view ss_game <$>
  flip execState (initState g) $ do

    old_crashed <- use ((hero hid).heroCrashed)

    let just_crashed = crashed && (not $ old_crashed)

    case (just_crashed, mdir) of
      (True, _) -> do
        (hero hid).heroCrashed %= const True
      (False, Nothing) ->
        return ()
      (_, Just dir) -> do

        p <- use ((hero hid).heroPos)

        p' <- pure $
          if old_crashed then p
          else boardMove (g^.gameBoard) p dir

        {- Movement -}
        (hero hid).heroLastDir %= const (Just dir)
        t <- (HashMap.! p') <$> use tiles
        case t of
          HeroTile hid' -> do
             return ()
          FreeTile -> do
            moveHero hid p'
            return ()
          MineTile _ -> do
            grabMine hid p'
            return ()
          TavernTile -> do
            gold <- use ((hero hid).heroGold)
            when (gold >= 2) $ do
              (hero hid).heroGold %= incGold (-2)
              (hero hid).heroLife %= incHP 50
          _ -> do
            return ()

    {- Fight resolution -}
    b <- use board
    p' <- use ((hero hid).heroPos)
    let hs = sortBy (compare `on` carouselDist4 hid) $ boardAdjascentHeroes p' b
    forM_ hs $ \hid' -> do
      dead_hid <- uses ss_killed (Set.member hid)
      dead_hid' <- uses ss_killed (Set.member hid')
      when ((not dead_hid) && (not dead_hid')) $ do
        uses ((hero hid').heroLife) (<=20) >>= \case
          True -> do
            killHero (Just hid) hid'
          False -> do
            (hero hid').heroLife %= incHP (-20)

    {- Mining -}
    nmines <- use ((hero hid).heroMineCount)
    (hero hid).heroGold %= incGold nmines

    {- Thirst -}
    (hero hid).heroLife %= incHP (-1)

    {- Turn -}
    ss_game.gameTurn %= (+1)
    t <- use (ss_game.gameTurn)
    mt <- use (ss_game.gameMaxTurns)
    when (t >= mt) $ do
      ss_game.gameFinished %= const True



