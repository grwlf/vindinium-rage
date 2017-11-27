{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Astar where

import qualified Data.List as List
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Prelude as Prelude
import Prelude hiding (break)
import qualified Data.PQueue.Prio.Min as MinPQueue
import qualified Data.PQueue.Prio.Max as MaxPQueue

import Data.Graph.AStar

import Imports
import Types
import Voronoy

data Path = Path {
    p_pos :: Pos
  , p_path :: [Pos]
  } deriving(Show,Eq,Ord)

pathLength :: Path -> Integer
pathLength Path{..} = toInteger $ 1 + length p_path

drawPosList :: [Pos] -> BImage
drawPosList ps = HashMap.fromList $ ps`zip`(repeat "x ")


drawPath :: Path -> BImage
drawPath Path{..} = drawPosList p_path

type KillZone = HashSet Pos

killZone :: Game -> [Hero] -> KillZone
killZone g = foldl' (\acc h' -> acc <> (heroKillZone g h')) emptyKillZone

enemiesKillZone :: Game -> Hero -> KillZone
enemiesKillZone g h = killZone g (gameEnemies g h)

emptyKillZone :: KillZone
emptyKillZone = mempty


-- | Searches safe path from the current hero to the position @tgt@, ignore
-- killzone positions
--
-- This version uses mean distance, so targets should not be too far from each
-- other.
safeAstar :: Game -> Hero -> HashSet Pos -> KillZone -> Maybe [Pos]
safeAstar g h tgts kz =
  let
    b = g.>gameBoard

    tgt_mean = meanPos tgts

    near p =
      boardAdjascent
        (\acc (t,p) ->
          let
            safe = boardAdjascent (\acc (t,_) -> acc || t == TavernTile) False p b
            danger = boardAdjascent (\acc (_,p) -> acc || p`HashSet.member`kz) False p b
          in
          case (t, safe || not danger) of
            (FreeTile, True) -> HashSet.insert p acc
            (_, True) -> acc
            (_, False) -> acc
        )
        HashSet.empty p b

    dist1 _ _ = 1

    heu p = sqdist p tgt_mean

    isgoal p = p`HashSet.member`tgts

    from = h^.heroPos
  in
  aStar near dist1 heu isgoal from

-- | Calls astar towards set of nodes. Returns family of `Path` objects that have
-- same (shortest) path and different goals.
--
-- FIXME: Fix Astar, allow payload for graph vertexes
safeAstarNodes' :: Game -> Hero -> HashSet Node -> KillZone -> [Path]
safeAstarNodes' g h ntgts kz =
  let
    gs = foldl' (\acc Node{..} -> HashMap.insertWith (<>) _n_center _n_goals acc) HashMap.empty ntgts
  in
  case safeAstar g h (HashSet.map _n_center ntgts) kz of
    Just pp ->
      let
        goals = toList $ gs HashMap.! (if null pp then h.>heroPos else last pp)
      in
      map (\g -> Path (g.>go_pos) pp) goals
    Nothing -> []


posPath :: Game -> Hero -> Pos -> KillZone -> Maybe Path
posPath g h pos kz =
  Path pos <$> do
    safeAstar g h (HashSet.singleton pos) kz

-- | Search for shortest path, return path per goal
nodesPath :: Game -> Hero -> HashSet Node -> [Path]
nodesPath g h t = safeAstarNodes' g h t (enemiesKillZone g h)

-- | Search for shortest path, return path per goal (unsafe)
nodesPathUnsafe :: Game -> Hero -> HashSet Node -> [Path]
nodesPathUnsafe g h t = safeAstarNodes' g h t emptyKillZone


-- | Calls astar towards specific hero
heroPath :: Game -> Hero -> Hero -> Maybe Path
heroPath g h h' =
  Path (h'.>heroPos) <$> do
    safeAstar g h (gameAdjascentAvail (h'.>heroPos) g) (enemiesKillZone g h)

-- | Calls astar towards specific hero (unsafe)
heroPathUnsafe :: Game -> Hero -> Hero -> Maybe Path
heroPathUnsafe g h h' =
  Path (h'.>heroPos) <$> do
    safeAstar g h (gameAdjascentAvail (h'.>heroPos) g) emptyKillZone


type Length = Integer
type MaxDist = Integer

-- | Searches for nearby mines available for capturing
nearestMines' :: Game -> Hero -> ClusterMap Mines -> KillZone -> MinPQueue Length Path
nearestMines' g h cm kz = foldClusters cm f mempty (h.>heroPos) where
  f mpq d c
    | d > 2 = mpq
    | otherwise =
        let
          hid = h.>heroId

          nodes =
            foldl' (\acc n ->
              let
                goals = flip HashSet.filter (n.>n_goals) $ \go ->
                  ((g.>gameTiles) HashMap.! (go.>go_pos)) /= MineTile (Just hid)
              in
              case HashSet.null goals of
                False -> HashSet.insert n{_n_goals = goals} acc
                True -> acc
              ) mempty (c.>c_nodes)
        in
        foldl' (\acc path -> MinPQueue.insert (pathLength path) path acc)
               mpq (safeAstarNodes' g h nodes kz)

nearestMines g h cm = nearestMines' g h cm (enemiesKillZone g h)

nearestMinesUnsafe g h cm = nearestMines' g h cm (emptyKillZone)




-- FIXME: looks like adjucent taverns are in the blind spot
nearestTaverns' :: Game -> Hero -> ClusterMap Tavs -> KillZone -> MinPQueue Length Path
nearestTaverns' g h ct kz = foldClusters ct f MinPQueue.empty (h.>heroPos) where
  f mpq d c
    | d > 2 = mpq
    | otherwise =
        foldl' (\acc path -> MinPQueue.insert (pathLength path) path acc)
               mpq (safeAstarNodes' g h (c.>c_nodes) kz)

nearestTaverns g h ct = nearestTaverns' g h ct (enemiesKillZone g h)

nearestTavernsUnsafe g h ct = nearestTaverns' g h ct (emptyKillZone)



