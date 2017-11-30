{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
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
import Cli

data Path = Path {
    p_pos :: Pos
  , p_path :: [Pos]
  } deriving(Show,Eq,Ord)

type Length = Integer
type MaxDist = Integer
type PathQueue = MinPQueue Length Path

pathInsert p@Path{..} q = MinPQueue.insert (pathLength p) p q

-- | Return last position of a plan, usually a point near mine/tavern/hero
-- `def` is a default location to return in case of empty plan
pathLastPos :: Pos -> Path -> Pos
pathLastPos def Path{..} | null p_path = def
                         | otherwise = last p_path

pathLength :: Path -> Integer
pathLength Path{..} = toInteger $ 1 + length p_path

pathMin :: PathQueue -> Maybe Path
pathMin q = fmap fst $ MinPQueue.minView q

drawPosList :: [Pos] -> BImage
drawPosList ps = HashMap.fromList $ ps`zip`(repeat (Right "x "))

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
-- Note 1: This version uses mean distance across target position, so targets should
-- not be too far from each other.
safeAstar :: Game -> Hero -> HashSet Pos -> KillZone -> [[Pos]]
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
            (_, _) -> acc
        )
        HashSet.empty p b

    dist1 _ _ = 1

    heu p = sqdist p tgt_mean

    isgoal p = p`HashSet.member`tgts

    from = h^.heroPos
  in
  maybeToList $ aStar near dist1 heu isgoal from

-- | Call astar towards set of nodes. Returns family of `Path` objects that have
-- same (shortest) path and different goals.
astarNode :: Game -> Hero -> Node -> KillZone -> PathQueue
astarNode g h node kz =
  flip3 foldr (safeAstar g h (HashSet.singleton $ node.>n_center) kz) mempty $ \poslist paths2 ->
    flip3 foldr (node.>n_goals) paths2 $ \goal paths3 ->
      pathInsert (Path (goal.>go_pos) poslist) paths3

-- | Call astar towards set of nodes. Returns family of `Path` objects that have
-- same (shortest) path and different goals.
astarNodes :: Game -> Hero -> HashSet Node -> KillZone -> PathQueue
astarNodes g h ntgts kz =
  flip3 foldr ntgts mempty $ \node paths ->
    (astarNode g h node kz) <> paths


-- | Search a path to a position @pos@, with respect to killzone @kz@
astarPos :: Game -> Hero -> Pos -> KillZone -> PathQueue
astarPos g h pos kz =
  flip3 foldr (safeAstar g h (HashSet.singleton pos) kz) mempty $ \poslist q ->
    pathInsert (Path pos poslist) q


-- | Calls astar towards specific hero
heroPath :: Game -> Hero -> Hero -> KillZone -> PathQueue
heroPath g h h' kz =
  flip3 foldr (safeAstar g h (gameAdjascentAvail (h'.>heroPos) g) kz) mempty $ \poslist q ->
    pathInsert (Path (h'.>heroPos) poslist) q

-- | Search for nearby mines available for capturing
nearestMines :: Game -> Hero -> ClusterMap Mines -> KillZone -> PathQueue
nearestMines g h cm kz =
  flip4 foldGoals cm (h.>heroPos) mempty $ \d (node,goal) acc -> if
    | d>2 && (not $ null acc) -> acc
    | ((g.>gameTiles) HashMap.! (goal.>go_pos)) == MineTile (Just $ h.>heroId) -> acc
    | otherwise ->
      flip3 foldr (astarNode g h node kz) acc $ \path acc2 ->
        MinPQueue.insert (pathLength path) path acc2


-- | Search for nearest taverns, with respect to killZone @kz@
-- FIXME * looks like adjucent taverns were in zapzap's blind spot, check the
-- FIXME   current behaviour
nearestTaverns :: Game -> Hero -> ClusterMap Tavs -> KillZone -> PathQueue
nearestTaverns g h ct kz =
  flip4 foldClusters ct (h.>heroPos) MinPQueue.empty $ \d cluster q -> if
    | d>2 && (not $ null q) -> q
    | otherwise ->
        flip3 foldr (astarNodes g h (cluster.>c_nodes) kz) q $ \path q2 ->
          pathInsert path q2




