{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Brain where

import Data.Graph.AStar
import Control.DeepSeq
import Data.PQueue.Prio.Min(MinPQueue)
import qualified Data.PQueue.Prio.Min as MinPQueue
import qualified Data.PQueue.Prio.Max as MaxPQueue

import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map

import Types
import Imports
import Voronoy
import Astar
import Cli

data Time = Time { time_int :: Integer }
  deriving(Show,Eq,Ord,Generic,Hashable)

type Reward = Double

gameTimeLeft :: Game -> Integer
gameTimeLeft g = (g.>gameMaxTurns) - (g.>gameTurn)

gameReward1 :: Game -> Hero -> Reward
gameReward1 g h =
     (n2d (h.>heroGold))
   + (n2d (h.>heroMineCount) * n2d (gameTimeLeft g))
   + (n2d (h.>heroLife) / 20.0)

gameReward :: Game -> Hero -> Reward
gameReward g h = reward1 + leaderbonus
  where
    reward1 = gameReward1 g h
    leaderbonus =
      flip3 foldr (gameEnemies g h) 0 $ \h' acc ->
        acc + (if reward1 > (gameReward1 g h') then 20.0 else 0.0)

data Plan = Plan {
    planPath :: Path
  -- ^ Plan waypoints except probably the destination
  , planTile :: Tile
  -- ^ Tile to go
  , planSafe :: Bool
  -- ^ True if plan path hits enemy killZone
  } deriving(Show)

planTgt = p_pos . planPath

drawPlan :: Plan -> BImage
drawPlan Plan{..} = drawPath planPath

planLastPos :: Pos -> Plan -> Pos
planLastPos def Plan{..} = pathLastPos def planPath

planStep :: Pos -> Plan -> Dir
planStep p Plan{..} =
  case p_path planPath of
    (p':ps) -> posDiff p p'
    [] -> posDiff p (p_pos planPath)

-- type PlanQueue = MaxPQueue Reward Plan

describePlan :: Plan -> Text
describePlan Plan{..} = (printTileC planTile) <> printPos (p_pos planPath)

describePlans :: (Foldable t) => t Plan -> Text
describePlans pq =
  execWriter $ do
    forM_ (toList pq) $ \p -> do
      tell $ describePlan p <> " "

drawGamePlans :: (Foldable t) => t Plan -> BImage
drawGamePlans plans =
  let l = toList plans in
  HashMap.fromList $
      (flip map (take 5 l) $ \(plan@Plan{..}) ->
        (planTgt plan, Left clrDef_White))
      <> [(planTgt (head l), Left clrDef_Red)]


-- | Search for nearby mines available for capturing
capturePlans :: Game -> Hero -> [HeroId] -> ClusterMap Mines -> [Plan]
capturePlans g h allies cm =
  flip4 foldGoals cm (h.>heroPos) mempty $ \d (node,goal) acc -> if
    |d>2 && (not $ null acc) -> acc
    |otherwise ->
      case gameTile g (goal.>goalPos) of
        gt@(MineTile (Just mhid))
          |mhid == h.>heroId -> acc
          |(mhid`elem`allies) && (h.>heroMineCount > 2) -> acc
          |otherwise ->
            let
              safeplans a =
                flip3 foldr (astarNode g h node (enemiesKillZone g h)) a $ \path acc' ->
                  (Plan path gt True):acc'
              unsafeplans a =
                flip3 foldr (astarNode g h node emptyKillZone) a $ \path acc' ->
                  (Plan path gt False):acc'
            in
            safeplans (unsafeplans acc)
        otherwise -> acc


{-
 foldClusters cm f MaxPQueue.empty (h.>heroPos) where
  f mpq d c
    | d > 2 = mpq
    | otherwise =
        let
          hid = h.>heroId
          hl = h.>heroLife
          hm = h.>heroMineCount

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

          danger = fst (nearestEnemy g h) <= 2
          h' = snd (nearestEnemy g h)
          h'id = h'.>heroId

        in
        flip execState mpq $ do
          forM_ (nodesPath g h nodes) $ \ pp@Path{..} ->
            let
              t = Time (pathLength pp)
              likelywin = (hl - (pathLength pp)) > 21

              adjacent = pathLength pp == 1

              tile = (g.>gameTiles) HashMap.! (p_pos)

              mrew =
                case (danger, adjacent, likelywin, tile) of
                  (True, False, False, _) ->
                    Nothing

                  (_, True, False, _) ->
                    Just $ gameReward g h (HashMap.fromList [(hid, (t,WealthEvt (-hm) (100-hl) 0))])

                  (False, _, True, MineTile (Just h'id))
                    | g.>gameHeroes.(idx h'id).heroName == h.>heroName
                      && (h.>heroMineCount >= 2) -> Nothing
                    | otherwise ->
                      Just $ gameReward g h (HashMap.fromList [(hid, (t,WealthEvt 1 (-20) 0)),
                                                               (h'id, (t,WealthEvt (-1) 0 0)) ])
                  (False, _, True, MineTile Nothing) ->
                    Just $ gameReward g h (HashMap.fromList [(hid, (t,WealthEvt 1 (-20) 0))])

                  _ -> Nothing
            in do
            case mrew of
              Just rew ->
                modify $ MaxPQueue.insert rew (Plan p_pos p_path tile rew)
              Nothing ->
                return ()

-- TODO * measure bot performance per plan


capturePlans :: Game -> Hero -> ClusterMap Mines -> MaxPQueue Reward Plan
capturePlans g h cm = foldClusters cm f MaxPQueue.empty (h.>heroPos) where
  f mpq d c
    | d > 2 = mpq
    | otherwise =
        let
          hid = h.>heroId
          hl = h.>heroLife
          hm = h.>heroMineCount

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

          danger = fst (nearestEnemy g h) <= 2
          h' = snd (nearestEnemy g h)
          h'id = h'.>heroId

        in
        flip execState mpq $ do
          forM_ (nodesPath g h nodes) $ \ pp@Path{..} ->
            let
              t = Time (pathLength pp)
              likelywin = (hl - (pathLength pp)) > 21

              adjacent = pathLength pp == 1

              tile = (g.>gameTiles) HashMap.! (p_pos)

              mrew =
                case (danger, adjacent, likelywin, tile) of
                  (True, False, False, _) ->
                    Nothing

                  (_, True, False, _) ->
                    Just $ gameReward g h (HashMap.fromList [(hid, (t,WealthEvt (-hm) (100-hl) 0))])

                  (False, _, True, MineTile (Just h'id))
                    | g.>gameHeroes.(idx h'id).heroName == h.>heroName
                      && (h.>heroMineCount >= 2) -> Nothing
                    | otherwise ->
                      Just $ gameReward g h (HashMap.fromList [(hid, (t,WealthEvt 1 (-20) 0)),
                                                               (h'id, (t,WealthEvt (-1) 0 0)) ])
                  (False, _, True, MineTile Nothing) ->
                    Just $ gameReward g h (HashMap.fromList [(hid, (t,WealthEvt 1 (-20) 0))])

                  _ -> Nothing
            in do
            case mrew of
              Just rew ->
                modify $ MaxPQueue.insert rew (Plan p_pos p_path tile rew)
              Nothing ->
                return ()

killPlan :: Game -> Hero -> Hero -> ClusterMap Tavs -> Maybe Plan
killPlan g h h' clt
  | h'.>heroName == h.>heroName = Nothing
  | otherwise =
    let
      path = safeAstar g h (boardAdjascentAvail h'p (g.>gameBoard)) emptyKillZone

      h'p = h'.>heroPos

      hid = h.>heroId
      h'id = h'.>heroId

      hl = h.>heroLife
      h'l = h'.>heroLife

      hm = h.>heroMineCount
      h'm = h'.>heroMineCount

      h't' = nearestTaverns g h' clt

      h't'loc = MinPQueue.minViewWithKey h't'

      h't'len = fst <$> fst <$> h't'loc
      ht'len = do
        pos <- (pathLastPos h'p <$> snd <$> fst <$> MinPQueue.minViewWithKey h't')
        ilength <$> p_path <$> posPath g h pos emptyKillZone

      runaway = fromMaybe True $ do
          p <- ilength <$> path
          return $
            case (p <= 1, ht'len, h't'len) of
              (True, _, _) -> False
              (_, Nothing, Just l') -> True
              (_, Just l, Just l') | (l > l') -> True
              _ -> False

      likelywin
        | runaway = False
        | (h.>heroPos) == (h.>heroSpawnPos) = True
        | (recdist (h'.>heroPos) (h.>heroSpawnPos)) <= 1 = True
        | (recdist (h.>heroPos) (h'.>heroSpawnPos)) <= 1 = False
        | (recdist (h'.>heroPos) (h'.>heroSpawnPos)) <= 1 = False
        | hid < h'id && hl < h'l = False
        | hid > h'id && hl <= h'l = False
        | otherwise = True

      rew = do
        p <- path
        t <- (Time . (+1) . toInteger . length) <$> path
        return $ do
        if likelywin
          then gameReward g h (HashMap.fromList [(hid,  (t,WealthEvt h'm (-(hl-h'l)) 0)),
                                                 (h'id, (t,WealthEvt (-h'm) (100-h'l) 0))
                                                ])
          else gameReward g h (HashMap.fromList [(hid,  (t,WealthEvt (-hm) (100-hl) 0)),
                                                 (h'id, (t,WealthEvt hm (-(h'l-hl)) 0))
                                                ])


    in
    Plan h'p <$> path <*> pure (HeroTile h'id) <*> rew


killPlans :: Game -> Hero -> ClusterMap Tavs -> MaxPQueue Reward Plan
killPlans g h clt = foldl' f MaxPQueue.empty (gameEnemies g h) where
  f acc h' =
    let
      p = killPlan g h h' clt
    in
    fromMaybe acc (MaxPQueue.insert <$> (goReward <$> p) <*> p <*> pure acc)

capturePlans :: Game -> Hero -> ClusterMap Mines -> MaxPQueue Reward Plan
capturePlans g h cm = foldClusters cm f MaxPQueue.empty (h.>heroPos) where
  f mpq d c
    | d > 2 = mpq
    | otherwise =
        let
          hid = h.>heroId
          hl = h.>heroLife
          hm = h.>heroMineCount

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

          danger = fst (nearestEnemy g h) <= 2
          h' = snd (nearestEnemy g h)
          h'id = h'.>heroId

        in
        flip execState mpq $ do
          forM_ (nodesPath g h nodes) $ \ pp@Path{..} ->
            let
              t = Time (pathLength pp)
              likelywin = (hl - (pathLength pp)) > 21

              adjacent = pathLength pp == 1

              tile = (g.>gameTiles) HashMap.! (p_pos)

              mrew =
                case (danger, adjacent, likelywin, tile) of
                  (True, False, False, _) ->
                    Nothing

                  (_, True, False, _) ->
                    Just $ gameReward g h (HashMap.fromList [(hid, (t,WealthEvt (-hm) (100-hl) 0))])

                  (False, _, True, MineTile (Just h'id))
                    | g.>gameHeroes.(idx h'id).heroName == h.>heroName
                      && (h.>heroMineCount >= 2) -> Nothing
                    | otherwise ->
                      Just $ gameReward g h (HashMap.fromList [(hid, (t,WealthEvt 1 (-20) 0)),
                                                               (h'id, (t,WealthEvt (-1) 0 0)) ])
                  (False, _, True, MineTile Nothing) ->
                    Just $ gameReward g h (HashMap.fromList [(hid, (t,WealthEvt 1 (-20) 0))])

                  _ -> Nothing
            in do
            case mrew of
              Just rew ->
                modify $ MaxPQueue.insert rew (Plan p_pos p_path tile rew)
              Nothing ->
                return ()

drinkPlans :: Game -> Hero -> ClusterMap Tavs -> MaxPQueue Reward Plan
drinkPlans g h clt = foldl f mempty (nearestTaverns g h clt) where
  f mpq pp@Path{..} =
    let
      hl = h.>heroLife
      hid = h.>heroId
      t = Time (pathLength pp)
      rew = gameReward g h (HashMap.fromList [(hid, (t, WealthEvt 0 (100 - hl) (-2)))])
      pl = Plan p_pos p_path TavernTile rew
    in
    MaxPQueue.insert rew pl mpq

passPlans :: Game -> Hero -> PlanQueue
passPlans g h =
  let
    hid = h.>heroId
    hl = h.>heroLife
    rew = gameReward g h (HashMap.fromList [(hid, (Time 1, zeroWealthEvt))])
  in
  MaxPQueue.singleton rew $
    Plan (h.>heroPos) [] (HeroTile (h.>heroId)) rew


data Bot = Bot {
    bot_clm :: !(ClusterMap Mines)
  , bot_clt :: !(ClusterMap Tavs)
  } deriving(Show,Generic, NFData)

warmup :: GameState -> Bot
warmup gs =
  let
    g = gs.>stateGame
    cm = clusterize 2 (build (g^.gameBoard) (nodeMines (g^.gameBoard)))
    ct = clusterize 2 (build (g^.gameBoard) (nodeTavs  (g^.gameBoard)))
  in
  Bot cm ct

data Plans = Plans {
    plans_kill :: PlanQueue
  , plans_capture :: PlanQueue
  , plans_drink :: PlanQueue
  , plans_pass :: PlanQueue
  }


think :: Bot -> Game -> HeroId -> Plans
think Bot{..} g hid = Plans{..} where
  h = g^.gameHeroes.(idx hid)
  clm = bot_clm
  clt = bot_clt
  plans_kill = killPlans g h clt
  plans_capture = capturePlans g h clm
  plans_drink = drinkPlans g h clt
  plans_pass = passPlans g h


move :: Bot -> GameState -> (Dir, PlanQueue)
move bot@Bot{..} gs =
  let
    g = gs.>stateGame
    h = gs.>stateHero
    hid = h.>heroId

    Plans{..} = think bot g hid

    allplans = plans_kill <>  plans_capture <> plans_drink <> plans_pass

    nearTavern =
        boardAdjascent
          (\acc (t,p) -> case t of { TavernTile -> Just p ; _ -> acc})
          Nothing (h.>heroPos) (g.>gameBoard)

    needHealing =
      ((h.>heroLife) < 90) && ((h.>heroGold) > 4)

    plan = pmax allplans

    planDesc =
      case plan of
        Just p -> ("PLAN " <> tunpack (printTile (goTile p)) <> " " <> show (goPos p))
        Nothing -> "NOPLAN"

  in
  case (nearTavern, needHealing)  of
    (Just tp, True) ->
      trace ("AUTODRINK, " <> planDesc) $
      (posDiff (h.>heroPos) tp, allplans)
    _ ->
      case plan of
        Just p ->
          trace ("BUSY, " <> planDesc) $
          (planStep (h.>heroPos) p, allplans)
        Nothing ->
          trace ("WANDER, " <> planDesc) $
          (South, allplans)


data BotIO = BotIO {
    bot_clust :: !(MVar Bot)
  }

warmupIO :: GameState -> IO BotIO
warmupIO gs = do
  mv <- newEmptyMVar
  forkIO $ do
    {- shallow compute cm and ct -}
    putMVar mv =<< do
      evaluate $ force $ warmup gs

  return $ BotIO mv


moveIO :: (MonadIO m) => BotIO -> GameState -> m (Dir, PlanQueue)
moveIO bs@BotIO{..} gs = do
  liftIO (tryReadMVar bot_clust) >>= \case
    Just b -> do
      (dir,plans) <- pure $ move b gs
      return (force dir, plans)
    Nothing -> do
      return (Stay, mempty)

-}
