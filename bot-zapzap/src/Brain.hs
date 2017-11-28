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

-- type Length = Integer
type Reward = Rational
type HP = Integer

catNodes :: ClusterMap Mines -> Pos -> [Node]
catNodes cm p =
  List.map snd $
  MinPQueue.toAscList $
  foldClusters cm f MinPQueue.empty p where
    f q d c = foldl' (\q n ->
        MinPQueue.insert (d,(n^.n_pos)`sqdist`p) n q
      )
      q (c^.c_nodes)

data Time = Time { time_int :: Integer }
  deriving(Show,Eq,Ord,Generic,Hashable)

data WealthEvt = WealthEvt {
    we_dmines :: Integer
  , we_dlife :: Integer
  , we_spedings :: Integer
  -- ^ Additional money spendings (e.g. drinking)
  } deriving(Show)

zeroWealthEvt = WealthEvt 0 0 0

-- Absolute reward
gameRewardAbs :: Game -> Hero -> HashMap HeroId (Time,WealthEvt) -> Reward
gameRewardAbs g hh evts =
  let
    rm = foldl f mempty (g.>gameHeroes) where
      f acc h =
        let
          Game{..} = g
          (Time{..},WealthEvt{..}) =
            fromMaybe (Time 1,zeroWealthEvt) $
            HashMap.lookup (h.>heroId) evts

          ta,tb :: Rational
          ta = fromInteger time_int -- time from now to event
          tb = fromInteger (_gameMaxTurns - _gameTurn - time_int) -- time from event to end of game

          r =
             (fromInteger $ h.>heroGold)
           + (fromInteger (h.>heroMineCount)) * ta
           + (fromInteger (h.>heroMineCount + we_dmines)) * tb
           + ((fromInteger (we_spedings)) / ta) * 0.01
           + ((fromInteger (we_dlife)) / ta) * 0.01

        in
        (Map.insert (h.>heroId) r acc)

    (leaderHid, leaderRew) = Map.findMax rm

    rew = rm Map.! (hh.>heroId)
  in
  case leaderHid == hh.>heroId of
    True -> rew
    False -> rew - leaderRew*0.001

gameRewardRel :: Game -> Hero -> HashMap HeroId (Time,WealthEvt) -> Reward
gameRewardRel g hh evts =
  let
    (rm,rmax) = foldl f (mempty,0) (g.>gameHeroes) where
      f (rm,rmax) h =
        let
          Game{..} = g
          (Time{..},WealthEvt{..}) =
            fromMaybe (Time 1,zeroWealthEvt) $
            HashMap.lookup (h.>heroId) evts

          t :: Rational
          t = fromInteger $ time_int

          hm :: Rational
          hm = fromInteger $ h.>heroMineCount

          hl :: Rational
          hl = fromInteger $ h.>heroLife

          r =
             ((fromInteger we_dmines) / t)
           + ((fromInteger we_dlife) / t)*0.01

        in
        (HashMap.insert (h.>heroId) r rm, if h`sameid`hh then rmax else r`max`rmax)
  in
  ((rm) HashMap.! (hh.>heroId)) - rmax*0.1

gameReward = gameRewardAbs

drawPath :: Path -> BImage
drawPath Path{..} = drawPosList p_path

data Plan = Plan {
    goPos :: Pos
  -- ^ Path destination, often it is a cell near the tavern/hero
  , goPath :: [Pos]
  -- ^ Plan waypoints except probably the destination
  , goTile :: Tile
  -- ^ Tile to go
  , goReward :: Reward
  -- ^ Plan reward
  } deriving(Show)

planStep :: Pos -> Plan -> Dir
planStep p Plan{..} =
  case goPath of
    (p':ps) -> posDiff p p'
    [] -> posDiff p goPos

bimagePlan :: Plan -> BImage
bimagePlan Plan{..} = drawPosList goPath -- <> (hmap1 goPos "X ")


describePlans :: MaxPQueue Reward Plan -> Text
describePlans pq =
  execWriter $ do
    forM_ ((MaxPQueue.toDescList pq)`zip`[0..4]) $ \((r,Plan{..}),_) -> do
      tell $ (printTile goTile) <> "," <> tpack (printf "%0.3f" (fromRational r :: Double)) <> " "

killPlans :: Game -> Hero -> ClusterMap Tavs -> MaxPQueue Reward Plan
killPlans g h clt = foldl' f MaxPQueue.empty (gameEnemies g h) where
  f acc h'
    | h'.>heroName == h.>heroName = acc
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

        h't'len = fst <$> fst <$> MinPQueue.minViewWithKey h't'
        ht'len = do
          pos <- (p_pos <$> snd <$> fst <$> MinPQueue.minViewWithKey h't')
          ilength <$> p_path <$> posPath g h pos emptyKillZone

        runaway = fromMaybe True $ do
            p <- ilength <$> path
            return $
              case (ht'len, h't'len) of
                (Nothing, Just l') | (p > l') -> True -- TODO: guard is redundant ?
                (Just l, Just l') | (l > l') -> True
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
      -- trace' (h'id, runaway, likelywin, rew) $
      fromMaybe acc $ MaxPQueue.insert <$> rew <*> (
          Plan h'p <$> path <*> pure (HeroTile h'id) <*> rew
        ) <*> pure acc

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
            -- trace' (tile, (p_pos), likelywin, rew ) $
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

warmup :: Game -> Bot
warmup g =
  let
    cm = clusterize 2 (build (g^.gameBoard) (nodeMines (g^.gameBoard)))
    ct = clusterize 2 (build (g^.gameBoard) (nodeTavs  (g^.gameBoard)))
  in
  Bot cm ct

pmax :: (Ord k) => MaxPQueue k a -> Maybe a
pmax q = fmap fst $ MaxPQueue.maxView q

pmin :: (Ord k) => MinPQueue k a -> Maybe a
pmin q = fmap fst $ MinPQueue.minView q

type PlanQueue = MaxPQueue Reward Plan

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


move :: (Monad m) =>  Bot -> Game -> HeroId -> m (Dir, PlanQueue)
move bot@Bot{..} g hid =
  let
    h = g^.gameHeroes.(idx hid)

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

  in do
  case (nearTavern, needHealing)  of
    (Just tp, True) ->
      trace ("AUTODRINK, " <> planDesc) $
      return (posDiff (h.>heroPos) tp, allplans)
    _ ->
      case plan of
        Just p ->
          trace ("BUSY, " <> planDesc) $
          return (planStep (h.>heroPos) p, allplans)
        Nothing ->
          trace ("WANDER, " <> planDesc) $
          return (South, allplans)


data BotIO = BotIO {
    bot_clust :: !(MVar Bot)
  }

warmupIO :: GameState -> IO BotIO
warmupIO g = do
  mv <- newEmptyMVar
  forkIO $ do
    {- shallow compute cm and ct -}
    putMVar mv =<< do
      evaluate $ force $ warmup (g.>stateGame)

  return $ BotIO mv


moveIO :: (MonadIO m) => BotIO -> GameState -> m (Dir, PlanQueue)
moveIO bs@BotIO{..} gs = do
  liftIO (tryReadMVar bot_clust) >>= \case
    Just b -> do
      (dir,plans) <- move b (gs.>stateGame) (gs.>stateHero.heroId)
      return (force dir, plans)
    Nothing -> do
      return (Stay, mempty)

