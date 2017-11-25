{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Voronoy where

import qualified Data.List as List
import Data.HashMap.Strict(HashMap,(!))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Control.Monad.State.Strict as S
import Imports hiding(State,(!))
import Types hiding (State)
import Prelude hiding (break,print)
import Data.Char

import qualified Data.Map.Lazy as MapL

-- | Game location, a tavern or a mine
data Goal = Goal {
    _go_id :: Integer
  -- ^ User-defined ID
  , _go_pos :: Pos
} deriving(Eq,Show,Ord,Generic,NFData,Hashable)

$(makeLenses ''Goal)

-- | Node is an entry point, either to the Mine or to the Tavern. Different
-- nodes may share a single Goal. Single Node may refer to several goals
data Node = Node {
    _n_center :: Pos
  -- ^ Center of a node
  , _n_goals :: HashSet Goal
  -- ^ Special points near the Node
} deriving(Eq,Show,Generic,NFData,Hashable)

$(makeLenses ''Node)

n_pos :: Lens' Node Pos
n_pos = n_center

-- Group is a tightly coupled set of nodes
type Group = [Node]

instance Ord Node where
  compare n1 n2 = (n1^.n_center)`compare`(n2^.n_center)

initNodes b oid ps = do
  forM_ ps $ \ pg -> do
    forM_ (boardAdjascentAvail pg b) $ \ p -> do
      modify $ HashMap.insertWith HashSet.union p
        (HashSet.singleton $ Goal oid pg)

mineObjId = 1
tavObjId = 2

-- | Collects board node points
nodeMineTav :: Board -> HashMap Pos Node
nodeMineTav b =
  HashMap.mapWithKey Node $
  flip execState HashMap.empty $ do
    initNodes b mineObjId (b^.bo_mines)
    initNodes b tavObjId (b^.bo_taverns)

-- | Collects board node points
nodeMines :: Board -> HashMap Pos Node
nodeMines b =
  HashMap.mapWithKey Node $
  flip execState HashMap.empty $ do
    initNodes b mineObjId (b^.bo_mines)

-- | Collects board node points
nodeTavs :: Board -> HashMap Pos Node
nodeTavs b =
  HashMap.mapWithKey Node $
  flip execState HashMap.empty $ do
    initNodes b tavObjId (b^.bo_taverns)

data State = State {
    _s_nodes :: HashMap Pos Node
  -- ^ List of nodes
  , _s_vor :: HashMap Pos (Node,Integer)
  -- ^ Tiles to node map
  , _s_frontier :: HashMap Node [Pos]
  -- ^ Frontier (internal)
  , _s_dist :: Integer
  -- ^ Radius (internal)
  , _s_conn :: HashMap Node (HashMap Node Integer)
  -- ^ Distance between directly connected nodes, pairwise
  , _s_square :: HashMap Node Integer
  -- ^ Node squares
  , _s_board :: Board
  } deriving(Show, Generic, NFData)

$(makeLenses ''State)

connectivity :: State -> [((Node,Node),Integer)]
connectivity s = nub $ concatMap (\(n,mp) -> map (\(n',d) -> ((n`min`n',n`max`n'),d)) $ HashMap.toList mp) $ HashMap.toList (s^.s_conn)

initState :: Board -> HashMap Pos Node -> State
initState b seed =
  let
    vor = HashMap.map (id &&& const 0) seed

    frnt = foldl' (\f n -> HashMap.insert n [n^.n_center] f) HashMap.empty seed

    conn = HashMap.empty

    sqs = HashMap.fromList $ (HashMap.elems seed)`zip`(repeat 1)
  in
  State seed vor frnt 0 conn sqs b

-- | Build Voronoy graph for interesting points of Board @b@
build :: Board -> HashMap Pos Node -> State
build b seed =
  flip execState (initState b seed) $ do
  loop $ do
    s <- get

    when (all (List.null) (s^.s_frontier)) $ do
      break()

    forM_ (s^.s_nodes) $ \n -> do
      let f = (s^.s_frontier) HashMap.! n
      f' <- do
        List.concat <$> do
        forM f $ \p -> do
          List.concat <$> do
          forM (toList (boardAdjascentAvail p b)) $ \p' -> do
            vor <- use s_vor
            case HashMap.lookup p' vor of
              Just (n'@Node{}, dist)
                |n /= n' -> do
                  let d = (dist+s^.s_dist)
                  s_conn %=
                    HashMap.insertWith (HashMap.unionWith min) n (HashMap.singleton n' d) .
                    HashMap.insertWith (HashMap.unionWith min) n' (HashMap.singleton n d)
                  return []
                |otherwise ->
                  return []
              Nothing -> do
                s_vor %= HashMap.insert p' (n, s^.s_dist+1)
                s_square %= HashMap.insertWith (+) n 1
                -- when (n^.n_center == Pos 3 22) $ do
                --   traceM ("added", p')
                return [p']
      s_frontier %= HashMap.insert n (f')
    s_dist %= (+1)

legend :: HashMap Pos (Node,Integer) -> HashMap Node Integer
legend vor = HashMap.fromList $ (nub $ map fst (HashMap.elems vor))`zip`[0..]

printPoint :: Board -> HashMap Pos (Node,Integer) -> Pos -> Text
printPoint b vor pos =
  case HashMap.lookup pos vor of
    Just (n,dist) ->
      case pos == n^.n_center of
        True ->
          tpack $ printf "%.2d" ((legend vor) HashMap.! n)
          -- tpack $ printf "X%1d" (length $ n^.n_goals)
        False -> tpack $ printf "  " -- "%.2d" ((legend vor) HashMap.! n)
    Nothing -> printTileStatic $ (b^.bo_tiles) HashMap.! pos

-- | Print Voronoy graph
print :: Board -> HashMap Pos (Node,Integer) -> Text
print b vor =
  foldl (<>) "" $
  flip map [0..(b^.bo_size)-1] $ \y ->
    foldl (<>) "\n" $
    flip map [0..(b^.bo_size)-1] $ \x ->
      -- "??"
      printPoint b vor (Pos x y)

type ClusterId = Integer

data Cluster = Cluster {
    _c_nodes :: HashSet Node
  , _c_square :: Integer
  , _c_id :: ClusterId
  , _c_letter :: Char
  } deriving(Show, Generic, Hashable, NFData)

$(makeLenses ''Cluster)

instance Eq Cluster where
  (==) c1 c2 = (c1^.c_id) == (c2^.c_id)

instance Ord Cluster where
  compare c1 c2 = (c1^.c_id)`compare`(c2^.c_id)


clmerge cl1@(Cluster n1 s1 i1 l1) cl2@(Cluster n2 s2 i2 l2)
  | i1 == i2 = Cluster (HashSet.union n1 n2) (s1+s2 {- Wrong, fixed below! -}) i1 l1
  | otherwise = error "clmerge: don't know how to merge clusters with different ids"

data Tavs
data Mines

data ClusterMap x = ClusterMap {
    _cm_vorst :: !(State)
  -- ^ Voronoy state for this cluster map
  , _cm_nodes :: !(HashMap Node ClusterId)
  -- ^ Node - to - cluster
  , _cm_map :: !(HashMap ClusterId Cluster)
  -- ^ Cluster dictionary
  , _cm_conn :: !(HashMap ClusterId (HashSet ClusterId))
  -- ^ Connectivity between clusters
  , _cm_nextcl :: !(Integer)
  } deriving(Show, Generic, NFData)

initialClusterMap :: State -> ClusterMap x
initialClusterMap s = ClusterMap s mempty mempty mempty 0

$(makeLenses ''ClusterMap)

foldClusters :: ClusterMap x -> (a -> Int -> Cluster -> a) -> a -> Pos -> a
foldClusters cm f a p = go a [(c0,0)] (HashSet.singleton c0) where
  c0 = clookup cm p
  go a [] _ = a
  go a ((c,depth):cs) visited =
    let
      new = HashSet.filter (\c -> not $ c`HashSet.member`visited) $ clneighbours cm (c^.c_id)
      order' = cs ++ ((HashSet.toList new)`zip`(repeat $ depth+1))
      visited' = visited`mappend`new
    in
    go (f a depth c) order' visited'

foldNodes :: ClusterMap x -> (a -> Int -> Node -> a) -> a -> Pos -> a
foldNodes cm f ini p = foldClusters cm f' ini p where
  f' acc depth c = foldl (\acc n -> f acc depth n) acc (c^.c_nodes)

foldGoals :: ClusterMap x -> (a -> Int -> (Node,Goal) -> a) -> a -> Pos -> a
foldGoals cm f ini p = foldNodes cm f' ini p where
  f' acc depth n = foldl (\acc g -> f acc depth (n,g)) acc (n^.n_goals)


-- clookup :: ClusterMap -> Pos -> Cluster
-- clookup cm p = (cm ^.cm_map) ! ((cm^.cm_nodes) !  ((cm ^. cm_vorst . s_nodes) ! p))

clookup :: ClusterMap x -> Pos -> Cluster
clookup cm p =
  let
    get :: (Eq a, Hashable a) => HashMap a b -> a -> Maybe b
    get = flip HashMap.lookup

    check :: (Show a) => (String,a) -> Maybe x -> x
    check (x,a) = fromMaybe (error $ x <> " on " <> show a)

    n = fst $ check ("pos -> nodes",p) ((cm ^. cm_vorst . s_vor) `get` p)
    cid = check ("node -> cid", n) ((cm^.cm_nodes) `get` n)
    c = check ("cid -> c",cid) ((cm ^.cm_map) `get` cid)
  in
  c

clneighbours :: ClusterMap x -> ClusterId -> HashSet Cluster
clneighbours cm clid =
  case HashMap.lookup clid (cm^.cm_conn) of
    Just cls' -> HashSet.map ((cm^.cm_map) !) cls'
    Nothing -> HashSet.empty

clusterize :: Integer -> State -> ClusterMap x
clusterize dnei s =
  let
    letters = MapL.fromList $ [0..]`zip`(filter (\x -> isPrint x && (not $ isSpace x)) ['a'..])

    clsingleton i n = Cluster (HashSet.singleton n) ((s^.s_square) HashMap.! n) i (letters MapL.! i)

    explode :: Integer -> [Node] -> S.State (ClusterMap x) ()
    explode clid (n:ns) = do
        cm_nodes %= HashMap.insert n clid
        cm_map %= HashMap.insertWith clmerge clid (clsingleton clid n)
        case HashMap.lookup n (s^.s_conn) of
          Nothing ->
            explode clid ns
          Just conmap -> do
            nei <- do
              concat <$> do
              forM (HashMap.toList conmap) $ \ (n',d) -> do
                cls <- use cm_nodes
                let mb_clid' = HashMap.lookup n' cls
                if d < dnei then
                  case mb_clid' of
                    Just clid' | clid == clid' -> return []
                               | otherwise -> error $ "Mismatching clusters for " ++ show n ++ " and " ++ show n'
                    Nothing -> return [n']
                else
                  case mb_clid' of
                    Just clid'
                      | clid /= clid' -> do
                        cm_conn %=
                          HashMap.insertWith (HashSet.union) clid (HashSet.singleton clid') .
                          HashMap.insertWith (HashSet.union) clid' (HashSet.singleton clid)
                        return []
                      | otherwise ->
                        return []
                    Nothing -> do
                      return []
            explode clid ((nub nei)++ns)
    explode clid [] = return ()

  in do
  flip execState (initialClusterMap s) $ do
    forM_ (s^.s_nodes) $ \n -> do
      ns <- use cm_nodes
      case HashMap.lookup n ns of
        Just clid -> return ()
        Nothing -> do
          nextid <- use cm_nextcl
          cm_nextcl %= (+1)
          explode nextid [n]
    cm_map %= HashMap.map (\c ->
      c {
        _c_square = sum $ map ((s^.s_square) !) $ HashSet.toList (c^.c_nodes)
      })

printClusters :: ClusterMap x -> Text
printClusters cm =
  let
    s = cm^.cm_vorst
    b = s^.s_board
    vor = s^.s_vor
    c = view cm_nodes cm
  in
  foldl (<>) "" $
  flip map [0..(b^.bo_size)-1] $ \y ->
    foldl (<>) "\n" $
    flip map [0..(b^.bo_size)-1] $ \x ->
      let
        pos = Pos x y
      in
      case HashMap.lookup pos vor of
        Just (n,d) ->
          tpack $ printf " %c" $ view c_letter $ (cm^.cm_map) ! (c ! n)
        Nothing ->
          printTileStatic $ (b^.bo_tiles) HashMap.! pos

