{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Control.Lens as Lens
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.List as List
import Imports

-- | Key aka password
newtype Key = Key Text
  deriving (Show, Eq)

instance ToJSON Key where
    toJSON (Key k) = Aeson.String k

-- | Note hid_int is 1-based
newtype HeroId = HeroId { hid_int :: Integer }
  deriving (Show, Read, Eq, Generic, Ord, NFData, Hashable)

-- | Distance from @a@ to @b@ module 4
carouselDist4 :: HeroId -> HeroId -> Integer
carouselDist4 a b =
  if a <= b then (hid_int b) - (hid_int a)
           else 4 + ((hid_int b) - (hid_int a))

nextHeroId :: HeroId -> HeroId
nextHeroId HeroId{..} = if hid_int < 4 then HeroId (hid_int+1) else HeroId 1

instance FromJSON HeroId where
    parseJSON x = HeroId <$> parseJSON x

data Pos = Pos { posX :: Integer , posY :: Integer }
  deriving (Show, Read, Eq, Ord, Generic, Hashable, NFData)

printPos :: Pos -> Text
printPos p = "(" <> tshow (posX p) <> "," <> tshow (posY p) <> ")"

sqdist :: Pos -> Pos -> Integer
sqdist a b = (posX a - posX b)^2 + (posY a - posY b)^2

recdist :: Pos -> Pos -> Integer
recdist (Pos x1 y1) (Pos x2 y2) = abs (x1 - x2) + abs (y1 - y2)

posNear p1 p2 = recdist p1 p2 == 1

meanPos :: (Foldable t) => t Pos -> Pos
meanPos ps =
  let
    Pos accx accy = foldl' (\acc p -> Pos (posX acc + posX p) (posY acc + posY p)) (Pos 0 0) ps
  in
  Pos (toInteger $ round $ (fromInteger accx) / (nlength ps)) (toInteger $ round $ fromInteger accy / nlength ps)

instance FromJSON Pos where
    {-parseJSON (Aeson.Object o) = Pos <$> o .: "x" <*> o .: "y"-}
    {-AA 20140204 These seem to be labelled around the wrong way in the JSON-}
    parseJSON (Aeson.Object o) = Pos <$> o .: "y" <*> o .: "x"
    parseJSON _ = mzero

data Tile = FreeTile | WoodTile | TavernTile | HeroTile HeroId | MineTile (Maybe HeroId)
    deriving (Show, Read, Eq, Generic, Hashable, NFData)

isMovableTile :: Tile -> Bool
isMovableTile = \case
  FreeTile -> True
  HeroTile _ -> True
  _ -> False

isHeroTile :: Tile -> Bool
isHeroTile (HeroTile _) = True
isHeroTile _ = False

data Dir = Stay | North | South | East | West
    deriving (Show, Read, Eq, Ord, Generic, Hashable, NFData, Enum, Bounded, Binary)

printDir :: Dir -> Text
printDir Stay = "o"
printDir North = "^"
printDir South = "v"
printDir East = ">"
printDir West = "<"

instance FromJSON Dir where
  parseJSON (Aeson.String "East") = pure East
  parseJSON (Aeson.String "West") = pure West
  parseJSON (Aeson.String "North") = pure North
  parseJSON (Aeson.String "South") = pure South
  parseJSON (Aeson.String "Stay") = pure Stay
  parseJSON x = error $ "ParseJSON Dir: error, no parse for " <> show x

instance ToJSON Dir where
  toJSON Stay = Aeson.String "Stay"
  toJSON North = Aeson.String "North"
  toJSON South = Aeson.String "South"
  toJSON East = Aeson.String "East"
  toJSON West = Aeson.String "West"

posDist :: Pos -> Pos -> Integer
posDist p1@(Pos x1 y1) p2@(Pos x2 y2) =
  (abs $ x2-x1) + (abs $ y2-y1)

-- | Difference between adjascent positions
-- TODO: remove error
posDiff :: Pos -> Pos -> Dir
posDiff p1@(Pos x1 y1) p2@(Pos x2 y2) =
  case (x2-x1, y2-y1) of
    (1,0) -> East
    (-1,0) -> West
    (0,1) -> South
    (0,-1) -> North
    (0,0) -> Stay
    _ -> trace ("warning: posDiff: non-adjascent positions: " <> show p1 <> " " <> show p2) $ Stay


data Hero = Hero {
    _heroId        :: HeroId
  , _heroName      :: Text
  , _heroUserId    :: Maybe Text
  , _heroElo       :: Maybe Integer
  , _heroPos       :: Pos
  , _heroLife      :: Integer
  , _heroGold      :: Integer
  , _heroMineCount :: Integer
  , _heroSpawnPos  :: Pos
  , _heroCrashed   :: Bool
  , _heroLastDir   :: Maybe Dir
} deriving (Show, Read, Eq, Generic, NFData)

-- | Returns hero elo
heroElo' def = maybe def id . _heroElo

$(makeLenses ''Hero)

sameid :: Hero -> Hero -> Bool
sameid h1 h2 = h1^.heroId == h2^.heroId

instance FromJSON Hero where
    parseJSON (Aeson.Object o) =
      Hero <$> o .: "id"
           <*> o .: "name"
           <*> o .:? "userId"
           <*> o .:? "elo"
           <*> o .: "pos"
           <*> o .: "life"
           <*> o .: "gold"
           <*> o .: "mineCount"
           <*> o .: "spawnPos"
           <*> o .: "crashed"
           <*> o .:? "lastDir"
    parseJSON _ = mzero

data Board = Board {
    _bo_size  :: !Integer
  , _bo_tiles :: !(HashMap Pos Tile)
  , _bo_mines :: !(HashSet Pos)
  , _bo_taverns :: !(HashSet Pos)
  , _bo_heroMines :: !(HashMap HeroId (HashSet Pos))
} deriving (Show, Read, Eq, Generic, NFData)

-- | Rough distance between heroes
herodist :: Hero -> Hero -> Integer
herodist h1 h2 = recdist (h1.>heroPos) (h2.>heroPos)

nullBoard sz = Board sz HashMap.empty HashSet.empty HashSet.empty HashMap.empty

$(makeLenses ''Board)

boardPositions :: Board -> [Pos]
boardPositions Board{..} = [ (Pos x y) | x <- [0.._bo_size-1], y <- [0.._bo_size-1]]

boardTiles :: Board -> [Tile]
boardTiles b = map (view bo_tiles b HashMap.!) (boardPositions b)

boardMovableTiles b = HashMap.size $ HashMap.filter isMovableTile $ b^.bo_tiles

boardAvailPositions b =
  HashMap.filterWithKey (\p t -> isMovableTile t) $ b^.bo_tiles

-- | Check pos for generic validity, i.e. inside size
boardValidPos :: Board -> Pos -> Bool
boardValidPos Board{..} Pos{..} = posX >= 0 && posX < _bo_size && posY >=0 && posY < _bo_size

-- FIXME: replace with boardAdjascent
boardAdjascentTiles :: (Tile -> Bool) -> Pos -> Board -> HashSet Pos
boardAdjascentTiles flt Pos{..} Board{..} =
  HashSet.unions $
  flip concatMap [
    (Pos (posX-1) posY),
    (Pos (posX+1) posY),
    (Pos posX (posY-1)),
    (Pos posX (posY+1))] $ \p ->
    fromMaybe [] $ do
      tile <- HashMap.lookup p _bo_tiles
      if flt tile then Just [HashSet.singleton p] else Nothing

-- | Return tiles which are potentially available for moving
boardAdjascentAvail :: Pos -> Board -> HashSet Pos
boardAdjascentAvail =
  boardAdjascentTiles $
    \case
      FreeTile -> True
      HeroTile _ -> True
      _ -> False

-- | Iterate over adjucent tiles
boardAdjascent :: (a -> (Tile, Pos) -> a) -> a -> Pos -> Board -> a
boardAdjascent f a Pos{..} b@Board{..} =
  List.foldl' (\acc p -> f acc (_bo_tiles HashMap.! p, p)) a $
  List.filter (boardValidPos b) $
  [ (Pos (posX-1) posY),
    (Pos (posX+1) posY),
    (Pos posX (posY-1)),
    (Pos posX (posY+1))  ]

-- | Return tiles occupied by heroes
boardAdjascentHeroes :: Pos -> Board -> [HeroId]
boardAdjascentHeroes =
  boardAdjascent (\hids (t,p) -> case t of {HeroTile hid -> hid:hids; _ -> hids }) []

-- | Move the position, check for board dimentions
boardMove :: Board -> Pos -> Dir -> Pos
boardMove b p Stay = p
boardMove b p@Pos{..} dir =
  let
    check p'@Pos{..} = if posX < 0 || posX > ((b^.bo_size)-1) ||
                          posY < 0 || posY > ((b^.bo_size)-1)
                       then p
                       else p'
  in
    check $
      case dir of
        North -> Pos posX (posY-1)
        South -> Pos posX (posY+1)
        West -> Pos (posX-1) posY
        East -> Pos (posX+1) posY
        Stay -> Pos posX posY

instance FromJSON Board where
    parseJSON (Aeson.Object o) = parseBoard <$> o .: "size" <*> o .: "tiles"
    parseJSON _ = mzero

-- | Print a tile with a canonical symbol
printTile FreeTile = "  "
printTile WoodTile = "##"
printTile (HeroTile hid@(HeroId i)) = "@" <> (tpack $ show i)
printTile TavernTile = "[]"
printTile (MineTile Nothing) = "$-"
printTile (MineTile (Just hid@(HeroId i))) = "$" <> (tpack $ show i)

-- | Print non-moving tile with a canonical symbol, otherwise print empty space
printTileStatic FreeTile = "  "
printTileStatic WoodTile = "##"
printTileStatic TavernTile = "[]"
printTileStatic (MineTile _) = "$-"
printTileStatic _ = "  "


printTiles :: Board -> Text
printTiles = foldl (<>) "" . map printTile . boardTiles

instance ToJSON Board where
    toJSON b  = Aeson.object [ "size"  .= view bo_size b
                             , "tiles" .= printTiles b
                             ]

parseBoard :: Integer -> String -> Board
parseBoard s t =
  let
    chunks []       = []
    chunks (_:[])   = error "chunks: even chars number"
    chunks (a:b:xs) = (a, b):chunks xs
    p :: (Lens.Field1 s t a b) => Lens s t a b
    p = _1
    b :: (Lens.Field2 s t a b) => Lens s t a b
    b = _2
  in
  view b $
  flip execState (Pos 0 0, nullBoard s) $ do
    forM_ (chunks t) $ \ab -> do
      pos@Pos{..} <- use p
      tile <- case ab of
            (' ', ' ') -> return $ FreeTile
            ('#', '#') -> return $ WoodTile
            ('@', x)   -> do
              let hid = HeroId $ read [x]
              b.bo_heroMines %= HashMap.insertWith (<>) hid (HashSet.empty)
              return $ HeroTile hid
            ('[', ']') -> do
              b.bo_taverns %= HashSet.insert pos
              return TavernTile
            ('$', x)   -> do
              b.bo_mines %= HashSet.insert pos
              mhid <-
                case x of
                  '-' -> pure Nothing
                  x -> do
                    let hid = HeroId $ read [x]
                    b.bo_heroMines %= HashMap.insertWith (<>) hid (HashSet.singleton pos)
                    pure (Just hid)
              return (MineTile mhid)
            (a, b) -> error $ "parse: unknown tile pattern " ++ (show $ a:b:[])
      b.bo_tiles %= HashMap.insert pos tile
      p %= const ( if | posX == s-1 -> Pos 0 (posY+1)
                      | otherwise -> Pos (posX+1) posY )

newtype GameId = GameId { gameid :: Text }
    deriving (Show, Read, Eq, Ord)

instance FromJSON GameId where
    parseJSON x = GameId <$> parseJSON x

data Game = Game {
    _gameId       :: GameId
  , _gameTurn     :: Integer
  , _gameMaxTurns :: Integer
  , _gameHeroes   :: HashMap HeroId Hero
  , _gameBoard    :: !Board
  , _gameFinished :: Bool
  } deriving (Show, Read, Eq)

$(makeLenses ''Game)

gameTiles :: Lens' Game (HashMap Pos Tile)
gameTiles = gameBoard.bo_tiles

gameTile :: Game -> Pos -> Tile
gameTile g pos = g.>gameTiles.(idx pos)

instance FromJSON Game where
    parseJSON (Aeson.Object o) = Game <$> o .: "id"
                                <*> o .: "turn"
                                <*> o .: "maxTurns"
                                <*> (HashMap.fromList <$> map (_heroId &&& id)  <$> o .: "heroes")
                                <*> o .: "board"
                                <*> o .: "finished"
    parseJSON _ = mzero

-- | Space around hero
--  ...
--  .@.
--  ...
--
heroKillZone :: Game -> Hero -> HashSet Pos
heroKillZone g h = HashSet.insert (h.>heroPos) $ boardAdjascentAvail (h.>heroPos) (g.>gameBoard)

heroMines :: HeroId -> Game -> Integer
heroMines h g = maybe 0 (view heroMineCount) (HashMap.lookup h (g^.gameHeroes))

getHero :: Game -> HeroId -> Hero
getHero g hid =
  case HashMap.lookup hid (g^.gameHeroes) of
    Just h -> h
    Nothing -> error $ "assert: getHero: no hero with id " ++ show hid ++ " among " ++ show (g^.gameHeroes)

gameGreatestMineCount :: Game -> Hero
gameGreatestMineCount g =
  head $ sortBy (compare`on` (0-) . _heroMineCount) $ HashMap.elems $ g^.gameHeroes

-- | Answers how does wealth of a hero differ from other reachest hero's wealth
overWealth :: Game -> Hero -> Integer
overWealth g h =
  let
    raiting =
      reverse $ sortBy (compare`on`_heroMineCount) $
      filter (\h' -> not (sameid h h')) $ HashMap.elems $ g^.gameHeroes
  in
  case raiting of
    (h':_) -> h^.heroMineCount - h'^.heroMineCount
    [] -> h^.heroMineCount

-- | Calculate list of enemies, ordered by (elo,income)
gameEnemies :: Game -> Hero -> [Hero]
gameEnemies g h = filter (not . (sameid h)) (HashMap.elems $ g^.gameHeroes)

-- | Nearest enemies (simple rectangle distance)
nearestEnemies :: Game -> Hero -> [(Integer,Hero)]
nearestEnemies g h = sortOn fst $ map (\h' -> (recdist (h.>heroPos) (h'.>heroPos), h')) $ gameEnemies g h

-- | Nearest Enemy
nearestEnemy :: Game -> Hero -> (Integer, Hero)
nearestEnemy g h = head $ nearestEnemies g h

-- | Returns a hero which kills given hero
gameFindKiller :: Game -> HeroId -> Game -> Maybe Hero
gameFindKiller g hid g' =
  let
    h = g.>gameHeroes.(idx hid)
    h' = g'.>gameHeroes.(idx hid)
    killed = (h'.>heroLife >= 99) && (h'.>heroMineCount == 0) && (h'.>heroPos == h'.>heroSpawnPos)
  in
  listToMaybe $
  sortBy (compare`on`(0-)._heroMineCount) $
  flip filter (gameEnemies g h) $ \e ->
    let
      e' = g'.>gameHeroes.(idx (e.>heroId))
      nearEnemy = (e.>heroPos) `posNear` (h.>heroPos)
      enemyIncome = e'.>heroMineCount - e.>heroMineCount
      heroIncome = h'.>heroMineCount - h.>heroMineCount
    in
    killed && nearEnemy && (enemyIncome >= (-heroIncome))

-- | Returns place of hero in the Gold rating among the current game, [0..3]
gameHeroPlace :: Game -> HeroId -> Integer
gameHeroPlace g hid =
  toInteger $
  fromMaybe  (error "gameHeroPlace: no such hero") $
  findIndex ((== hid) . _heroId) $
  sortBy (compare`on`(0-)._heroGold) $
  HashMap.elems (g.>gameHeroes)

gameAdjascentAvail :: Pos -> Game -> HashSet Pos
gameAdjascentAvail p g = boardAdjascentAvail p (g.>gameBoard)


gameAdjascent :: (a -> (Tile, Pos) -> a) -> a -> Pos -> Game -> a
gameAdjascent f a p g = boardAdjascent f a p (g.>gameBoard)


isNearEnemy :: Game -> Hero -> Bool
isNearEnemy g h = gameAdjascent f False (h.>heroPos) g where
  f True _ = True
  f False ((HeroTile _),_) = True
  f False _ = False

gameSetHero :: Game -> HeroId -> Pos -> Game
gameSetHero g hid p =
  let
    h = g.>gameHeroes.(idx hid)
  in
  Lens.set (gameHeroes.(idx hid).heroPos) p $
  Lens.set (gameBoard.bo_tiles.(idx (h.>heroPos))) FreeTile $
  Lens.set (gameBoard.bo_tiles.(idx p)) (HeroTile (h.>heroId)) $ g

data GameState = GameState {
    _stateGame    :: !Game
  , _stateHero    :: !Hero
  , _stateToken   :: Text
  , _stateViewUrl :: Text
  , _statePlayUrl :: Text
  , _stateJSON    :: Aeson.Value
  } deriving (Read, Show)

$(makeLenses ''GameState)

instance FromJSON GameState where
    parseJSON (Aeson.Object o) =
      GameState
        <$> o .: "game"
        <*> o .: "hero"
        <*> o .: "token"
        <*> o .: "viewUrl"
        <*> o .: "playUrl"
        <*> pure (Aeson.Object o)
    parseJSON _ = mzero


class Accessible s a | s -> a where
  lens :: Lens' s a

