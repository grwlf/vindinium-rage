{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Cli where

import System.IO

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.HashMap.Strict as HashMap

import Util
import Imports
import Types

blankLine :: (MonadIO m) => m ()
blankLine = out []

out :: (MonadIO m) => [Text] -> m ()
out args = liftIO $ do
  Text.putStrLn (Text.unwords args)

err :: (MonadIO m) => [Text] -> m ()
err args = liftIO $ do
  Text.hPutStrLn stderr (Text.unwords args)

drawBoard :: Board -> [BImage] -> Text
drawBoard Board{..} maps =
  foldl (<>)
    (foldl (<>) "  " $
    flip map [0.._bo_size -1] $ \x ->
      (Text.pack $ printf "%02d" x)) $
  flip map [0.._bo_size -1] $ \y ->
    foldl (<>) "\n" $
    ((Text.pack $ printf "%02d" y):) $
    flip map [0.._bo_size-1] $ \x ->
      let
        p = Pos x y
        t = (_bo_tiles HashMap.! p)
        def = printTileC t
      in
      case t of
        HeroTile _ -> def
        _ -> fromMaybe def $ msum (map (HashMap.lookup p) maps)

printBoard b = drawBoard b []


clearTerminal :: (MonadIO m) => m ()
clearTerminal = liftIO $ do
  putStrLn "\027[2J"
  putStrLn "\027[1;1H"

clrDef = "\027[39m"
clrGreen = "\027[32m"
clrLGreen = "\027[92m"
clrCyan = "\027[36m"
clrBlue = "\027[34m"
clrRed =  "\027[31m"
clrYellow = "\027[33m"

heroColors = HashMap.fromList [
  (HeroId 1,clrRed),
  (HeroId 2,clrBlue),
  (HeroId 3,clrGreen),
  (HeroId 4,clrYellow)]

heroColor hid = heroColors HashMap.! hid

printHero :: HeroId -> Text
printHero hid = (heroColors HashMap.! hid) <> "@" <> (tshow (hid_int hid)) <> clrDef

printTileC (HeroTile hid) = printHero hid
printTileC TavernTile = clrCyan <> "[]" <> clrDef
printTileC (MineTile (Just hid@(HeroId i))) = (heroColors HashMap.! hid) <> "$" <> (tpack $ show i) <> clrDef
printTileC x = printTile x

printTilesC :: Board -> Text
printTilesC = foldl (<>) "" . map printTileC . boardTiles


printHeroStats :: Game -> Text
printHeroStats g@Game{..} =
  let
    hs = sortBy (compare`on`_heroId) $ HashMap.elems $ g^.gameHeroes
  in
  execWriter $ do
    tell $ "HeroId "
    forM_ hs $ \h -> do
      tell $ (heroColor (h.>heroId)) <> (tpack $ printf "%20s" (Text.take 10 $ h^.heroName)) <> clrDef
    tell "\n\n"
    tell $ "Life "
    forM_ hs $ \h -> do
      tell $ tpack $ printf "%20s" (show $ h.>heroLife)
    tell "\n"
    tell $ "Gold "
    forM_ hs $ \h -> do
      tell $ tpack $ printf "%20s" (show $ h.>heroGold)
    tell "\n"
    tell $ "Mines "
    forM_ hs $ \h -> do
      tell $ tpack $ printf "%20s" (show $ h.>heroMineCount)
    tell "\n"

printGame g = drawBoard (g.>gameBoard) []
drawGame g xs = drawBoard (g.>gameBoard) xs


-- | Let the user iterate through game records. Optional list of games @mgs@.
-- Default location will be used if Nothing.
-- Executes @exec@ when user press Enter
drawGameFinder :: FilePath -> Maybe [FilePath] -> (ServerState -> IO ()) -> IO ()
drawGameFinder data_dir mgs execfunc = do
  hSetBuffering stdin NoBuffering
  let d = data_dir
  gs <-
    case mgs of
      Just x -> return x
      Nothing -> do
        map (</> "%04d.json") <$> map (d </>) <$> filter ("game"`isPrefixOf`) <$> getDirectoryContents d
  let imax = length gs
  flip evalStateT (0::Int, 0::Int) $
    let
      fn i j = printf (gs !! i) j

      display :: (MonadIO m) => (Int,Int) -> m ()
      display (i,j) = do
        ss <- loadState (fn i j)
        clearTerminal
        out [ printGame (ss.>stateGame) ]
        blankLine
        out [ tpack (fn i j), tshow i, "of", tshow imax ]
        out [ "Use j/k to iterate through the games" ]
        out [ "Use h/l to iterate through the game moves" ]
        out [ "Use o to execute the decision maker" ]

    in do
    display (0,0)
    forever $ do
      (i,j) <- get
      c <- liftIO getChar
      (i',j') <- do
        case c of
          'j' -> return ((i+1)`min`(imax-1), 0)
          'k' -> return ((i-1)`max`0, 0)
          'h' -> return (i, j-1)
          'l' -> return (i, j+1)
          _ -> return (i,j)

      case c of
        'o' -> liftIO $ do
          ss <- loadState (fn i j)
          execfunc ss

        _ -> do
          put =<< do
            liftIO $ handle
              (\(e :: SomeException) ->
              do
                Text.putStrLn $ "Got an exception: " <> tshow e
                return (i,j)
              )

              (do
                display (i',j')
                return (i',j')
              )


