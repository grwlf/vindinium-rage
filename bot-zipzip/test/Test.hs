{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Test where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck (testProperty, Property)
import Test.Tasty.HUnit (testCase, (@?=), assertEqual, assertBool)
import Test.QuickCheck (forAll, sublistOf, classify, whenFail, counterexample, elements, vectorOf, Gen, Testable, frequency, sized)
import System.FilePath ((</>))
import System.Process (system)
import System.Directory (createDirectoryIfMissing)
import System.IO (stderr,hPutStrLn)
import Data.Map (Map(..),(!))
import qualified Data.Map as Map

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.PQueue.Prio.Min as MinPQueue
import qualified Data.PQueue.Prio.Max as MaxPQueue

import Imports hiding (Map,(!))
import Voronoy
import Astar
import Types
import Cli
import Brain
import Sim


testGameP n str patches = (g,map snd heroes,cm,ct) where
  g = Game (GameId "foo") 4 16 (HashMap.fromList heroes) board False
  board = parseBoard (ilength str) (concat str)
  pos = flip3 HashMap.foldrWithKey (board.>bo_tiles) mempty $ \p t x ->
    case t of
      HeroTile hid -> Map.insert hid p x
      _ -> x
  hm ~hid = toInteger $ HashSet.size $ (board.>bo_heroMines) HashMap.! hid
  heroes = take n [
      let hid=HeroId 1 in (hid, patches!! 0 $ Hero hid "foo1" (Just "bar1") (Just 3000) (pos!hid) 100 100 (hm hid) (Pos 0 0) False Nothing)
    , let hid=HeroId 2 in (hid, patches!! 1 $ Hero hid "foo2" (Just "bar2") (Just 3000) (pos!hid) 100 100 (hm hid) (Pos 0 1) False Nothing)
    , let hid=HeroId 3 in (hid, patches!! 2 $ Hero hid "foo3" (Just "bar3") (Just 3000) (pos!hid) 100 100 (hm hid) (Pos 0 2) False Nothing)
    , let hid=HeroId 4 in (hid, patches!! 3 $ Hero hid "foo4" (Just "bar4") (Just 3000) (pos!hid) 100 100 (hm hid) (Pos 0 3) False Nothing)
    ]
  cm = clusterize 2 (build (g^.gameBoard) (nodeMines (g.>gameBoard)))
  ct = clusterize 2 (build (g^.gameBoard) (nodeTavs  (g.>gameBoard)))

testGame n str = testGameP n str [id,id,id,id]

lives ls = flip3 foldr ls mempty $ \l acc ->
             (execState (heroLife %= (const l))) : acc

-- | Run @defaultMain tests@ to run the test tree
tests :: TestTree
tests = testGroup "Astar Tests" [
     testCase "Simple clusterize and astar test" $
      let
        (g,h:_,cm,ct) = testGame 1 [
           {- 0 1 2 3 4 5 6 -}
            "##############"
           ,"##        []##"
           ,"##  ##########"
           ,"##          ##"
           ,"########    ##"
           ,"##@1        ##"
           ,"##############"]

        ps = MinPQueue.elems $ nearestTaverns g h ct emptyKillZone
        dbg = tunpack $ clrDef <> drawGame g [ drawPath (ps !! 0) ]
      in do
      assertEqual dbg 0 (HashMap.size $ cm.>cm_nodes)
      assertEqual dbg 0 (HashMap.size $ cm.>cm_map)
      assertEqual dbg 0 (HashMap.size $ cm.>cm_conn)

      assertEqual dbg 1 (HashMap.size $ ct.>cm_map)
      assertEqual dbg 0 (HashMap.size $ ct.>cm_conn)
      assertEqual dbg 1 (HashMap.size $ ct.>cm_nodes)

      assertEqual dbg (length ps) 1

    ,testCase "Astar should find both paths to the tavern" $
      let
        (g,h:_,cm,ct) = testGame 1 [
           {- 0 1 2 3 4 5 6 -}
            "##############"
           ,"##          ##"
           ,"##  ######[]##"
           ,"##          ##"
           ,"########    ##"
           ,"##@1        ##"
           ,"##############"]

        ps = MinPQueue.elems $ nearestTaverns g h ct emptyKillZone
        dbg = tunpack $ clrDef <> drawGame g ( flip map ps $ drawPath )
      in do
      assertBool dbg ((pathLength (ps!!0)) <= (pathLength (ps!!1)))
      assertEqual dbg 2 (length ps)

    ,testCase "Astar should find paths to the adjucent tavern" $
      let
        (g,h:_,cm,ct) = testGame 1 [
           {- 0 1 2 3 4 5 6 -}
            "######"
           ,"##@1[]"
           ,"######"]

        ps = MinPQueue.elems $ nearestTaverns g h ct emptyKillZone
        path = ps !! 0
        dbg = tunpack $ clrDef <> drawGame g ( flip map ps $ drawPath )
      in do
      assertEqual dbg 1 (length ps)
      assertEqual dbg 1 (pathLength path)

    ,testCase "Reward shoud reflect the number of mines" $
      let
        (g,h1:h2:_,cm,ct) = testGame 2 [
           {- 0 1 2 3 4 5 6 -}
            "@1$1$1"
           ,"@2  $2"
           ,"      "]

        r1 = gameReward g h1
        r2 = gameReward g h2
        dbg = "r1 " <> show r1 <> " r2 " <> show r2
      in do
      assertEqual dbg 2 (h1.>heroMineCount)
      assertEqual dbg 1 (h2.>heroMineCount)
      assertBool dbg (gameEnemies g h1 == [h2])
      assertBool dbg (gameEnemies g h2 == [h1])
      assertBool dbg (r1 > r2)

    ,testCase "Simulator should handle mine capturing" $
      let
        (g,h1:h2:_,_,_) =
          testGame 2 [
             {- 0 1 2 3 4 5 6 -}
              "@1  $1"
             ,"      "
             ,"  @2$1"]

        (g',h1':h2':_,_,_) =
          testGameP 2 [
             {- 0 1 2 3 4 5 6 -}
              "@1  $1"
             ,"      "
             ,"  @2$2"]
             (lives [100, 80])

        g'' = simGrabMine g h2 (Pos 2 2)

        dbg = tunpack $ clrDef <> drawGame g'' [] <> "\n" <> tshow g' <> "\n" <> tshow g''
      in do
      assertEqual dbg 1 (h1'.>heroMineCount)
      assertEqual dbg 1 (h2'.>heroMineCount)
      assertBool dbg (g' == g'')

    ,testCase "Simulator should handle hero kills" $
      let
        (g,h1:h2:_,_,_) =
          testGame 2 [
             {- 0 1 2 3 4 5 6 -}
              "    $2"
             ,"  @1  "
             ,"  @2$2"]

        (g',h1':h2':_,_,_) =
          testGame  2 [
             {- 0 1 2 3 4 5 6 -}
              "    $1"
             ,"@2@1  "
             ,"    $1"]

        g'' = simKillHero g (Just h1) h2

        dbg = tunpack $ clrDef <> drawGame g'' [] <> (tpshow $ g''.>gameHeroes.(idx (HeroId 2)))
      in do
      assertEqual dbg 2 (h1'.>heroMineCount)
      assertEqual dbg 0 (h2'.>heroMineCount)
      assertBool dbg (g' == g'')
  ]


main = defaultMain tests
