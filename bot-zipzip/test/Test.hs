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

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.PQueue.Prio.Min as MinPQueue
import qualified Data.PQueue.Prio.Max as MaxPQueue

import Imports
import Voronoy
import Astar
import Types
import Cli


testGame1 str = (g,hero1,cm,ct) where
  g = Game (GameId "foo") 4 16 (HashMap.fromList [(HeroId 1, hero1)]) board False
  board = parseBoard (ilength str) (concat str)
  pos = flip3 HashMap.foldrWithKey (board.>bo_tiles) (error "no test hero defined") $ \p t x -> if
    | isHeroTile t -> p
    | otherwise -> x
  hero1 =
    Hero (HeroId 1) "foo" (Just "bar") (Just 3000) pos 99 100 0 (Pos 0 0) False Nothing
  cm = clusterize 2 (build (g^.gameBoard) (nodeMines (g.>gameBoard)))
  ct = clusterize 2 (build (g^.gameBoard) (nodeTavs  (g.>gameBoard)))

-- | Run @defaultMain tests@ to run the test tree
tests :: TestTree
tests = testGroup "Astar Tests" [
     testCase "Simple clusterize and astar test" $
      let
        (g,h,cm,ct) = testGame1 [
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
        (g,h,cm,ct) = testGame1 [
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
        (g,h,cm,ct) = testGame1 [
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
  ]


main = defaultMain tests
