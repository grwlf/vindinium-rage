module RPC where

import qualified TF
import qualified TF_Client as Client

import Control.Exception
import Data.Maybe
import Data.Text.Lazy
import Text.Printf
import Network

import Thrift
import Thrift.Protocol.Binary
import Thrift.Transport
import Thrift.Transport.Handle
import Thrift.Server

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import TF


-- data Emebedding = Embedding {
--   embed :: HashMap Int (HashMap Pos Double)
-- }


-- embed :: Board -> 



testRPC :: IO ()
testRPC = do
  transport  <- hOpen ("localhost", PortNumber 9090)
  let binProto = BinaryProtocol transport
  let client = (binProto, binProto)

  Client.step client (HashMap.empty)
  printf "Done\n"

