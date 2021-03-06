
{-# LANGUAGE OverloadedStrings #-}

module SimpleLocalnetHelper
       (getSlaveBackend,
        getMasterBackend,
        startMasterProcess) where

import qualified Control.Distributed.Process as DP
import qualified Control.Distributed.Process.Node as Node
import Control.Distributed.Process.Backend.SimpleLocalnet
import qualified Data.ByteString.Char8 as BS
import Network.Transport (EndPointAddress(..))

-- | Make a NodeId from "host:port" string.
makeNodeId :: String -> DP.NodeId
makeNodeId addr = DP.NodeId . EndPointAddress . BS.concat $ [BS.pack addr, ":0"]

splitAddr :: String -> (String, String)
splitAddr addr = (host, port)
  where
    (host, rest) = break (== ':') addr
    port = tail rest

deleteAt :: Int -> [String] -> [String]
deleteAt i xs = xs1 ++ xs2
  where (xs1, rest) = splitAt i xs
        xs2 = tail rest

getSlaveBackend :: Int -> [String] -> DP.RemoteTable -> IO Backend
getSlaveBackend i addrs rtable =
  do let addr = addrs !! i
         (host, port) = splitAddr addr
     backend <- initializeBackend host port rtable
     return backend

getMasterBackend :: Int -> [String] -> DP.RemoteTable -> IO (Backend, [DP.NodeId])
getMasterBackend i addrs rtable =
  do let addr = addrs !! i
         (host, port) = splitAddr addr
         slaves = map makeNodeId $ deleteAt i addrs 
     backend <- initializeBackend host port rtable
     return (backend, slaves)

startMasterProcess :: Backend -> [DP.NodeId] -> (Backend -> [DP.NodeId] -> DP.Process ()) -> IO ()
startMasterProcess backend slaves process =
  do node <- newLocalNode backend
     Node.runProcess node (process backend slaves)
