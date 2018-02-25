
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecursiveDo #-}

-- Closed Queueing Network (CQN)

module DistributedNode where

import Control.Monad
import Control.Monad.Trans
import qualified Control.Distributed.Process as DP
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Concurrent

import Data.Typeable
import Data.Binary
import Data.Array

import GHC.Generics

import Simulation.Aivika.Trans
import Simulation.Aivika.Distributed

import ModelConfig
import DistributedModel

-- | The start message.
data StartMessage =
  StartMessage { startMessageProcessIds   :: Array Int DP.ProcessId,
                 startMessageIndex        :: Int
               } deriving (Typeable, Generic)
                          
instance Binary StartMessage

-- | Run the master model.
runMasterModel :: ModelConfig
                  -> DP.ProcessId
                  -> Array Int DP.ProcessId
                  -> Int
                  -> DP.Process (DP.ProcessId, DP.Process Int)
runMasterModel config timeServerId pids n =
  runDIO m ps timeServerId
  where
    ps = defaultDIOParams { dioLoggingPriority = WARNING,
                            dioTimeHorizon = modelTimeHorizon config }
    m  = do registerDIO
            a <- runSimulation (masterModel config pids n) (specs config)
            terminateDIO
            return a

-- | Run the slave model.
runSlaveModel :: (ModelConfig, DP.ProcessId) -> DP.Process ()
runSlaveModel (config, timeServerId) =
  do StartMessage pids n <- DP.expect
     let ps = defaultDIOParams { dioLoggingPriority = WARNING,
                                 dioTimeHorizon = modelTimeHorizon config }
         m  = do registerDIO
                 a <- runSimulation (slaveModel config pids n) (specs config)
                 unregisterDIO
                 return a
     (inboxId, logicalProcess) <- runDIO m ps timeServerId
     DP.spawnLocal $
       do a <- logicalProcess
          DP.say $
            "Slaves's result: " ++ show a
     DP.relay inboxId

remotable ['runSlaveModel]

-- | The master node implementation.
master :: Backend -> ModelConfig -> [DP.NodeId] -> DP.Process ()
master = \backend config nodes ->
  mdo liftIO . putStrLn $ "Slaves: " ++ show nodes
      DP.say "Started simulating in the distributed mode..."
      let n = modelTandemQueueCount config
      unless (n == 1 + length nodes) $
        error $ "Expected " ++ show (n - 1) ++ " slave nodes"
      let timeServerParams = defaultTimeServerParams { tsLoggingPriority = WARNING }
      timeServerId <- DP.spawnLocal $ timeServer n timeServerParams
      (masterId, masterProcess) <- runMasterModel config timeServerId pids n
      slaveIds <- forM nodes $ \node ->
        DP.spawn node ($(mkClosure 'runSlaveModel) (config, timeServerId))
      let pids = array (1, n) $ (n, masterId) : zip [1 .. (n - 1)] slaveIds
      forM_ (zip slaveIds [1..]) $ \(slaveId, i) ->
        DP.send slaveId (StartMessage pids i)
      a <- masterProcess
      DP.say $
        "Master's result: " ++ show a
      liftIO $
        threadDelay 100000
