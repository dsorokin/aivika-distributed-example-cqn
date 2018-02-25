
{-# LANGUAGE RecursiveDo #-}

-- Closed Queueing Network (CQN)

module ParallelNode where

import Control.Monad
import Control.Monad.Trans
import qualified Control.Distributed.Process as DP
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Concurrent

import Data.Array

import Simulation.Aivika.Trans
import Simulation.Aivika.Distributed

import ModelConfig
import DistributedModel

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
runSlaveModel :: (ModelConfig,
                  DP.ProcessId,
                  Array Int DP.ProcessId,
                  Int)
                 -> DP.Process (DP.ProcessId, DP.Process Int)
runSlaveModel (config, timeServerId, pids, n) =
  runDIO m ps timeServerId
  where
    ps = defaultDIOParams { dioLoggingPriority = WARNING,
                            dioTimeHorizon = modelTimeHorizon config }
    m  = do registerDIO
            a <- runSimulation (slaveModel config pids n) (specs config)
            unregisterDIO
            return a

-- | The master node implementation.
master :: Backend -> ModelConfig -> [DP.NodeId] -> DP.Process ()
master = \backend config nodes ->
  mdo liftIO . putStrLn $ "Slaves: " ++ show nodes
      DP.say "Started simulating in the parallel mode..."
      let n = modelTandemQueueCount config
          timeServerParams = defaultTimeServerParams { tsLoggingPriority = WARNING }
      timeServerId <- DP.spawnLocal $ timeServer n timeServerParams
      (masterId, masterProcess) <- runMasterModel config timeServerId pids n
      slavePairs <- forM [1 .. (n - 1)] $ \i ->
        runSlaveModel (config, timeServerId, pids, i)
      let slaveIds = map fst slavePairs
          slaveProcesses = map snd slavePairs
          pids = array (1, n) $ (n, masterId) : zip [1 .. (n - 1)] slaveIds
      forM_ slaveProcesses $ \m ->
        DP.spawnLocal $
        do a <- m
           DP.say $
             "Slave's result: " ++ show a
      a <- masterProcess
      DP.say $
        "Master's result: " ++ show a
      liftIO $
        threadDelay 100000
