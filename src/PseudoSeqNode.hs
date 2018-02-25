
-- Closed Queueing Network (CQN)

module PseudoSeqNode where

import Control.Monad
import Control.Monad.Trans
import qualified Control.Distributed.Process as DP
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet

import Simulation.Aivika.Trans
import Simulation.Aivika.Distributed

import ModelConfig
import PseudoSeqModel

runModel :: ModelConfig -> DP.ProcessId -> DP.Process ()
runModel config timeServerId =
  do let ps = defaultDIOParams { dioLoggingPriority = WARNING,
                                 dioTimeHorizon = modelTimeHorizon config }
         m =
           do registerDIO
              a <- runSimulation (localModel config) (specs config)
              terminateDIO
              return a
     (modelId, modelProcess) <- runDIO m ps timeServerId
     a <- modelProcess
     DP.say $ "The result is " ++ show a

master :: Backend -> ModelConfig -> [DP.NodeId] -> DP.Process ()
master = \backend config nodes ->
  do liftIO . putStrLn $ "Slaves: " ++ show nodes
     DP.say "Started simulating in the pseudo-sequential mode..."
     let timeServerParams = defaultTimeServerParams { tsLoggingPriority = WARNING }
     timeServerId  <- DP.spawnLocal $ timeServer 1 timeServerParams
     runModel config timeServerId
