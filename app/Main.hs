
-- Closed Queueing Network (CQN)

-- Compiling and Running
---
-- Compile using
--
-- ghc -O2 -threaded Main.hs
--
-- Fire up some slave nodes (for the example, we run them on a single machine):
--
-- ./Main cqn.conf slave 2 +RTS -N &
-- ./Main cqn.conf slave 3 +RTS -N &
-- ./Main cqn.conf slave 4 +RTS -N &
--
-- And start the master node:
--
-- ./Main cqn.conf master 1 +RTS -N 

import qualified Control.Distributed.Process as DP
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet

import System.Environment (getArgs)
import System.Exit

import Simulation.Aivika

import SimpleLocalnetHelper
import ModelConfig
import qualified SeqModel as SeqM
import qualified PseudoSeqNode as PseudoSeqN
import qualified ParallelNode as ParN
import qualified DistributedNode as DistributedN

main :: IO ()
main =
  do args <- getArgs
     case args of
       filename : args' ->
         do config <- readModelConfig filename
            case modelSimulationMode config of
              SequentialSimulationMode | null args' ->
                do printSimulationResultsInStopTime
                     printResultSourceInEnglish
                     (SeqM.localModel config) (SeqM.specs config)
              SequentialSimulationMode ->
                do putStrLn "Only the configuration file is expected here"
                   exitFailure
              PseudoSequentialSimulationMode | null args' ->
                do backend <- initializeBackend "localhost" "8080" rtable
                   startMaster backend (PseudoSeqN.master backend config)
              PseudoSequentialSimulationMode ->
                do putStrLn "Only the configuration file is expected here"
                   exitFailure
              ParallelSimulationMode | null args' ->
                do backend <- initializeBackend "localhost" "8080" rtable
                   startMaster backend (ParN.master backend config)
              ParallelSimulationMode ->
                do putStrLn "Only the configuration file is expected here"
                   exitFailure
              DistributedSimulationMode ->
                do let addrs = modelLogicalProcessAddrs config
                   case args' of
                     ["master", index] ->
                       do (backend, nodes) <- getMasterBackend (read index - 1) addrs rtable
                          startMasterProcess backend nodes
                            (\backend nodes -> DistributedN.master backend config nodes)
                     ["slave", index] ->
                       do backend <- getSlaveBackend (read index - 1) addrs rtable
                          startSlave backend
                     _ ->
                       do putStrLn "Expected arguments: config master/slave index"
                          exitFailure
       _ -> 
         do putStrLn "Expected the configuration file name as the first argument"
            exitFailure
       where
         rtable :: DP.RemoteTable
         rtable = DistributedN.__remoteTable initRemoteTable
         -- rtable = initRemoteTable
