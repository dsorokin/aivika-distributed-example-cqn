
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecursiveDo #-}

-- Closed Queueing Network (CQN)

-- The case of either parallel or distributed simulation model

module DistributedModel where

import Control.Monad
import Control.Monad.Trans
import qualified Control.Distributed.Process as DP
import Control.Distributed.Process.Serializable
import Control.Concurrent

import Data.Typeable
import Data.Binary
import Data.Array

import GHC.Generics

import Simulation.Aivika.Trans
import qualified Simulation.Aivika.Trans.Queue.Infinite as IQ
import Simulation.Aivika.Distributed

import ModelConfig

-- | A small time gap
timeGap = 1.0e-6

-- | Represents a queue tandem.
type QueueTandem a = Array Int (IQ.FCFSQueue DIO a)

-- | The message that comes with switch.
data SwitchMessage a = SwitchMessage a
                     deriving (Eq, Ord, Show, Typeable, Generic)

instance Binary a => Binary (SwitchMessage a)

-- | The switch message item.
type SwitchMessageItem = Int

-- | Select the next tandem by the specified current tandem index.
selectTandem :: ModelConfig -> Int -> Parameter DIO Int
selectTandem config n
  | modelTandemQueueCount config <= 1 = return n
  | otherwise =
    do p1 <- randomUniform 0 1
       if p1 <= modelSameTandemProb config
         then return n
         else do n' <- randomUniformInt 1 (modelTandemQueueCount config - 1)
                 if n' < n
                   then return n'
                   else return (n' + 1)

-- | Generate a queue tandem.
generateTandem :: ModelConfig -> Simulation DIO (QueueTandem a)
generateTandem config =
  do qs <- forM [1 .. modelSingleServerCount config] $ \k ->
       do q <- runEventInStartTime IQ.newFCFSQueue
          return (k, q)
     return $ array (1, modelSingleServerCount config) qs

-- | Start processing the queue tandem.
startTandem :: ModelConfig
               -> QueueTandem a
               -> Int
               -> (a -> Event DIO ())
               -> Simulation DIO ()
startTandem config queues n switch = 
  forM_ [1 .. modelSingleServerCount config] $ \k ->
  runProcessInStartTime $
  let loop =
        do let q = queues ! k
           a <- IQ.dequeue q
           randomExponentialProcess $ modelServiceTime config
           liftEvent $
             if k == modelSingleServerCount config
             then switch a
             else let q' = queues ! (k + 1)
                  in IQ.enqueue q' a
           loop
  in loop

-- | Switch the transact to some random queue tandem.
switchTandem :: ModelConfig
                -> (Int -> a -> Event DIO ())
                -> QueueTandem a
                -> Int
                -> a
                -> Event DIO ()
switchTandem config send queues n a =
  do n' <- liftParameter $ selectTandem config n
     if n' == n
       then runProcess $
            do holdProcess $ modelLookahead config
               liftEvent $
                 addToTandem queues a
       else send n' a

-- | Add the item to the specified queue tandem.
addToTandem :: QueueTandem a
               -> a
               -> Event DIO ()
addToTandem queues a =
  let q = queues ! 1
  in IQ.enqueue q a

-- | Initialize the tandems.
initTandems :: ModelConfig -> (Int -> Int -> Event DIO ()) -> Event DIO ()
initTandems config send =
  forM_ [1 .. modelInitialJobCount config * modelTandemQueueCount config] $ \i ->
  do n <- liftParameter $ randomUniformInt 1 (modelTandemQueueCount config)
     send n i

-- | Subscribe the queue tandem to receiving new messages.
subscribeTandem :: Serializable a => QueueTandem a -> Simulation DIO ()
subscribeTandem queues =
  runEventInStartTime $
  handleSignal_ messageReceived $ \(SwitchMessage a) ->
  addToTandem queues a

-- | Return the tandem dequeue count.
tandemDequeueCount :: ModelConfig -> QueueTandem a -> Event DIO Int
tandemDequeueCount config queues =
  let q = queues ! modelSingleServerCount config
  in IQ.dequeueCount q

-- | Send a message to the logical process.
sendToLogicalProcess :: ModelConfig
                        -> Array Int DP.ProcessId
                        -> Int
                        -> SwitchMessageItem
                        -> Event DIO ()
sendToLogicalProcess config pids n a =
  do t <- liftDynamics time
     let t' = t + modelLookahead config
         pid = pids ! n
     enqueueMessage pid t' (SwitchMessage a)

-- | Initialize the logical processes.
initLogicalProcesses :: ModelConfig -> Array Int DP.ProcessId -> Event DIO ()
initLogicalProcesses config pids =
  initTandems config $ \n a ->
  do t <- liftDynamics time 
     let t' = t + timeGap
         pid = pids ! n
     enqueueMessage pid t' (SwitchMessage a)

-- | The logical process model.
logicalProcessModel :: ModelConfig
                       -> (Int -> SwitchMessageItem -> Event DIO ())
                       -> Int
                       -> Simulation DIO Int
logicalProcessModel config send n =
  do queues <- generateTandem config
     subscribeTandem queues
     let switch = switchTandem config send queues n
     startTandem config queues n switch
     runEventInStopTime $
       tandemDequeueCount config queues

-- | The master model.
masterModel :: ModelConfig
               -> Array Int DP.ProcessId
               -> Int
               -> Simulation DIO Int
masterModel config pids n =
  do runEventInStartTime $
       enqueueEventWithStartTime $
       initLogicalProcesses config pids
     let send = sendToLogicalProcess config pids
     logicalProcessModel config send n

-- | The slave model.
slaveModel :: ModelConfig
              -> Array Int DP.ProcessId
              -> Int
              -> Simulation DIO Int
slaveModel config pids n =
  do let send = sendToLogicalProcess config pids
     logicalProcessModel config send n

-- | Get the simulation specs.
specs :: ModelConfig -> Specs DIO
specs config =
  Specs { spcStartTime = modelStartTime config,
          spcStopTime = modelStopTime config,
          spcDT = 1.0,
          spcMethod = RungeKutta4,
          spcGeneratorType = SimpleGenerator }
