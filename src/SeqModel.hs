
-- Closed Queueing Network (CQN)

-- A case of the true sequential model.

module SeqModel where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika
import qualified Simulation.Aivika.Queue.Infinite as IQ

import ModelConfig

-- | Represents a queue tandem.
type QueueTandem a = Array Int (IQ.FCFSQueue a)

-- | Represents an array of queue tandems in case of sequential simulation.
type QueueTandemArray a = Array Int (QueueTandem a)

-- | Select the next tandem by the specified current tandem index.
selectTandem :: ModelConfig -> Int -> Parameter Int
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
generateTandem :: ModelConfig -> Simulation (QueueTandem a)
generateTandem config =
  do qs <- forM [1 .. modelSingleServerCount config] $ \k ->
       do q <- runEventInStartTime IQ.newFCFSQueue
          return (k, q)
     return $ array (1, modelSingleServerCount config) qs

-- | Start processing the queue tandem.
startTandem :: ModelConfig 
               -> QueueTandem a
               -> Int
               -> (a -> Event ())
               -> Simulation ()
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
                -> (Int -> a -> Event ())
                -> QueueTandem a
                -> Int
                -> a
                -> Event ()
switchTandem config send queues n a =
  do n' <- liftParameter $ selectTandem config n
     if n' == n
       then runProcess $
            do holdProcess $ modelLookahead config
               let q = queues ! 1
               liftEvent $
                 IQ.enqueue q a
       else send n' a

-- | Initialize the tandems.
initTandems :: ModelConfig -> (Int -> Int -> Event ()) -> Simulation ()
initTandems config send =
  runEventInStartTime $
  forM_ [1 .. modelInitialJobCount config * modelTandemQueueCount config] $ \i ->
  do n <- liftParameter $ randomUniformInt 1 (modelTandemQueueCount config)
     send n i

-- | Generate local tandems for sequential simulation.
generateLocalTandems :: ModelConfig -> Simulation (QueueTandemArray a)
generateLocalTandems config =
  do queues <- forM [1 .. modelTandemQueueCount config] $ \n ->
       do queues <- generateTandem config
          return (n, queues)
     return $ array (1, modelTandemQueueCount config) queues

-- | Start local tandems in case of sequential simulation.
startLocalTandems :: ModelConfig -> QueueTandemArray a -> Simulation ()
startLocalTandems config tandems =
  forM_ (assocs tandems) $ \(n, queues) ->
  do let send = sendToLocalTandem config tandems
         switch = switchTandem config send queues n
     startTandem config queues n switch

-- | Return dequeue counts in case of sequential simulation.
localTandemDequeueCounts :: ModelConfig -> QueueTandemArray a -> Event [Int]
localTandemDequeueCounts config tandems =
  forM (elems tandems) $ \queues ->
  let q = queues ! modelSingleServerCount config
  in IQ.dequeueCount q

-- | Send the transact to the specified queue tandem
-- in case of sequential simulation.
sendToLocalTandem :: ModelConfig -> QueueTandemArray a -> Int -> a -> Event ()
sendToLocalTandem config tandems n a =
  runProcess $
  do holdProcess $ modelLookahead config
     let queues = tandems ! n
         q = queues ! 1
     liftEvent $
       IQ.enqueue q a

-- | Initial the local queue tandems in case of sequential simulation.
initLocalTandems :: ModelConfig -> QueueTandemArray Int -> Simulation ()
initLocalTandems config tandems =
  initTandems config $ \n a ->
  do let queues = tandems ! n
         q = queues ! 1
     liftEvent $
       IQ.enqueue q a

-- | The sequential simulation model. 
localModel :: ModelConfig -> Simulation Results
localModel config =
  do tandems <- generateLocalTandems config
     startLocalTandems config tandems
     initLocalTandems config tandems
     return $
       results
       [resultSource
        "dequeueCounts"
        "Dequeue Counts"
        (localTandemDequeueCounts config tandems)]

-- | Get the simulation specs.
specs :: ModelConfig -> Specs
specs config =
  Specs { spcStartTime = modelStartTime config,
          spcStopTime = modelStopTime config,
          spcDT = 1.0,
          spcMethod = RungeKutta4,
          spcGeneratorType = SimpleGenerator }
