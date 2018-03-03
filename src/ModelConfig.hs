
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module ModelConfig
       (ModelConfig(..),
        ModelSimulationMode(..),
        readModelConfig) where

import Data.Ini
import Data.Either
import Data.Text hiding (map)
import Data.Typeable
import Data.Binary

import GHC.Generics

-- | Model configuration parameters.
data ModelConfig =
  ModelConfig { modelSimulationMode :: ModelSimulationMode,
                -- ^ the simulation mode
                modelStartTime :: Double,
                -- ^ the start time
                modelStopTime :: Double,
                -- ^ the stop time
                modelTandemQueueCount :: Int,
                -- ^ the tandem queue count
                modelInitialJobCount :: Int,
                -- ^ the initial job count per queue tandem
                modelSingleServerCount :: Int,
                -- ^ the single server count
                modelServiceTime :: Double,
                -- ^ the service time
                modelQueueDelay :: Double,
                -- ^ the queue delay
                modelLookahead :: Double,
                -- ^ the lookahead parameter
                modelSameTandemProb :: Double,
                -- ^ a probability of that the same tandem will be selected
                modelTimeHorizon :: Maybe Double,
                -- ^ the time horizon
                modelFaultTolerant :: Bool,
                -- ^ the flag indicating whether the simulation is fault-tolerant
                modelLogicalProcessAddrs :: [String]
                -- ^ the addresses of logical processes
              } deriving (Eq, Ord, Show, Typeable, Generic)

data ModelSimulationMode = SequentialSimulationMode
                           -- ^ the model is true sequential
                         | PseudoSequentialSimulationMode
                           -- ^ the model is equivalent to a sequential one
                           -- but launched with help of the distributed module
                         | ParallelSimulationMode
                           -- ^ the logical processes are executed in paralell
                           -- within one computational node
                         | DistributedSimulationMode
                           -- ^ the logical processes are executed in different
                           -- computational nodes, possibly on different computers
                         deriving (Eq, Ord, Show, Typeable, Generic)

instance Binary ModelConfig
instance Binary ModelSimulationMode

-- | Like @forceEither@ from MissingH.
forceEither :: Either String a -> a
forceEither (Right a) = a
forceEither (Left e)  = error e

modelSection = "model"

startTimeKey = "start-time"

stopTimeKey = "stop-time"

tandemQueueCountKey = "tandem-queues"

singleServerCountKey = "single-servers"

serviceTimeKey = "service-time"

queueDelayKey = "queue-delay"

initialJobCountKey = "initial-jobs"

lookaheadKey = "lookahead"

sameTandemRatioKey = "same-tandem-ratio"
sameTandemRatioValueInfinity = "infinity"

timeHorizonKey = "time-horizon"
timeHorizonValueNone = "none"

faultTolerantKey = "fault-tolerant"
faultTolerantValueYes = "yes"
faultTolerantValueNo  = "no"

simulationModeKey = "simulation-mode"
simulationModeValueSeq = "sequential"
simulationModeValuePseudoSeq = "pseudo-sequential"
simulationModeValuePar = "parallel"
simulationModeValueDistributed = "distributed"

lpSection = "lp"

hostKey = "host"
portKey = "port"

-- | Read the model configuration parameters from the specified file.
readModelConfig :: String -> IO ModelConfig
readModelConfig filename =
  do ini <- fmap forceEither $ readIniFile filename
     let tandemQueueCount :: Int
         !tandemQueueCount = read $ unpack $ forceEither $ lookupValue modelSection tandemQueueCountKey ini
         startTime :: Double
         !startTime = read $ unpack $ forceEither $ lookupValue modelSection startTimeKey ini
         stopTime :: Double
         !stopTime = read $ unpack $ forceEither $ lookupValue modelSection stopTimeKey ini
         singleServerCount :: Int
         !singleServerCount = read $ unpack $ forceEither $ lookupValue modelSection singleServerCountKey ini
         serviceTime :: Double
         !serviceTime = read $ unpack $ forceEither $ lookupValue modelSection serviceTimeKey ini
         queueDelay :: Double
         !queueDelay = read $ unpack $ forceEither $ lookupValue modelSection queueDelayKey ini
         initialJobCount :: Int
         !initialJobCount = read $ unpack $ forceEither $ lookupValue modelSection initialJobCountKey ini
         lookahead :: Double
         !lookahead = read $ unpack $ forceEither $ lookupValue modelSection lookaheadKey ini
         !sameTandemProb = parseModelSameTandemProb tandemQueueCount ini
         !simulationMode = parseModelSimulationMode ini
         !timeHorizon
           | simulationMode == SequentialSimulationMode       = Nothing
           | simulationMode == PseudoSequentialSimulationMode = Nothing
           | otherwise = parseModelTimeHorizon ini
         !faultTolerant
           | simulationMode == DistributedSimulationMode = parseModelFaultTolerant ini
           | otherwise = False
         !logicalProcessAddrs
           | simulationMode == DistributedSimulationMode = parseModelLogicalProcessAddrs tandemQueueCount ini
           | otherwise = []
     return $! ModelConfig { modelSimulationMode = simulationMode,
                             modelStartTime = startTime,
                             modelStopTime = stopTime,
                             modelTandemQueueCount = tandemQueueCount,
                             modelInitialJobCount = initialJobCount,
                             modelSingleServerCount = singleServerCount,
                             modelServiceTime = serviceTime,
                             modelQueueDelay = queueDelay,
                             modelLookahead = lookahead,
                             modelSameTandemProb = sameTandemProb,
                             modelTimeHorizon = timeHorizon,
                             modelFaultTolerant = faultTolerant,
                             modelLogicalProcessAddrs = logicalProcessAddrs
                           }

-- | Parse the model mode.
parseModelSimulationMode :: Ini -> ModelSimulationMode
parseModelSimulationMode ini = parse (unpack $ forceEither $ lookupValue modelSection simulationModeKey ini)
  where parse x =
          case x of
            x | x == simulationModeValueSeq -> SequentialSimulationMode
            x | x == simulationModeValuePseudoSeq -> PseudoSequentialSimulationMode
            x | x == simulationModeValuePar -> ParallelSimulationMode
            x | x == simulationModeValueDistributed -> DistributedSimulationMode
            x -> error $ "Unsupported simulation mode: " ++ x

-- | Parse the model logical process addresses.
parseModelLogicalProcessAddrs :: Int -> Ini -> [String]
parseModelLogicalProcessAddrs count ini =
  flip map [1..count] $ \i ->
  let section = pack $ lpSection ++ show i
      host = unpack $ forceEither $ lookupValue section hostKey ini
      port :: Int
      port = read $ unpack $ forceEither $ lookupValue section portKey ini
  in host ++ ":" ++ show port

-- | Parse the probability of that the same tandem will be selected.
parseModelSameTandemProb :: Int -> Ini -> Double
parseModelSameTandemProb count ini =
  let value = unpack $ forceEither $ lookupValue modelSection sameTandemRatioKey ini
  in if value == sameTandemRatioValueInfinity
     then 1.0
     else let ratio :: Double
              ratio = read value
          in 1 / ((fromIntegral count - 1) / ratio + 1)

-- | Parse the time horizon value.
parseModelTimeHorizon :: Ini -> Maybe Double
parseModelTimeHorizon ini =
  let value = unpack $ forceEither $ lookupValue modelSection timeHorizonKey ini
  in if value == timeHorizonValueNone
     then Nothing
     else Just (read value)

-- | Parse the fault-tolerant flag.
parseModelFaultTolerant :: Ini -> Bool
parseModelFaultTolerant ini =
  let value = unpack $ forceEither $ lookupValue modelSection faultTolerantKey ini
  in case value of
    x | x == faultTolerantValueYes -> True
    x | x == faultTolerantValueNo  -> False
    x -> error $ "Unrecognized value of the fault-tolerant flag: " ++ x
