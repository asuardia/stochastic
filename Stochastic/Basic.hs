{-# OPTIONS_GHC -XMultiParamTypeClasses #-}
{-# OPTIONS_GHC -XRankNTypes #-}

module Stochastic.Basic   
    ( 
     Diffusion
    ) where
    
-------------------------------- TYPES -----------------------------------
type R = Double
type R_N = [Double]
type R_N_M = [[Double]]
type F_t = Double -> Double
type Time = Double
type BrownTimes = [Time]
type DiffTimes = [Time]
type Simulation = [(Time, R_N_M)]
type NumSim = Int
type Simulator = (Diffusion df) => DiscretMethod -> BrownianGenerator 
                                -> DiffTimes -> df -> Simulation
type TransformDiff = F_sim -> Simulation -> Simulation
data F_sim = PathDependent {trDifdPD :: R_N -> R_N -> R_N} 
           | European      {trDifdE :: R_N -> R_N}
type ShocksGenerator = NumSim -> BrownTimes -> Simulation
-------------------------------- CLASSES ---------------------------------
-- Diffusion class
class Diffusion d where
    driftMD     :: d -> [R_N  -> R]
    diffCoefsMD :: d -> [[R_N -> R]]	
	
-- ParametricDiffusion class
class ParametricDiffusion pd where
    toDiffusion :: (Diffusion d) => pd -> d

--------------------------------- DATA -----------------------------------


data DiscretMethod = Euler

data BrownianGenerator = BrownianGenerator {
                                               bTimes    :: BrownTimes,
                                               numSim    :: NumSim,
                                               shocksGen :: ShocksGenerator
                                           }

data DiffSimulation df = DiffSimulation { 
                                            simMethod :: DiscretMethod,
                                            brownGen  :: BrownianGenerator,
                                            diffusion :: (Diffusion df) => df,
                                            mcTimes   :: DiffTimes,
                                            simulDiff :: Simulator
                                        }

data MCEngine d = MCEngine {
                               diffSimulation :: (Diffusion d) => DiffSimulation d,
                               transfDiff     :: F_sim
                           }

------------------------------ FUNCTIONS ---------------------------------

launchMC :: (Diffusion df) => BrownTimes -> NumSim -> DiscretMethod -> df
                           -> DiffTimes -> ShocksGenerator -> F_sim 
                           -> Simulation
launchMC brT nSim dMet df dfT genSh trnDff = runEngine mcEng
    where 
          ---------------------------------
          runEngine :: (Diffusion df) => MCEngine df -> Simulation
          runEngine mcE = liftTransDiff (transfDiff mcE) sim 
              where diffSim = (diffSimulation mcE)
                    sim     = (simulDiff diffSim) (simMethod diffSim) 
                                                  (brownGen diffSim)  
                                                  (mcTimes diffSim)   
                                                  (diffusion diffSim)
          ---------------------------------
          simulateDf :: Simulator
          simulateDf dM brGen dfT df = [(0.0, [[0.0]])]
          ---------------------------------
          liftTransDiff :: TransformDiff 
          liftTransDiff (PathDependent trn) sim = scanl1 roller sim
              where 
                    roller (t1, s1) (t2, s2) = (t2, zipWith trn s1 s2)
          liftTransDiff (European trn) sim = fmap lift sim
              where
                    lift (t, s) = (t, fmap trn s)
          ---------------------------------
          mcEng = MCEngine {
                               diffSimulation = dfSim,
                               transfDiff = trnDff
                           }
          ---------------------------------
          dfSim = DiffSimulation { 
                                     simMethod = dMet,
                                     brownGen  = brGen,
                                     diffusion = df,
                                     mcTimes   = dfT,
                                     simulDiff = simulateDf
                                 }
          ---------------------------------
          brGen = BrownianGenerator {
                                        bTimes    = brT,
                                        numSim    = nSim,
                                        shocksGen = genSh
                                    }

--------------------------------------------------------------------------

generateShocks :: ShocksGenerator
generateShocks nSim brT = [(0.0, [[0.0]])]



------------------------------ AUX FUNCTIONS ------------------------------

from1toN :: (Double -> Double) -> [Double] -> Double
from1toN f (t:x:xs) = x * f t 
    
from0toN :: Double -> [Double] -> Double
from0toN d (t:x:xs) = x * d 








































