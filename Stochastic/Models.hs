data BlackScholles = BlackScholles {
                                       muBS    :: R_N, 
                                       sigmaBS :: R_N_M
                                   }

instance Diffusion BlackScholles where  
    driftMD bs     = fmap from0toN (muBS bs)
    diffCoefsMD bs = fmap (fmap from0toN) (sigmaBS bs)

data LogNormalProcess = LogNormalProcess {
                                             muLN    :: [F_t],
                                             sigmaLN :: [[F_t]]
                                         }

instance Diffusion LogNormalProcess where  
    driftMD ln     = fmap from1toN (muLN ln)
    diffCoefsMD ln = fmap (fmap from1toN) (sigmaLN ln)
