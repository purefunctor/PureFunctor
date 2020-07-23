module QuantumList where

import Data.List
import System.IO.Unsafe
import System.Random

{- 
-- QuantumList.hs
--
-- Expand/Collapse reality cause why not?
-- Licensed under MIT (c) 2020 PureFunctor
-}

newtype QuantumList a = QuantumList [[a]]

instance Show a => Show (QuantumList a) where
    show = show . collapse

expand :: Show a => [a] -> QuantumList a
expand = QuantumList . permutations

collapse :: Show a => QuantumList a -> [a]
collapse (QuantumList ql) = ql !! (unsafePerformIO $ (`mod` length ql) . abs <$> randomIO)

(!!!) :: Show a => QuantumList a -> Int -> a
ql !!! i = (collapse ql) !! i
