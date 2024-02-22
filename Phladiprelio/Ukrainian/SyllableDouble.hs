{-# LANGUAGE TypeSynonymInstances, NoImplicitPrelude, Strict #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-- |
-- Module      :  Phladiprelio.Ukrainian.SyllableDouble
-- Copyright   :  (c) Oleksandr Zhabenko 2021-2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- This module works with syllable segmentation in Ukrainian. Uses 'Double' whenever possible.
-- Is inspired by the DobutokO.Sound.DIS5G6G module from @dobutokO2@ package.
-- See: 'https://hackage.haskell.org/package/dobutokO2-0.43.0.0/docs/DobutokO-Sound-DIS5G6G.html'.
-- The initial 'Double' data are gotten from there.

module Phladiprelio.Ukrainian.SyllableDouble where

import GHC.Base
import GHC.Num ((+))
import CaseBi.Arr
import Phladiprelio.Ukrainian.Melodics (Sound8)
import GHC.Int
import Data.Foldable (foldl')

s0DuratD1 :: Sound8 -> Double
s0DuratD1 = getBFstLSorted' 0.051020 [(1,0.138231),(2,9.3605e-2),(3,0.116463),(4,0.10907),(5,9.9955e-2),(6,9.415e-2),(7,2.0227e-2),(8,5.5601e-2),(9,5.5601e-2),(10,7.0658e-2),(11,7.0658e-2),(15,5.7143e-2),(16,5.7143e-2),(17,7.2063e-2),(18,7.2063e-2),(19,6.2948e-2),(20,6.2948e-2),(21,7.6825e-2),(22,7.6825e-2),(23,4.8934e-2),(24,4.8934e-2),(25,5.6054e-2),(26,5.6054e-2),(27,5.7143e-2),(28,6.4036e-2),(29,6.4036e-2),(30,7.737e-2),(31,7.737e-2),(32,7.424e-2),(33,7.424e-2),(34,4.9206e-2),(35,4.9206e-2),(36,8.2268e-2),(37,8.2268e-2),(38,5.3061e-2),(39,5.7596e-2),(40,5.7596e-2),(41,6.6077e-2),(42,6.6077e-2),(43,6.2268e-2),(44,6.2268e-2),(45,4.5351e-2),(46,4.5351e-2),(47,0.13483),(48,0.13483),(49,7.4603e-2),(50,0.110658),(51,0.110658),(52,7.7188e-2),(53,7.7188e-2),(54,7.4558e-2),(66,8.9342e-2)]
{-# INLINE s0DuratD1 #-}

-- |
s0DuratD2 :: Sound8 -> Double
s0DuratD2 = getBFstLSorted' 0.06408817 [(1,0.27161466),(2,0.27192511),(3,0.28539351),(4,0.25250039),(5,0.2050935),(6,0.20026538),(7,2.218624e-2),(8,7.729654e-2),(9,7.729654e-2),(10,8.048113e-2),(11,8.048113e-2),(15,0.10977617),(16,0.10977617),(17,6.58655e-2),(18,6.58655e-2),(19,7.751571e-2),(20,7.751571e-2),(21,5.392745e-2),(22,5.392745e-2),(23,8.900757e-2),(24,8.900757e-2),(25,6.099951e-2),(26,6.099951e-2),(27,8.226452e-2),(28,0.11159399),(29,0.11159399),(30,0.14303837),(31,0.14303837),(32,5.639178e-2),(33,5.639178e-2),(34,6.354637e-2),(35,6.354637e-2),(36,8.404524e-2),(37,8.404524e-2),(38,5.616409e-2),(39,0.12541547),(40,0.12541547),(41,0.12838476),(42,0.12838476),(43,0.15776219),(44,0.15776219),(45,4.91782e-2),(46,4.91782e-2),(47,9.603085e-2),(48,9.603085e-2),(49,5.294375e-2),(50,5.047358e-2),(51,5.047358e-2),(52,7.905155e-2),(53,7.905155e-2),(54,7.512999e-2),(66,7.835033e-2)]
{-# INLINE s0DuratD2 #-}

s0DuratD3 :: Sound8 -> Double
s0DuratD3 = getBFstLSorted' 0.05779993 [(1,0.25872483),(2,0.22876537),(3,0.25423777),(4,0.20243791),(5,0.19849003),(6,0.19777405),(7,1.943042e-2),(8,8.453724e-2),(9,8.453724e-2),(10,9.996042e-2),(11,9.996042e-2),(15,0.13787716),(16,0.13787716),(17,7.437409e-2),(18,7.437409e-2),(19,7.985903e-2),(20,7.985903e-2),(21,0.10289067),(22,0.10289067),(23,0.10039843),(24,0.10039843),(25,6.643842e-2),(26,6.643842e-2),(27,0.10975353),(28,0.1090645),(29,0.1090645),(30,0.14576594),(31,0.14576594),(32,6.084464e-2),(33,6.084464e-2),(34,5.937718e-2),(35,5.937718e-2),(36,7.798724e-2),(37,7.798724e-2),(38,5.901357e-2),(39,0.11906522),(40,0.11906522),(41,0.13985258),(42,0.13985258),(43,0.15880087),(44,0.15880087),(45,5.893304e-2),(46,5.893304e-2),(47,0.10765654),(48,0.10765654),(49,6.247632e-2),(50,6.03912e-2),(51,6.03912e-2),(52,0.13526622),(53,0.13526622),(54,8.190674e-2),(66,7.8444e-2)]
{-# INLINE s0DuratD3 #-}

s0DuratD4 :: Sound8 -> Double
s0DuratD4 = getBFstLSorted' 0.14160713 [(1,0.20859653),(2,0.21194045),(3,0.2089092),(4,0.19826109),(5,0.20249813),(6,0.20167924),(7,1.957491e-2),(8,8.508446e-2),(9,8.508446e-2),(10,0.17053331),(11,0.17053331),(15,7.768941e-2),(16,7.768941e-2),(17,0.12987485),(18,0.12987485),(19,0.14343568),(20,0.14343568),(21,0.22822145),(22,0.22822145),(23,0.16712392),(24,0.16712392),(25,8.566847e-2),(26,8.566847e-2),(27,6.241711e-2),(28,0.16563571),(29,0.16563571),(30,0.2694089),(31,0.2694089),(32,0.13174949),(33,0.13174949),(34,5.978079e-2),(35,5.978079e-2),(36,9.572877e-2),(37,9.572877e-2),(38,5.705798e-2),(39,0.21173804),(40,0.21173804),(41,0.24441358),(42,0.24441358),(43,0.19044721),(44,0.19044721),(45,0.10747824),(46,0.10747824),(47,5.737927e-2),(48,5.737927e-2),(49,0.10201693),(50,0.18138075),(51,0.18138075),(52,0.26765448),(53,0.26765448),(54,0.12159184),(66,7.663289e-2)]
{-# INLINE s0DuratD4 #-}

class (Eq a) => SyllableDurations4 a where
  sDuratsD :: a -> Double
  sDuratsD2 :: a -> Double
  sDuratsD3 :: a -> Double
  sDuratsD4 :: a -> Double
  syllableDurationsGDc :: (a -> Double) -> [[[a]]] -> [[Double]]
  syllableDurationsGDc g = map (map (k g))
    where k f = foldl' (\y x -> f x + y) 0
  {-# INLINE syllableDurationsGDc #-}

instance SyllableDurations4 Sound8 where
  sDuratsD = s0DuratD1
  sDuratsD2 = s0DuratD2
  sDuratsD3 = s0DuratD3
  sDuratsD4 = s0DuratD4

-- | General variant of the 'syllableDurationsD' function.
syllableDurationsGD :: (Sound8 -> Double) -> [[[Sound8]]] -> [[Double]]
syllableDurationsGD g = syllableDurationsGDc g
{-# INLINE syllableDurationsGD #-}

syllableDurationsD :: [[[Sound8]]] -> [[Double]]
syllableDurationsD = syllableDurationsGDc s0DuratD1
{-# INLINE syllableDurationsD #-}

syllableDurationsD2 :: [[[Sound8]]] -> [[Double]]
syllableDurationsD2 = syllableDurationsGDc s0DuratD2
{-# INLINE syllableDurationsD2 #-}

syllableDurationsD3 :: [[[Sound8]]] -> [[Double]]
syllableDurationsD3 = syllableDurationsGDc s0DuratD3
{-# INLINE syllableDurationsD3 #-}

syllableDurationsD4 :: [[[Sound8]]] -> [[Double]]
syllableDurationsD4 = syllableDurationsGDc s0DuratD4
{-# INLINE syllableDurationsD4 #-}

