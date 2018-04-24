{- |

Generators used in multiple test modules.

-}
module Generators where

import Data.Ratio ((%))
import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Types
import QuantityQueue (PricePerUnit, Price)


-- | Generate a potentially 0 Quantity & PricePerUnit.
quantityAndPrice :: MonadGen m => m (Quantity, PricePerUnit)
quantityAndPrice =
    (,) <$> genQuantity <*> genQuantity


-- | Generate a Price up to some maximum.
boundedCost :: MonadGen m => Price -> m Price
boundedCost maxPrice = do
    quantity <- positiveQuantity
    let maxPricePerUnit = maxPrice / quantity
    pricePerUnit <- boundedQuantity maxPricePerUnit
    return $ quantity * pricePerUnit

-- | Generate a Price.
cost :: MonadGen m => m Price
cost =
    (*) <$> genQuantity <*> genQuantity

-- | Generate a Quantity up to some maximum.
boundedQuantity :: MonadGen m => Quantity -> m Quantity
boundedQuantity maxQuantity = do
    den <- positiveInteger
    let maxNumerator = truncate $ fromQuantity maxQuantity * toRational den
    num <- boundedNatural maxNumerator
    return . Quantity $ num % den
    where
        boundedNatural :: MonadGen m => Int -> m Integer
        boundedNatural b =
            fromIntegral <$> Gen.int (Range.constant 0 b)

-- | Generate a Quantity.
genQuantity :: MonadGen m => m Quantity
genQuantity = do
    num <- naturalInteger
    den <- positiveInteger
    return . Quantity $ num % den

-- | Generate a positive Quantity.
positiveQuantity :: MonadGen m => m Quantity
positiveQuantity = do
    num <- positiveInteger
    den <- positiveInteger
    return . Quantity $ num % den

-- | Generate a positive Int & cast it to an Integer.
positiveInteger :: MonadGen m => m Integer
positiveInteger =
    fromIntegral <$> Gen.int (Range.constant 1 maxBound)

-- | Generate a non-negative Int & cast it to an Integer.
naturalInteger :: MonadGen m => m Integer
naturalInteger =
    fromIntegral <$> Gen.int (Range.constant 0 maxBound)
