{- |

Test the QuantityQueue module.

-}
{-# LANGUAGE LambdaCase #-}
module QuantityQueueSpec where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Generators
import QuantityQueue
import Types


spec :: Spec
spec = parallel $ do
    generalSpec
    emptySpec
    washSaleSpec
    addPurchaseSpec
    addSaleSpec
    addFeeSpec


generalSpec :: Spec
generalSpec = describe "QuantityQueue" $ do
    parallel $ it "never gives a negative quantity with only purchases" $
        require $ property $ do
            purchaseCount <- forAll . Gen.int $ Range.constant 1 20
            withPurchases <- forAll $ applyPurchases purchaseCount empty
            assert $ totalQuantity withPurchases > 0
            realizedGains withPurchases === 0
    it "always gives a negative quantity with only sales" $
        pendingWith "NEGATIVE QUANTITIES UNIMPLEMENTED"
    it "always gives a negative quantity with only fees" $
        pendingWith "NEGATIVE QUANTITIES UNIMPLEMENTED"
    where
        applyPurchases n queue =
            if n > 0 then do
                (quantity, price) <- (,) <$> positiveQuantity <*> genQuantity
                maybeFee <- Gen.maybe $ boundedQuantity quantity
                applyPurchases (n - 1)
                    $ addPurchase quantity price maybeFee queue
            else
                return queue


emptySpec :: Spec
emptySpec = describe "empty" $ do
    it "returns zero for the totals & gains" $ do
        totalQuantity empty `shouldBe` 0
        totalCost empty `shouldBe` 0
        realizedGains empty `shouldBe` 0
    it "properly handles feeless purchases" $
        require $ property $ do
            (quantity, price) <- forAll quantityAndPrice
            let withPurchase = addPurchase quantity price Nothing empty
            totalQuantity withPurchase === quantity
            totalCost withPurchase === quantity * price
            realizedGains withPurchase === 0
    it "properly handles purchases with fees" $
        require $ property $ do
            (quantity, price) <- forAll quantityAndPrice
            feeTotal <- forAll $ boundedQuantity quantity
            let withPurchase = addPurchase quantity price (Just feeTotal) empty
            totalQuantity withPurchase === quantity
            totalCost withPurchase === (quantity * price) + feeTotal
            realizedGains withPurchase === 0


washSaleSpec :: Spec
washSaleSpec = describe "a feeless wash sale" $ do
    it "does not change the total quantity" $
        require $ property $ do
            (quantity, price) <- forAll quantityAndPrice
            initialQueue <- forAll genQueue
            let washSale = feelessWashSale quantity price initialQueue
            totalQuantity washSale === totalQuantity initialQueue
    it "does not change the cost total or realized gains" $
        pendingWith "WASH SALE UNIMPLEMENTED"
    where
        feelessWashSale q p =
            addSale q p . addPurchase q p Nothing


addPurchaseSpec :: Spec
addPurchaseSpec = describe "addPurchase" $ do
    it "increments the totals & does not touch the gains" $
        require $ property $ do
            queue <- forAll genQueue
            (quantity, price) <- forAll quantityAndPrice
            let withPurchase = addPurchase quantity price Nothing queue
            totalQuantity withPurchase === quantity + totalQuantity queue
            totalCost withPurchase === (price * quantity) + totalCost queue
            realizedGains withPurchase === realizedGains queue
    it "adds any fees to the cost basis" $
        require $ property $ do
            queue <- forAll genQueue
            (quantity, price) <- forAll quantityAndPrice
            fee <- forAll . boundedQuantity $ quantity * price
            let withPurchase = addPurchase quantity price (Just fee) queue
            totalCost withPurchase === (price * quantity + fee) + totalCost queue
    it "removes any applicable debit" $
        pendingWith "NEGATIVE QUANTITIES UNIMPLEMENTED"


addSaleSpec :: Spec
addSaleSpec = describe "addSale" $ do
    it "decrements the total quantity" $
        require $ property $ do
            queue <- forAll genQueue
            quantity <- forAll . boundedQuantity $ totalQuantity queue
            price <- forAll genQuantity
            let withSale = addSale quantity price queue
            totalQuantity withSale === totalQuantity queue - quantity
    it "modifies the cost & realized gains" $
        require $ property $ do
            (initialQueue, buyQuantity, buyPrice, maybeFee) <-
                forAll queueWithPurchase
            let costBasis = buyPrice + maybe 0 (/ buyQuantity) maybeFee
            firstSellQuantity <- forAll $ boundedQuantity buyQuantity
            firstSellPrice <- forAll genQuantity
            let afterFirstSale = addSale firstSellQuantity firstSellPrice initialQueue
            totalCost afterFirstSale ===
                costBasis * (buyQuantity - firstSellQuantity)
            realizedGains afterFirstSale === (firstSellPrice - costBasis) * firstSellQuantity
            secondQuantity <- forAll $ boundedQuantity $ buyQuantity - firstSellQuantity
            secondPrice <- forAll genQuantity
            let afterSecondSale = addSale secondQuantity secondPrice afterFirstSale
                secondCost =
                    costBasis
                        * (buyQuantity - firstSellQuantity - secondQuantity)
                secondGains =
                    realizedGains afterFirstSale + (secondPrice - costBasis) * secondQuantity
            totalCost afterSecondSale === secondCost
            realizedGains afterSecondSale === secondGains
    it "reports a debt when selling more than the available quantity" $
        pendingWith "NEGATIVE QUANTITIES UNIMPLEMENTED"


addFeeSpec :: Spec
addFeeSpec = describe "addFee" $ do
    it "decrements the total quantity, & does not change the gains" $
        require $ property $ do
            queue <- forAll genQueue
            quantity <- forAll $ boundedQuantity $ totalQuantity queue
            let withFee = addFee quantity queue
            totalQuantity withFee === totalQuantity queue - quantity
            if quantity == 0 || totalCost queue == 0 then
                totalCost withFee === totalCost queue
            else
                assert $ totalCost withFee /= totalCost queue
            realizedGains withFee === realizedGains queue
    it "modifes the cost" $
        require $ property $ do
            (initialQueue, buyQuantity, buyPrice, maybeFee) <-
                forAll queueWithPurchase
            let costBasis = buyPrice + maybe 0 (/ buyQuantity) maybeFee
            feeAmount <- forAll $ boundedQuantity buyQuantity
            let afterFee = addFee feeAmount initialQueue
            if buyPrice > 0 then
                totalCost afterFee === (buyQuantity - feeAmount) * costBasis
            else
                totalCost afterFee === totalCost initialQueue




-- UTILS

-- | Generate a Queue with a single purchase.
queueWithPurchase :: MonadGen m => m (Queue, Quantity, PricePerUnit, Maybe Price)
queueWithPurchase = do
    quantity <- positiveQuantity
    price <- genQuantity
    maybeFee <- Gen.maybe . boundedCost $ quantity * price
    let initialQueue = addPurchase quantity price maybeFee empty
    return (initialQueue, quantity, price, maybeFee)


-- | Generate a Queue with 0-30 purchases, sells, & fees.
genQueue :: MonadGen m => m Queue
genQueue = do
    manipulationCount <- Gen.int (Range.constant 0 30)
    randomize manipulationCount empty
    where
        randomize :: MonadGen m => Int -> Queue -> m Queue
        randomize manipulationCount queue
            | manipulationCount <= 0 =
                return queue
            | otherwise = do
                let availableQuantity = totalQuantity queue
                    actionRange =
                        if availableQuantity > 0 then
                            3
                        else
                            1
                newQueue <- Gen.int (Range.constant 1 actionRange) >>= \case
                    1 -> do
                        (quantity, price) <- quantityAndPrice
                        maybeFee <- Gen.maybe $ boundedCost $ quantity * price
                        return $ addPurchase quantity price maybeFee queue
                    2 -> do
                        quantity <- boundedQuantity availableQuantity
                        price <- genQuantity
                        return $ addSale quantity price queue
                    _ -> do
                        quantity <- boundedQuantity availableQuantity
                        return $ addFee quantity queue
                randomize (manipulationCount - 1) newQueue
