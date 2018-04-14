{-# LANGUAGE TemplateHaskell #-}
{-|

QuantityQueue's are FIFO queues for tracking the available quantity, cost
basis, & realized gains for a Currency.

TODO: Implement Wash Sales for tax purposes

TODO: Support Debt: when sale/fee amount > available amount

TODO: Make Price == (Quantity, Currency) & maintain invariant that all
PricePerUnit in a Queue is in the same currency.

TODO: Use newtypes, define Price and/or PricePerUnit in Types module

-}
module QuantityQueue
    ( PricePerUnit
    , Price
    , Queue
    , empty
    , totalQuantity
    , totalCost
    , realizedGains
    , addPurchase
    , addSale
    , addFee
    ) where

import Control.Arrow (first)
import Control.Lens ((^.), (.~), (%~), (&), makeLenses)
import Data.Foldable (toList)

import qualified Data.Sequence as S

import Types


-- | The Price-Per-Unit of a Currency
type PricePerUnit = Quantity

-- | A Total Price for a Purchase, Sale, or Fee
type Price = Quantity

-- | A FIFO Queue for Currency Purchases
data Queue =
    QuantityQueue
        { _qqQueue :: S.Seq (Quantity, PricePerUnit)
        , _qqGains :: Quantity
        } deriving Show
makeLenses ''Queue


-- CREATE

empty :: Queue
empty =
    QuantityQueue
        { _qqQueue = S.empty
        , _qqGains = 0
        }


-- QUERY

-- | Fetch the Available Amount in a Queue.
totalQuantity :: Queue -> Quantity
totalQuantity queue =
    queue ^. qqQueue
        & toList
        & map fst
        & sum

-- | Fetch the Total Cost for a Queue.
totalCost :: Queue -> Quantity
totalCost queue =
    foldl (\s (q, p) -> s + q * p) 0
        $ queue ^. qqQueue & toList

-- | Fetch the Realized Gains for a Queue.
realizedGains :: Queue -> Quantity
realizedGains queue =
    queue ^. qqGains


-- MANIPULATE

-- | Add a Purchase to the Queue, adjusting the Cost Basis to account for
-- portential fees.
-- TODO: Handle negative prices in front of queue
addPurchase :: Quantity -> PricePerUnit -> Maybe Price -> Queue -> Queue
addPurchase quantity currencyPrice maybeFeeTotal queue =
    let
        price = case maybeFeeTotal of
            Just feeTotal ->
                ((currencyPrice * quantity) + feeTotal) / quantity
            Nothing ->
                currencyPrice
    in
        case queue ^. qqQueue of
            S.Empty ->
                queue & qqQueue .~ S.singleton (quantity, price)
            rest S.:|> r ->
                if snd r == price then
                    queue & qqQueue .~ rest S.|> first (+ quantity) r
                else
                    queue & qqQueue %~ (S.|> (quantity, price))


-- | Remove the given Quantity from the front of Queue & use the sale Price
-- & purchase Price to adjust the realized gains.
-- TODO: Handle empty queue, increase negative prices in front of queue
addSale :: Quantity -> PricePerUnit -> Queue -> Queue
addSale quantity salePrice queue = case queue ^. qqQueue of
    S.Empty ->
        error $ "addSale: empty queue" -- TODO: Handle negative quantities
            ++ " " ++ show quantity
    (nextAmount, nextPrice) S.:<| rest
        | nextAmount > quantity ->
            queue
                & qqQueue .~ (nextAmount - quantity, nextPrice) S.<| rest
                & qqGains %~ (+) ((salePrice - nextPrice) * quantity)
        | nextAmount < quantity ->
            addSale (quantity - nextAmount) salePrice
                $ queue
                    & qqQueue .~ rest
                    & qqGains %~ (+) ((salePrice - nextPrice) * nextAmount)
        | otherwise ->
            queue & qqQueue .~ rest
                  & qqGains %~ (+) ((salePrice - nextPrice) * nextAmount)


-- | Remove the Fee Quantity without changing the realized gains.
--
-- This is done because the cost of fees is factored into the cost basis of
-- purchased currencies for tax purposes, so we don't want to claim them as
-- gains.
--
-- TODO: Handle empty & negatie queues.
addFee :: Quantity -> Queue -> Queue
addFee quantity queue = case queue ^. qqQueue of
    S.Empty ->
        error $ "addFee: empty queue" ++ " " ++ show quantity
    (nextAmount, nextPrice) S.:<| rest
        | nextAmount > quantity ->
            queue & qqQueue .~ (nextAmount - quantity, nextPrice) S.<| rest
        | nextAmount < quantity ->
            queue & qqQueue .~ rest
        | otherwise ->
            queue & qqQueue .~ rest
