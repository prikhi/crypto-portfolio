{-# LANGUAGE NamedFieldPuns #-}
module EthereumGains
    ( State
    , initial
    , getTransactions
    , updatePrice
    , view
    ) where

import Brick
import Control.Lens ((^.))
import Data.Maybe (mapMaybe, fromMaybe)

import qualified Brick.Widgets.Border as B
import qualified Data.Map as Map

import GainsTable
import Types
import qualified QuantityQueue as QQ


data State
    = State
        { gainsState :: GainsState
        , ethUSDPrice :: Maybe Quantity
        }

-- TODO: Get & display amount of USD-ETH bought/sold

-- | Build the Cache & Aggregates using the given Transactions.
initial :: [Transaction] -> State
initial ts =
    State
        { gainsState =
            initialGainsState addTransaction . reverse
                $ getTransactions ts
        , ethUSDPrice =
            Nothing
        }

-- | Filter out non-Ethereum or USD-ETH Transactions.
getTransactions :: [Transaction] -> [Transaction]
getTransactions =
    mapMaybe $ \transaction -> case transactionData transaction of
        Trade t ->
            if (tradeSellCurrency t == eth && tradeBuyCurrency t /= Currency "USD")
                    || (tradeBuyCurrency t == eth && tradeSellCurrency t /= Currency "USD") then
                Just transaction
            else
                Nothing
        _ ->
            Nothing

-- | Modify the ETH Currency Queues to Account for a new Transaction.
--
-- Everything but Altcoin<->ETH trades are ignored & the (ETH / altcoin)
-- ratio is used for the buy & sell prices.
addTransaction :: Transaction -> Queues -> Queues
addTransaction Transaction {transactionData} =
    case transactionData of
        Trade td ->
            let
                buyCurrency =
                    tradeBuyCurrency td
                buyQuantity =
                    tradeBuyQuantity td
                sellCurrency =
                    tradeSellCurrency td
                sellQuantity =
                    tradeSellQuantity td
                ethPrice =
                    case (buyCurrency, sellCurrency) of
                        (Currency "ETH", _) ->
                            buyQuantity / sellQuantity
                        _ ->
                            sellQuantity / buyQuantity
            in
                updateQueues (QQ.addSale sellQuantity ethPrice) sellCurrency
                    . updateFee td
                    . updateQueues (QQ.addPurchase buyQuantity ethPrice Nothing) buyCurrency
        _ ->
            id
    where
        updateFee :: TradeData -> Queues -> Queues
        updateFee td =
            case (,) <$> tradeFeeCurrency td <*> tradeFeeQuantity td of
                Just (currency, quantity) ->
                    updateQueues (QQ.addFee quantity) currency
                Nothing ->
                    id
        updateQueues :: (QQ.Queue -> QQ.Queue) -> Currency -> Queues -> Queues
        updateQueues f c =
            if c /= eth then
                Map.alter (Just . f . fromMaybe QQ.empty) c
            else
                id



-- | Update the CurrencyCache & AggregateData Given a New Price.
updatePrice :: State -> Currency -> Quantity -> State
updatePrice s@(State gs _) currency price =
    if currency == eth then
        s { ethUSDPrice = Just price }
    else
        s { gainsState = updateGainsState currency price gs }


-- | Render the Ethereum Gains Table
view :: State -> [Widget AppWidget]
view s@(State gs _) =
    [ vBox
        [ B.hBorderWithLabel (str " Ethereum Gains ")
        , B.border
            . padLeftRight 1
            . renderGainsTable EthereumGainsTable 8 (tableFooter s) gs
            . reverse
            . Map.foldlWithKey (\acc k v -> if k == eth then acc else (k, v) : acc) []
            $ gainsState s ^. currencyCache
        , statusBar s
        ]
    ]


-- | Render a Simple Status Bar Showing the Current USD Price for ETH.
-- TODO: Show 1d/1w/1m % Change
statusBar :: State -> Widget n
statusBar (State _ ethUSDPrice) =
    padLeft Max
        $ padRight (Pad 1)
        $ str
        $ maybe "Loading GDAX Stream..." (("ETH-USD: $" ++) . showQuantity 2)
            ethUSDPrice


-- | Render the Totals Row of the Table
tableFooter :: State -> [[Widget n]]
tableFooter (State s ethUSDPrice) =
    [ [ str " "
      , str " "
      , str " "
      , str " "
      , str " "
      , inUSD aTotalCost
      , inUSD aTotalValue
      , inUSD aUnrealizedGains
      , inUSD aRealizedGains
      ]
    ]
    where
        inUSD :: (AggregateData -> Quantity) -> Widget n
        inUSD f =
            alignRight
                . maybe "Loading..."
                    (("$" ++) . showQuantity 2 . (* f (s ^. aggregateData)))
                $ ethUSDPrice
        alignRight :: String -> Widget n
        alignRight = padLeft Max . str
