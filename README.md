# Crypto Portfolio

[![Build Status](https://travis-ci.org/prikhi/crypto-portfolio.svg?branch=master)](https://travis-ci.org/prikhi/crypto-portfolio)

This is an early alpha of a Cryptocurrency Portfolio management application
with a terminal UI.

Right now it's mostly useful if you trade mostly on GDAX or Binance & have your
transactions entered in at http://CoinTracking.info.

## Features

It currently:

* Reads a cointracking.info Trades Table CSV export.
* Streams the current USD price of each coin from GDAX.
* Streams the current ETH or BTC prices of each of my coins from Binance.
* Shows an Ethereum Gains table: the amount, cost, value, % change, & gains for
  any altcoins traded with ETH, with totals in both ETH & USD.
* Shows a USD Gains table: the amount, cost, value, % change, & gains for every
  coin traded, earned, & spent.
* Shows a list of all Trades, Income, Expenses, & Transfers.

![The User Interface of the Crypto Portfolio Program, Showing a Table of Costs, Values, and Gains for Several Cryptocurrencies](http://bugs.sleepanarchy.com/projects/crypto-portfolio/repository/revisions/master/entry/screenshot.png "KSP Automation Screenshot")


## Usage

Build it with `stack`:

    stack build

Go to the `Enter Coins` page on https://CoinTracking.info, hit the `Export`
button and choose `CSV`. Move it to this directory and call it
`trade_table.csv`.

Now run the app:

    stack exec crypto-portfolio

Press `n`/`p` to cycle through the views & `q` to quit. You can scroll the
tables with the following keys:

    j - scroll down 1 line
    k - scroll up 1 line
    Ctrl-d - scroll down 1/2 page
    Ctrl-u - scroll up 1/2 page
    Ctrl-f - scroll down a page
    Ctrl-b - scroll up a page
    g - go to first row
    G - go to last row


## Contribute

Lots to do, PRs gratiously accepted. For big things, please open an issue with
your intentions so we can be on the same page.

You can find more refactoring/cleanup sort of stuff by greping the `src` and
`app` folders for `TODO`.

### Short Term

* Write script to generate random data(so I don't have to make up data when I
  want to take a screenshot), maybe using historical prices so gains are
  realistic.
* "Privacy" mode that obfuscates amounts but still shows percents/prices
* Highlight focused table row(skipping horizontal dividers) 
    * Completely highlight multi-line rows
* Toggle between latest price & 24hr/7d/1mo/1yr average prices
* Dashboard view - cointracking for inspiration
    * current BTC & ETH price
    * total coin value in USD
    * total USD investment
    * current account value & 24hr change
    * currency table w/ quantity, value / unrealized gains, price, & % change
    * balance per exchange/account and/or currency
    * Bar graph of each currency's portfolio percentage(coin val / total val)
        * Could use stacked Brick.Widgets.ProgressBar for horizontal graph
        * See https://github.com/christ0ph3r/cryptocurrency-cli for inspiration
* Colors
    * green for positive gains & percents, red for negative?
    * Color "Type" column in trades list?
    * `Graphics.Vty.Attributes` offers 240 color support.

## Long Term

* Allow changing Altcoin pair to other GDAX currencies(currently hardcoded to ETH)
    * Could change Ethereum Gains to Altcoin Gains view.
* Sorting Tables
    * Add `Maybe (sorting function)` to Column type
    * Keybinding to sort by previous/next column & toggle sort direction
    * Unicode up/down arrows in header of current sorted column
* Filtering Tables, Searching Transactions
    * Allow text filtering of any column, like CoinTracking
* Have it's own trade management system(so I don't need to use cointracking)
    * Mimic the CoinTracking "Enter Coins" page?
    * Allow editing Trades directly from Trades List view
    * Database: Persistent/Esqueleto, ACID State, or Beam? Needs `ReaderT IO`
      in update.
    * Full data exports in CSV and/or XLS/ODS.
    * Allow Tagging/Grouping Trades(Search/Filter)
    * Track Exchanges(Search/Filter)
    * Import trades directly from GDAX & Binance API or exports instead of
      CoinTracking. Maybe add them as "unapproved" until edited/verified by
      User.
* Have additional views(dashboard, add/edit trades, watch list, alerts, etc.)
    * Current Balance - amount, value, % of holdings, USD price, 1h/24h/7d/30d
      price trends.
    * Trade Analysis - CoinTracking only shows this for a single coin to
      another coin. Maybe abstract the Ethereum Gains view so it works w/ any
      fiat-market coin & add an inline trade table?
    * Fee Report - Table w/ every transaction that has a fee, w/ fee amount &
      currency, USD value at transaction date, USD value now, date, & totals in
      the footer.
    * See CoinTracking for inspiration of other views / reports to add.
* Desktop Notifications
    * Price Alerts
    * When new Transaction imported from GDAX / Binance APIs
* Release Table module as Brick Widget package
* Release Binance & GDAX modules as package(even though it's incomplete)
* Debug/info/error logging(Katip or fast-logger packages?), maybe `Message Log`
  view or just log to file.
* Coin research views(subreddit, cmc data, wikipedia, google trends)
* Servant JSON API w/ Elm frontend? Or try reflex-platform for easy
  web/desktop/mobile cross-compilation?
* User-definable colorschemes, see `Brick.Themes` module.
* Support more exchanges if anyone else cares about this.


## License

GPL-3.0
