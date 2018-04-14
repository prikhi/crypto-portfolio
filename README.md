# Crypto Portfolio

[![Build Status](https://travis-ci.org/prikhi/crypto-portfolio.svg?branch=master)](https://travis-ci.org/prikhi/crypto-portfolio)

This is a simple prototype at the moment.

It currently:

* Reads my cointracking.info Trades from a CSV.
* Streams the current USD price of each coin from GDAX.
* Streams the current ETH or BTC prices of each of my coins from Binance.
* Shows an Ethereum Gains table: the amount, cost, value, % change, & gains for
  any altcoins I've traded with ETH, with totals in both ETH & USD.
* Shows a USD Gains table: the amount, cost, value, % change, & gains for every
  coin I've traded, earned, & spent.
* Shows a list of all Trades, Income, Expenses, & Transfers.

![The User Interface of the Crypto Portfolio Program, Showing a Table of Costs, Values, and Gains for Several Cryptocurrencies](http://bugs.sleepanarchy.com/projects/crypto-portfolio/repository/revisions/master/entry/screenshot.png "KSP Automation Screenshot")


Some short term things it could do:

* Write script to generate random data(so I don't have to make up data when I
  want to take a screenshot), maybe using historical prices so gains are
  realistic.
* "Privacy" mode that obfuscates amounts but still shows percents/prices
* Highlight focused table row(selection cursor that skips hBorders?)
* Toggle between latest price & 24hr/7d/1mo/1yr average prices
* Dashboard view - cointracking for inspiration
    * current BTC & ETH price
    * total coin value in USD
    * total USD investment
    * current account value & 24hr change
    * currency table w/ quantity, unrealized gains, price, & % change
    * balance per exchange/account
* Sorting tables
* Colors
    * green for positive gains & percents, red for negative?
    * Color type in trades list?

And some bigger goals:

* Allow changing Altcoin pair to other GDAX currencies(currently hardcoded to ETH)
* Filtering Tables(see CoinTracking filters)
* Have it's own trade management system(so I don't need to use cointracking)
    * Database: Persistent/Esqueleto or ACID? Needs `ReaderT IO` in update.
* Allow Tagging/Grouping Trades(Search/Filter)
* Track Exchanges(Search/Filter)
* Import trades directly from GDAX & Binance API or exports
* Have additional views(dashboard, add/edit trades, watch list, alerts, etc.)
* Price alerts that send desktop notifications
* Release Table module as Brick Widget package
* Release Binance & GDAX modules as package(even though it's incomplete)
* Debug/info/error logging(Katip or fast-logger packages?)
* More reports, see CoinTracking for inspiration
* Coin research views(subreddit, cmc data, wikipedia, google trends)
* Servant JSON API w/ Elm frontend? Or try reflex-platform for easy
  web/desktop/mobile cross-compilation?


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


## License

GPL-3.0
