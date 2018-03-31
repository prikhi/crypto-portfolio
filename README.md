# Crypto Portfolio

[![Build Status](https://travis-ci.org/prikhi/crypto-portfolio.svg?branch=master)](https://travis-ci.org/prikhi/crypto-portfolio)

This is a simple prototype at the moment.

It currently:

* Reads my cointracking.info Trade List from a CSV
* Streams the current price of ETH in USD from GDAX
* Streams the current prices of each of my coins from Binance
* Shows the amount, cost, value, % change & gains for each coin.
* Shows the total cost, value, & gains for portfolio, in ETH & USD.

![The User Interface of the Crypto Portfolio Program, Showing a Table of Costs, Values, and Gains for Several Cryptocurrencies](http://bugs.sleepanarchy.com/projects/crypto-portfolio/repository/revisions/master/entry/screenshot.png "KSP Automation Screenshot")


Some short term things it could do:

* Use sales in cost basis(FIFO)
* Toggle between latest price & 24hr/7d/1mo/1yr average prices
* Sorting tables
* Gracefully handle network & decoding errors
* Colors

And some bigger goals:

* Track things purchased w/ USD as well(e.g. portfolio & USD gains views)
* Have it's own trade management system(so I don't need to use cointracking)
* Track Income / Expenses / Transfers as well as Trades
* Allow Tagging/Grouping Trades
* Track Exchanges
* Import trades directly from GDAX & Binance API or exports
* Have additional views(portfolio, add/edit trades, watch list, alerts, etc.)
* Refactor table rendering into it's own module, maybe release as Brick Widget
* Debug/info logging
* Coin research views(subreddit, cmc data, wikipedia)


## Usage

Build it with `stack`:

    stack build

Go to the `Enter Coins` page on https://CoinTracking.info, hit the `Export`
button and choose `CSV`. Move it to this directory and call it
`trade_table.csv`.

Now run the app:

    stack exec crypto-portfolio

Press `q` to quit.


## License

GPL-3.0
