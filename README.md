# Crypto Portfolio

[![Build Status](https://travis-ci.org/prikhi/crypto-portfolio.svg?branch=master)](https://travis-ci.org/prikhi/crypto-portfolio)

This is a simple prototype at the moment.

It currently:

* Reads my cointracking.info Trades from a CSV
* Streams the current price of ETH in USD from GDAX
* Streams the current prices of each of my coins from Binance
* Shows the amount, cost, value, % change & gains for each coin.
* Shows the total cost, value, & gains for altcoin portfolio, in ETH & USD.
* Shows a list of all Trades, Income, Expenses, & Transfers.

![The User Interface of the Crypto Portfolio Program, Showing a Table of Costs, Values, and Gains for Several Cryptocurrencies](http://bugs.sleepanarchy.com/projects/crypto-portfolio/repository/revisions/master/entry/screenshot.png "KSP Automation Screenshot")


Some short term things it could do:

* Use sales in cost basis(FIFO)
* Toggle between latest price & 24hr/7d/1mo/1yr average prices
* Dashboard view - cointracking for inspiration
* USD portfolio view
* Sorting tables
* Gracefully handle network & decoding errors, re-launch price process for exceptions.
* Colors

And some bigger goals:

* Track things purchased w/ USD as well(e.g. portfolio & USD gains views)
* Have it's own trade management system(so I don't need to use cointracking)
* Allow Tagging/Grouping Trades
* Track Exchanges
* Import trades directly from GDAX & Binance API or exports
* Have additional views(portfolio, add/edit trades, watch list, alerts, etc.)
* Release Table module as Brick Widget package
* Debug/info logging
* Coin research views(subreddit, cmc data, wikipedia, google trends)
* Servant JSON API w/ Elm frontend


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
