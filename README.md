# Crypto Portfolio

This is a simple prototype at the moment.

It currently:

* Reads my cointracking.info Trade List from a CSV
* Fetches the current prices of each of my coins from Binance
* Renders a table showing the amount, cost, value, % change & gains for each
  coin.

Some short term things it could do:

* Also calculate dollar amount using GDAX prices
* Use sales in cost basis(FIFO)
* Toggle between latest price & 24hr/7d/1mo/1yr average prices

And some bigger goals:

* Track things purchased w/ USD as well(e.g. portfolio & USD gains views)
* Have it's own trade management system(so I don't need to use cointracking)
* Track Income / Expenses / Transfers as well as Trades
* Allow Tagging/Grouping Trades
* Track Exchanges
* Import trades directly from GDAX & Binance API or exports
* Have additional views(portfolio, add/edit trades, watch list, alerts, etc.)

## Usage

Build it with `stack`:

    stack build

Go to the `Trades List` report on https://CoinTracking.info, filter it by ETH
sales, and save the CSV export. Move it to this directory and call it
`eth_trades.csv`.

Now run the app:

    stack exec crypto-portfolio-exe

Press `q` to quit.


## License

GPL-3.0
