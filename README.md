# Crypto Portfolio

Maybe eventually something more useful.

I want something I can track eth gains with.

That'll hook up to GDAX & Binance for prices.

Just gets data by reading cointracking trades for now.

Maybe some day it'll have forms for entering transactions & I can replace coin
tracking w/ it.

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
