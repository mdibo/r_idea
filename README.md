# r_idea
这里用R来检验交易想法。

idea来源与互联网，和朋友交流以及突发奇想。

不是每个交易想法都能变成策略，很多都是简单的尝试，真形成可交易策略，我也不会把参数放在这里呀 :)

文件的简单说明：
- pair_trading.R
> [Pair Trading Strategy and Backtesting using Quantstrat](http://www.quantinsti.com/blog/pair-trading-strategy-backtesting-using-quantstrat/)
>
> By	Marco Bicolas Dibo

- pair_trading_bank_cn.R
> 由前启发，对四大行捉对检查。price ratio's z-score 的 mean reverting 的性质是有的，但是能不能用于交易就不好说了。

- others
	- bank_stock_fuquan.py sina数据小爬虫，含复权因子。
	- bank_stock_market.py sina数据小爬虫，和复权数据比对。
	- tk.csv tushare中通联数据的token

- data
> 代码中用到的部分数据。
