############################################################################################################
# install packages not in official source
############################################################################################################

# run only once 
# install.packages("quantstrat", repos="http://R-Forge.R-project.org")
# install.packages("devtools")
# library("devtools")
# install_github("IlyaKipnis/IKTrading")


############################################################################################################
# Pair Trading Strategy and Backtesting using Quantstrat
# http://www.quantinsti.com/blog/pair-trading-strategy-backtesting-using-quantstrat/
# Marco Bicolas Dibo
# 我这里学习一下作者的想法，并不准备使用他的quantstrat. 
# 只有自己亲自写的回测才是好的回测。
# 作者贴出来的代码好可怕，好多遗漏的地方，函数内部有调用外部参数而不注明
############################################################################################################

# Load libraries

library(quantstrat)
library(tseries)
library(IKTrading)
library(PerformanceAnalytics)

# .blotter holds the portfolio and account object and .strategy holds the orderbook and strategy object

.blotter <- new.env()
.strategy <- new.env()

# fetch market data and plot the spread
startDate <- "2009-01-01"
endDate <- "2012-12-31"
symb1 <- 'C'
symb2 <- 'BAC'
getSymbols(symb1, from=startDate, to=endDate, adjust=TRUE)

getSymbols(symb2, from=startDate, to=endDate, adjust=TRUE)

spread <- OHLC(C)-OHLC(BAC)
colnames(spread)<-c("open","high","low","close")

symbols <- c("spread")
stock(symbols, currency = 'USD', multiplier = 1)

chart_Series(spread)

add_TA(EMA(Cl(spread), n=20), on=1, col="blue", lwd=1.5)
legend(x=5, y=50, legend=c("EMA 20"),
       fill=c("blue"), bty="n")



#Inititalize strategy, portfolio, account and orders
initDate <- "2009-01-01"
initEq <- 10000
qs.strategy <- 'pairStrat'
initPortf(qs.strategy, symbols = symbols, initDate=initDate)

initAcct(qs.strategy, portfolios=qs.strategy, initDate=initDate,initEq=initEq)

initOrders(qs.strategy,initDate=initDate)
# Save strategy
strategy(qs.strategy, store = TRUE)
# rm.strat(pairStrat) # only when trying a new test
ls(.blotter) # .blotter holds the portfolio and account object
ls(.strategy) # .strategy holds the orderbook and strategy object

# a) Z-Score
PairRatio <- function(x) { #returns the ratio of close prices for 2 symbols
  x1 <- get(x[1])
  x2 <- get(x[2])
  rat <- log10(Cl(x1) / Cl(x2))  # close 价格比值的对数
  colnames(rat) <- 'Price.Ratio'
  rat
}
Price.Ratio <- PairRatio(c(symb1[1],symb2[1]))
MaRatio <- function(x){
  N <- 20
  Mavg <- rollapply(x, N , mean) # 20日均线
  colnames(Mavg) <- 'Price.Ratio.MA'
  Mavg
}
Price.Ratio.MA <- MaRatio(Price.Ratio)
Sd <- function(x){
  N <- 20
  Stand.dev <- rollapply(x, N, sd) #20日标准差
  colnames(Stand.dev) <- "Price.Ratio.SD"
  Stand.dev
}
Price.Ratio.SD <- Sd(Price.Ratio)
ZScore <- function(x){
  a1 <- x$Price.Ratio  
  b1 <- x$Price.Ratio.MA
  c1 <- x$Price.Ratio.SD
  z <- (a1-b1)/c1  # 20日为一个周期标准化 (R-mu)/sd
  colnames(z)<- 'Z.Score'
  z
}
# b) Augmented Dickey Fuller
ft2<-function(x){
  adf.test(x)$p.value # adf检验 p值
}
Pval <- function(x){
  N.ADF <- 60
  Augmented.df <- rollapply(x, width = N.ADF, ft2) # 60日为周期，做ADF检验
  colnames(Augmented.df) <- "P.Value"
  Augmented.df
}
P.Value <- Pval(Price.Ratio)
add.indicator(strategy = qs.strategy, name = "ZScore", arguments =
                list(x=merge(Price.Ratio,Price.Ratio.MA,Price.Ratio.SD)))
add.indicator(strategy = qs.strategy, name = "Pval", arguments =
                list(x=quote(Price.Ratio)))


Z.Score <- ZScore(x=merge(Price.Ratio,Price.Ratio.MA,Price.Ratio.SD))
plot(main = "Z-Score Time Series", xlab = "Date" , ylab = "Z-Score",Z.Score, type = "l" )
abline(h = 2, col = 2, lwd = 3 ,lty = 2) 
abline(h = -2, col = 3, lwd = 3 ,lty = 2)

alpha = 1 # We set it to 0.1 if we want a 10% significance level. If we want to set the ADF test (second condition)
#off, we just change it to "1", in that case the p-value will always be lower than the significance level and the # and the strategy will not require the pair to be cointegrated.

# alpha 是考虑到 引入ADF检验的一个值，如果ADF检验通不过，即使有信号也不参与交易。
# alphg=1 的时候， 则ADF检验不考虑。

# Z-Score entry and exit thresholds:

buyThresh = -2 # 图中红线和绿线分别是买入和卖出信号。
sellThresh = -buyThresh
exitlong = 1
exitshort = 1

# Before running our backtest, we have to add the signals, position limits and rules of our strategy:

# z-score>2， 触发多头策略  
add.signal(qs.strategy, name="sigThreshold",arguments=list(column="Z.Score", threshold=buyThresh,
                                                             relationship="lt", cross=FALSE),label="longEntryZ")
# P-value 影响是否进入
add.signal(qs.strategy, name="sigThreshold",arguments=list(column="P.Value", threshold= alpha,
                                                           relationship="lt", cross=FALSE),label="PEntry")
# 上述两个信号逻辑并来决定最终是否为进信号
add.signal(qs.strategy, name="sigAND",
           arguments=list(columns=c("longEntryZ", "PEntry"), cross=FALSE),
           label="longEntry")

# z-score < 1，触发多头退出信号。
add.signal(qs.strategy, name="sigThreshold",arguments=list(column="Z.Score", threshold= exitlong,
                                                           relationship="gt", cross=FALSE),label="longExit")

# 下面类似定义空头进出，进信号总是严于出信号

add.signal(qs.strategy, name="sigThreshold",arguments=list(column="Z.Score", threshold=sellThresh,
                                                           relationship="gt", cross=FALSE),label="shortEntryZ")

add.signal(qs.strategy, name="sigAND", arguments=list(columns=c("shortEntryZ", "PEntry"), cross=FALSE),
           label="shortEntry")

add.signal(qs.strategy, name="sigThreshold",arguments=list(column="Z.Score", threshold= exitshort,
                                                           relationship="lt", cross=FALSE),label="shortExit")

# 仓位限制
addPosLimit( portfolio = qs.strategy, # add position limit rules
             symbol = 'spread',
             timestamp = initDate,
             maxpos = 3000,
             longlevels = 1,
             minpos = -3000)

# 两种进信号和相应的出信号
add.rule(qs.strategy, name='ruleSignal',arguments = list(sigcol="longEntry",
                                                         sigval=TRUE, orderqty=3000,  osFUN = osMaxPos, replace = FALSE, ordertype='market',
                                                         orderside='long', prefer = "open"), type='enter' )

add.rule(qs.strategy, name='ruleSignal', arguments = list(sigcol="shortEntry",
                                                          sigval=TRUE, orderqty=-3000,  osFUN = osMaxPos, replace = FALSE,ordertype='market',
                                                          orderside='short', prefer = "open"), type='enter')

add.rule(qs.strategy, name='ruleSignal', arguments = list(sigcol="longExit",
                                                          sigval=TRUE, orderqty= 'all', ordertype='market', orderside='short', prefer = "open"), type='exit')

add.rule(qs.strategy, name='ruleSignal', arguments = list(sigcol="shortExit",
                                                          sigval=TRUE, orderqty= 'all' , ordertype='market', orderside='long', prefer = "open"), type='exit')

summary(get.strategy(qs.strategy))

applyStrategy(strategy = qs.strategy, portfolios = qs.strategy, mktdata = spread)

tns <-getTxns(Portfolio=qs.strategy, Symbol= symbols)

#Update portfolio, account, equity
updatePortf(qs.strategy)

updateAcct(qs.strategy)

updateEndEq(qs.strategy)

# 由于只有 OHLC 数据，交易频率肯定是日间的，估计是以收盘价为准
# 但是我从数据当中看到了分钟？

chart.P2 = function (Portfolio, Symbol, Dates = NULL, ..., TA = NULL)
{
  pname <- Portfolio
  Portfolio <- getPortfolio(pname)
  if (missing(Symbol))
    Symbol <- ls(Portfolio$symbols)[[1]]
  else Symbol <- Symbol[1]
  Prices = get(Symbol)
  if (!is.OHLC(Prices)) {
    if (hasArg(prefer))
      prefer = eval(match.call(expand.dots = TRUE)$prefer)
    else prefer = NULL
    Prices = getPrice(Prices, prefer = prefer)
  }
  freq = periodicity(Prices)
  switch(freq$scale, seconds = {
    mult = 1
  }, minute = {
    mult = 60
  }, hourly = {
    mult = 3600
  }, daily = {
    mult = 86400
  }, {
    mult = 86400
  })
  if (!isTRUE(freq$frequency * mult == round(freq$frequency,
                                             0) * mult)) {
    n = round((freq$frequency/mult), 0) * mult
  }
  else {
    n = mult
  }
  tzero = xts(0, order.by = index(Prices[1, ]))
  if (is.null(Dates))
    Dates <- paste(first(index(Prices)), last(index(Prices)),
                   sep = "::")
  Portfolio$symbols[[Symbol]]$txn <- Portfolio$symbols[[Symbol]]$txn[Dates]
  Portfolio$symbols[[Symbol]]$posPL <- Portfolio$symbols[[Symbol]]$posPL[Dates]
  Trades = Portfolio$symbols[[Symbol]]$txn$Txn.Qty
  Buys = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades >
                                                           0)]
  Sells = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades <
                                                            0)]
  Position = Portfolio$symbols[[Symbol]]$txn$Pos.Qty
  if (nrow(Position) < 1)
    stop("no transactions/positions to chart")
  if (as.POSIXct(first(index(Prices))) < as.POSIXct(first(index(Position))))
    Position <- rbind(xts(0, order.by = first(index(Prices) -
                                                1)), Position)
  Positionfill = na.locf(merge(Position, index(Prices)))
  CumPL = cumsum(Portfolio$symbols[[Symbol]]$posPL$Net.Trading.PL)
  if (length(CumPL) > 1)
    CumPL = na.omit(na.locf(merge(CumPL, index(Prices))))
  else CumPL = NULL
  if (!is.null(CumPL)) {
    CumMax <- cummax(CumPL)
    Drawdown <- -(CumMax - CumPL)
    Drawdown <- rbind(xts(-max(CumPL), order.by = first(index(Drawdown) -
                                                          1)), Drawdown)
  }
  else {
    Drawdown <- NULL
  }
  if (!is.null(Dates))
    Prices = Prices[Dates]
  chart_Series(Prices, name = Symbol, TA = TA, ...)
  if (!is.null(nrow(Buys)) && nrow(Buys) >= 1)
    (add_TA(Buys, pch = 2, type = "p", col = "green", on = 1))
  if (!is.null(nrow(Sells)) && nrow(Sells) >= 1)
    (add_TA(Sells, pch = 6, type = "p", col = "red", on = 1))
  if (nrow(Position) >= 1) {
    (add_TA(Positionfill, type = "h", col = "blue", lwd = 2))
    (add_TA(Position, type = "p", col = "orange", lwd = 2,
            on = 2))
  }
  if (!is.null(CumPL))
    (add_TA(CumPL, col = "darkgreen", lwd = 2))
  if (!is.null(Drawdown))
    (add_TA(Drawdown, col = "darkred", lwd = 2, yaxis = c(0,
                                                          -max(CumMax))))
  plot(current.chob())
}
chart.P2(qs.strategy, "spread", prefer = "close")