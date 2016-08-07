############################################################################################################
# 数据来源: 新浪网
#
# 获取方式: 自编爬虫
#
# tushare 的数据有时间限制
# yahoo 的数据太粗糙
# 
# fuquan 数据为　带复权因子的数据
# market 数据为　市场日　OHLC　数据
############################################################################################################

setwd("~/r_idea/")

############################################################################################################
# 读入数据及简单合并
############################################################################################################
read_stock_fuquan <- function(stock_file){
  colClasses <- c("character", "numeric", "numeric", "numeric", 
                  "numeric", "numeric", "numeric", "numeric")
  stock_data <- read.table(stock_file, header=TRUE, sep=",", colClasses=colClasses)
  return(stock_data)
}

read_stock <- function(stock_file){
  colClasses <- c("character", "numeric", "numeric", "numeric", 
                  "numeric", "numeric", "numeric")
  stock_data <- read.table(stock_file, header=TRUE, sep=",", colClasses=colClasses)
  return(stock_data)
}

# double check
# merge two data into one
merge_stock <- function(stock_data, stock_data_fuquan){
  
  if(nrow(stock_data_fuquan) == nrow(stock_data) & stock_data[1, "date"] == stock_data_fuquan[1, "date"] &
     stock_data[nrow(stock_data), "date"] == stock_data_fuquan[nrow(stock_data_fuquan), "date"]){
    stock_data$factor <- stock_data_fuquan$factor
  }else{
    stop("data problem, market data != fuquan market data.")
  }
  
  return(stock_data)
}

# sh601288
# 农业银行
sh601288_fuquan <- read_stock_fuquan("./data/fuquan/601288")
sh601288_market <- read_stock("./data/market/601288")
sh601288 <- merge_stock(sh601288_market, sh601288_fuquan)
rm(sh601288_market,sh601288_fuquan)

# sh601398
# 工商银行
sh601398_fuquan <- read_stock_fuquan("./data/fuquan/601398")
sh601398_market <- read_stock("./data/market/601398")
sh601398 <- merge_stock(sh601398_market, sh601398_fuquan)
rm(sh601398_market, sh601398_fuquan)

# sh601939
# 建设银行
sh601939_fuquan <- read_stock_fuquan("./data/fuquan/601939")
sh601939_market <- read_stock("./data/market/601939")
sh601939 <- merge_stock(sh601939_market, sh601939_fuquan)
rm(sh601939_market, sh601939_fuquan)

# sh601988
# 中国银行
sh601988_fuquan <- read_stock_fuquan("./data/fuquan/601988")
sh601988_market <- read_stock("./data/market/601988")
sh601988 <- merge_stock(sh601988_market, sh601988_fuquan)
rm(sh601988_market, sh601988_fuquan)

############################################################################################################
# 时间确定
# 共同时间 = 模式时间段 + 测试时间段
# 开始时间: 2011-01-01
# 结束时间: 2016-07-31
# 时长: 5年7个月
# 模拟时间段: 2011-01-01 -> 2013-12-31 3年
# 测试时间段: 2014-01-01 -> 2016-07-31 2年7个月
# 时间不同步处理方法:
#   1. 获得上证综指的交易日期。
#   2. 所有股票日期补全，加一列显示是否交易。
#   3. 非交易日 OHL 为NA, close,factor 由前一交易日后移， VA为0.
############################################################################################################

# 农业银行上市时间最短
# 这里调用了 tushare 中的通联接口来获得交易日历

# library("rPython")
# python.exec("import tushare")
# python.exec("tushare.set_token('fa8d82ec55d910894965982e026b47469a99b4395df2d4ef90c8c0ddd04a298d')")
# python.exec("trade_cal = tushare.Master().TradeCal(exchangeCD='XSHG', beginDate='20110101', endDate='20160731', field='calendarDate,isOpen,prevTradeDate')")
# python.exec("trade_cal.to_csv('./data/trade_cal', index=False)")

stock_list <- c("sh601288", "sh601398", "sh601939", "sh601988")
for(stock_code in stock_list){
  
  stock_data <- get(stock_code)
  # stock_data <- stock_data[stock_data$date <= end_date & stock_data$date >= start_date,]
  cat(nrow(stock_data), min(stock_data$date), max(stock_data$date), fill=TRUE)
  
}

############################################################################################################
# Z-score and ADF-test
# 模拟时间段
# choose 2 in 4: 6种
# 很多时候停牌原因时间太久，没有好的方法查不了了。
############################################################################################################

start_date = "2011-01-01"
end_date = "2013-12-31"

n_window = 20
n_adf_win = 60

stock_a = sh601288
stock_b = sh601398



date_a <- stock_a$date
date_b <- stock_b$date


setdiff(date_a, date_b)
setdiff(date_b, date_a)


trade_cal = read.table('./data/trade_cal', sep=",", stringsAsFactors = FALSE, header=TRUE)


stock_a <- stock_a[stock_a$date<=end_date & stock_a$date>=start_date, ]
stock_b <- stock_b[stock_b$date<=end_date & stock_b$date>=start_date, ]
trade_cal_sub <- trade_cal[trade_cal$calendarDate<=end_date & trade_cal$calendarDate>=start_date, ]

# stock_data in trading days
stock_data <- merge(x=stock_a, y=stock_b, by.x="date", by.y="date", all.x=TRUE, all.y=TRUE, suffixes = c("_a", "_b"))
stock_data <- merge(x=stock_data, y =trade_cal_sub, by.x="date", by.y ="calendarDate", all.x=TRUE, all.y=TRUE)
stock_data <- stock_data[stock_data$isOpen==1,]

n <- nrow(stock_data)

# close and factor filling
na_filling  <- function(x){
  for(i in 2:length(x)){
    if(is.na(x[i])){
      x[i]<- x[i-1]
      }
  }
  return(x)
}
stock_data$close_a <- na_filling(stock_data$close_a)
stock_data$factor_a <- na_filling(stock_data$factor_a)
stock_data$close_b <- na_filling(stock_data$close_b)
stock_data$factor_b <- na_filling(stock_data$factor_b)


# price ratio and moving average/sd
stock_data$price_ratio <- log(stock_data$close_a/stock_data$close_b)
stock_data$pr_ma <- NA
stock_data$pr_msd <- NA

for(i in n_window:n){
  
  stock_data[i, "pr_ma"] <- mean(stock_data[(i-n_window+1):i, "price_ratio"])
  stock_data[i, "pr_msd"] <- sd(stock_data[(i-n_window+1):i, "price_ratio"])
}

# price rtio's z-score

stock_data$pr_z_score <- (stock_data$price_ratio - stock_data$pr_ma)/stock_data$pr_msd

plot(as.Date(stock_data$date),stock_data$pr_z_score, type='l', main="primitive price ratio z-score", xlab="date", ylab="z-score")
abline(a=2, b=0, col="red", lty=1)
abline(a=1, b=0, col="red", lty=2)

abline(a=-2, b=0, col="skyblue", lty=1)
abline(a=-1, b=0, col="skyblue", lty=2)

# modified price ratio
# 因为要考虑到分红派息的问题
# 股票分红派息配股的时候，股价会相应下降，这个时候会对“价差”影响较大
# 不如使用复权价格来看看 price ratio
# 复权方法:
#   定点前复权
#   在交易日 k， 以交易日 k-20 为定锚进行复权。


stock_data$mod_price_ratio <- log(stock_data$close_a/stock_data$close_b)
stock_data$mpr_ma <- NA
stock_data$mpr_sd <- NA

for(i in n_window:n){
  
  fuquan_price_a <- stock_data[i, "close_a"]*stock_data[i, "factor_a"]/stock_data[i-n_window+1, "factor_a"]
  fuquan_price_b <- stock_data[i, "close_b"]*stock_data[i, "factor_b"]/stock_data[i-n_window+1, "factor_b"]
  stock_data[i, "mod_price_ratio"] <- log(fuquan_price_a/fuquan_price_b)
  stock_data[i, "mpr_ma"] <- mean(stock_data[(i-n_window+1):i, "mod_price_ratio"])
  stock_data[i, "mpr_msd"] <- sd(stock_data[(i-n_window+1):i, "mod_price_ratio"])
  
}

stock_data$mpr_z_score <- (stock_data$mod_price_ratio - stock_data$mpr_ma)/stock_data$mpr_msd

plot(as.Date(stock_data$date),stock_data$mpr_z_score, type='l', main="modified price ratio z-score", xlab="date", ylab="z-score")
abline(a=2, b=0, col="red", lty=1)
abline(a=1, b=0, col="red", lty=2)

abline(a=-2, b=0, col="skyblue", lty=1)
abline(a=-1, b=0, col="skyblue", lty=2)

plot(stock_data$pr_z_score, type="l", col="darkred", xlab="", ylab="", main="pr v.s. mpr")
lines(stock_data$mpr_z_score, type="l", col="darkblue")

abline(a=2, b=0, col="gray60", lty=1)
abline(a=1, b=0, col="gray60", lty=2)

abline(a=-2, b=0, col="gray60", lty=1)
abline(a=-1, b=0, col="gray60", lty=2)

# 显然在分红派息配股之后，进出信号短时间内会有所改变







