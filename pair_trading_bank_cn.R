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

min(sh601288$date)
min(sh601398$date)
min(sh601939$date)
min(sh601988$date)
# 农业银行上市时间最短


############################################################################################################
# Z-score and ADF-test
# 模拟时间段
# choose 2 in 4: 6种
############################################################################################################

stock_a = sh601288
stock_b = sh601398

start_date = "2011-01-01"
end_date = "2013-12-31"


stock_a <- stock_a[stock_a$date<=end_date & stock_a$date>=start_date, ]
stock_b <- stock_b[stock_b$date<=end_date & stock_b$date>=start_date, ]
all(stock_a$date==stock_b$date)

stock_a[35:40, ]
stock_b[35:40, ]

