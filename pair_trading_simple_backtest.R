############################################################################################################
#         简单 pair trading 的回测
############################################################################################################
# 注意到的问题：
#   1. 涨跌停板不可以购买。
#   2. 停牌是不可以购买。
#   3. 简单的复权处理（复权因子）
# 没有注意到的问题：
#   1. 复权具体事项：
#     1.1. 送红股派息时候的税率处理
#     1.2. 股票的持仓时间分化处理（涉及到股息的税率处理）
#     1.3. 配股时候的金钱处理
#     1.4. balance和position具体变动更加细化
#     1.5. 待添加
#   2. close购买的滑点问题
#   3. 其他待添加
############################################################################################################
setwd("/home/zhuzhi/r_idea/")
source("./stock_tools.R")

############################################################################################################
#         第一步：读入数据并预处理
############################################################################################################
#   以两只股票为例子，写一个粗糙的代码
#   sh601288 农业银行
#   sh601398 工商银行
############################################################################################################

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

# 预处理
sh601398 <- pre_close_add(sh601398)
sh601398 <- ohlc_limit_classify(sh601398)

sh601288 <- pre_close_add(sh601288)
sh601288 <- ohlc_limit_classify(sh601288)

sh601939 <- pre_close_add(sh601939)
sh601939 <- ohlc_limit_classify(sh601939)

sh601988 <- pre_close_add(sh601988)
sh601988 <- ohlc_limit_classify(sh601988)

############################################################################################################
#         第二步：设置参数
############################################################################################################
# stock_main: stock_main := sh601288
# stock_sub: stock_sub := sh601398
#
# 价差定义: price_ratio := log(stock_main) - log(stock_sub)
# 仓位定义: position := (position_of_stock_main - position_of_stock_sub)/2
# 多头建仓信号: price_ratio >= 2 & position = 0
# 多头平仓信号: price_ratio <= 1 & position > 0
# 空头建仓信号: price_ratio <= -2 & position = 0
# 空头平仓信号: price_ratio >= -1 & position < 0
# 辅助购买可行性指标: ohlc_limit_type
#
# 交易起止日期: start_date, end_date
#
# 滑动窗宽: n_window
############################################################################################################

stock_main = sh601398
stock_sub = sh601988

short_open_price_ratio = 2
short_close_price_ratio = 1
long_open_price_ratio = -2
long_close_price_ratio = -1

start_date = "2011-01-01"
end_date = "2013-12-31"

n_window = 20
############################################################################################################
#         第三步： 数据合并，生成一些需要的因子
############################################################################################################
# 需要加入交易日日历
# 合并成为一个大的 dataframe
# 输出 main 和 sub 的停牌日期
# 
# 交易可行性因子之一：
# flag_isTrade: 0 or 1
#
# 价格比相关因子：
# price_ratio = log(stock_main_close/stock_sub_close)
# pr_ma: 价格比滑动平均
# pr_msd: 价格比滑动标准差
#
# 修正价格比相关因子：
# 
############################################################################################################
trade_cal = read.table('./data/trade_cal', sep=",", stringsAsFactors = FALSE, header=TRUE)

stock_main <- stock_main[stock_main$date<=end_date & stock_main$date>=start_date, ]
stock_sub <- stock_sub[stock_sub$date<=end_date & stock_sub$date>=start_date, ]
trade_cal_sub <- trade_cal[trade_cal$calendarDate<=end_date & trade_cal$calendarDate>=start_date, ]

stock_data <- merge(x=stock_main, y=stock_sub, by.x="date", by.y="date", all.x=TRUE, all.y=TRUE, suffixes = c("_main", "_sub"))
stock_data <- merge(x=stock_data, y =trade_cal_sub, by.x="date", by.y ="calendarDate", all.x=TRUE, all.y=TRUE)
stock_data <- stock_data[stock_data$isOpen==1,]


# # 打印停牌日期
# date_suspension_main <- stock_data[is.na(stock_data$close_main), "date"]
# date_suspension_sub <- stock_data[is.na(stock_data$close_sub), "date"]
# 
# cat("main suspension date: ", paste(date_suspension_main, collapse=","), fill = TRUE)
# cat("sub suspension date: ", paste(date_suspension_sub, collapse=","), fill = TRUE)

# 加入是否可交易标志
stock_data$flag_isTrade = as.integer(!(is.na(stock_data$close_main)|is.na(stock_data$close_sub)))

# factor filling
stock_data$factor_main <- na_filling(stock_data$factor_main)
stock_data$factor_sub <- na_filling(stock_data$factor_sub)
  
# close filling
stock_data$close_main <- na_filling(stock_data$close_main)
stock_data$close_sub <- na_filling(stock_data$close_sub)

# price_ratio and price ratio z-score
stock_data$price_ratio <- log(stock_data$close_main/stock_data$close_sub)
stock_data$pr_ma <- NA
stock_data$pr_msd <- NA

for(i in n_window:nrow(stock_data)){
  
  stock_data[i, "pr_ma"] <- mean(stock_data[(i-n_window+1):i, "price_ratio"])
  stock_data[i, "pr_msd"] <- sd(stock_data[(i-n_window+1):i, "price_ratio"])
}

stock_data$pr_z_score <- (stock_data$price_ratio - stock_data$pr_ma)/stock_data$pr_msd

# 修正价格比
stock_data$mod_price_ratio <- log(stock_data$close_main/stock_data$close_sub)
stock_data$mpr_ma <- NA
stock_data$mpr_msd <- NA

for(i in n_window:nrow(stock_data)){
  
  fuquan_price_main <- stock_data[i, "close_main"]*stock_data[i, "factor_main"]/stock_data[i-n_window+1, "factor_main"]
  fuquan_price_sub <- stock_data[i, "close_sub"]*stock_data[i, "factor_sub"]/stock_data[i-n_window+1, "factor_sub"]
  stock_data[i, "mod_price_ratio"] <- log(fuquan_price_main/fuquan_price_sub)
  stock_data[i, "mpr_ma"] <- mean(stock_data[(i-n_window+1):i, "mod_price_ratio"])
  stock_data[i, "mpr_msd"] <- sd(stock_data[(i-n_window+1):i, "mod_price_ratio"])
  
}

stock_data$mpr_z_score <- (stock_data$mod_price_ratio - stock_data$mpr_ma)/stock_data$mpr_msd


############################################################################################################
#         第四步： 粗略快速回测，检测是否可有利润
############################################################################################################
# 不考虑以下因素：
#   - flag_isTrade 以外的成交可能性
#   - 印花税
#   - 佣金
#   - 股息红利等税
# 回测有错误：
#   没有考虑到复权因素，甚至我怀疑目前的利润都是由复权贡献的 -- 2016-08-24 by zhuzhi
############################################################################################################

position = 0
cost = 0
profit = 0
cum_profit = 0
share_per_trade = 1000
stock_data$position = 0
stock_data$cum_profit = 0


for(i in (n_window):nrow(stock_data)){
  #print(i)
  if(position==0 & stock_data[i, "flag_isTrade"]==1 & stock_data[i, "mpr_z_score"]>=2){
    # short position open
    position <- share_per_trade
    cost <- (stock_data[i, "close_sub"]-stock_data[i, "close_main"])*share_per_trade
    stock_data[i, "position"] <- position
    stock_data[i, "cum_profit"] <- cum_profit + 0
  }else if(position>0 & stock_data[i, "flag_isTrade"]==1 & stock_data[i, "mpr_z_score"]<=1){
    # short position close
    position <- 0
    profit <- (stock_data[i, "close_sub"]-stock_data[i, "close_main"])*share_per_trade - cost
    stock_data[i, "position"] <- position
    stock_data[i, "cum_profit"] <- cum_profit + profit
    cum_profit <- cum_profit + profit
    cost <- 0
    
  }else if(position==0 & stock_data[i, "flag_isTrade"]==1 & stock_data[i, "mpr_z_score"]<=-2){
    # long position open
    position <- -share_per_trade
    cost <- (stock_data[i, "close_main"]-stock_data[i, "close_sub"])*share_per_trade
    stock_data[i, "position"] <- position
    stock_data[i, "cum_profit"] <- cum_profit + 0
  }else if(position<0 & stock_data[i, "flag_isTrade"]==1 & stock_data[i, "mpr_z_score"]>=-1){
    # long position closed
    position <- 0
    profit <- (stock_data[i, "close_main"]-stock_data[i, "close_sub"])*share_per_trade - cost
    stock_data[i, "position"] <- position
    stock_data[i, "cum_profit"] <- cum_profit + profit
    cum_profit <- cum_profit + profit
    cost <- 0
    
  }else if(position>0){
    # no trade
    # short in
    profit <- (stock_data[i, "close_sub"]-stock_data[i, "close_main"])*share_per_trade - cost
    stock_data[i, "position"] <- position
    stock_data[i, "cum_profit"] <- cum_profit + profit
  }else if(position<0){
    # no trade
    # short in
    profit <- (stock_data[i, "close_main"]-stock_data[i, "close_sub"])*share_per_trade - cost
    stock_data[i, "position"] <- position
    stock_data[i, "cum_profit"] <- cum_profit + profit
  }else{
    # no trade
    # no position
    stock_data[i, "position"] <- position
    stock_data[i, "cum_profit"] <- cum_profit
  }
  
}


plot(stock_data$cum_profit, type="l")

############################################################################################################
#         第四步： 稍微详细回测，检测具体的交易细节
############################################################################################################
# 考虑以下因素：
#   - 印花税
#   - 佣金
# 不考虑以下因素：
#   - 股息红利等税
############################################################################################################

source("./books_gen.R")

# 初始化四表
book_list <- books_gen()
t_rzhzk <- book_list$T_RZHZK
t_rcj <- book_list$T_RCJ
t_rcc <- book_list$T_RCC
t_rwt <- book_list$T_RWT

# 重定义参数

## 数据参数
stock_main = sh601398
stock_sub = sh601988
stock_code_main = "601398"
stock_code_sub = "601988"

## 信号参数
short_open_price_ratio = 2
short_close_price_ratio = 1
long_open_price_ratio = -2
long_close_price_ratio = -1

## 时间参数
start_date = "2011-01-01"
end_date = "2013-12-31"

## 其他参数
n_window = 20

## 交易相关参数
### 1. 初始现金
cash = 50000
### 2. 最大仓位,百分比
max_pos = 0.5
### 3. 相关费率
yj_ratio = 0.0005
yhs_ratio = 0.001

## 底仓建立
init_date = stock_data[1, "date"]
init_pos <- round(cash*max_pos/(stock_data[1, "close_main"] + stock_data[1, "close_sub"]), -3)
### 1. t_rwt
{
  # main buy
  {
    k_rwt <- nrow(t_rwt)
    t_rwt[k_rwt+1, "RQ"] <- init_date
    t_rwt[k_rwt+1, "ZQDM"] <- stock_code_main
    t_rwt[k_rwt+1, "WTJG"] <- stock_data[1, "close_main"]
    t_rwt[k_rwt+1, "WTSL"] <- init_pos
    t_rwt[k_rwt+1, "WTJE"] <- init_pos*stock_data[1, "close_main"]
    t_rwt[k_rwt+1, "MMFX"] <- "B"
    t_rwt[k_rwt+1, "CJBZ"] <- "deal"
  }
  # sub buy
  {
    k_rwt <- nrow(t_rwt)
    t_rwt[k_rwt+1, "RQ"] <- init_date
    t_rwt[k_rwt+1, "ZQDM"] <- stock_code_sub
    t_rwt[k_rwt+1, "WTJG"] <- stock_data[1, "close_sub"]
    t_rwt[k_rwt+1, "WTSL"] <- init_pos
    t_rwt[k_rwt+1, "WTJE"] <- init_pos*stock_data[1, "close_sub"]
    t_rwt[k_rwt+1, "MMFX"] <- "B"
    t_rwt[k_rwt+1, "CJBZ"] <- "deal"
  }
}
### 2. t_rcc
{
  # main buy
  {
    k_rcj <- nrow(t_rcj)
    t_rcj[k_rcj+1, "RQ"] <- init_date
    t_rcj[k_rcj+1, "ZQDM"] <- stock_code_main
    t_rcj[k_rcj+1, "CJJG"] <- stock_data[1, "close_main"]
    t_rcj[k_rcc+1, "CJSL"] <- init_pos
    t_rcj[k_rcc+1, "CJJE"] <- init_pos*t_rcj[k_rcj+1, "CJJG"]
    t_rcj[k_rcc+1, "YJ"] <- t_rcj[k_rcc+1, "CJJE"]*yj_ratio
  }
  
}


for(i in (n_window):nrow(stock_data)){
  #print(i)
  if(position==0 & stock_data[i, "flag_isTrade"]==1 & stock_data[i, "mpr_z_score"]>=2){
    # short position open
    position <- share_per_trade
    cost <- (stock_data[i, "close_sub"]-stock_data[i, "close_main"])*share_per_trade
    stock_data[i, "position"] <- position
    stock_data[i, "cum_profit"] <- cum_profit + 0
  }else if(position>0 & stock_data[i, "flag_isTrade"]==1 & stock_data[i, "mpr_z_score"]<=1){
    # short position close
    position <- 0
    profit <- (stock_data[i, "close_sub"]-stock_data[i, "close_main"])*share_per_trade - cost
    stock_data[i, "position"] <- position
    stock_data[i, "cum_profit"] <- cum_profit + profit
    cum_profit <- cum_profit + profit
    cost <- 0
    
  }else if(position==0 & stock_data[i, "flag_isTrade"]==1 & stock_data[i, "mpr_z_score"]<=-2){
    # long position open
    position <- -share_per_trade
    cost <- (stock_data[i, "close_main"]-stock_data[i, "close_sub"])*share_per_trade
    stock_data[i, "position"] <- position
    stock_data[i, "cum_profit"] <- cum_profit + 0
  }else if(position<0 & stock_data[i, "flag_isTrade"]==1 & stock_data[i, "mpr_z_score"]>=-1){
    # long position closed
    position <- 0
    profit <- (stock_data[i, "close_main"]-stock_data[i, "close_sub"])*share_per_trade - cost
    stock_data[i, "position"] <- position
    stock_data[i, "cum_profit"] <- cum_profit + profit
    cum_profit <- cum_profit + profit
    cost <- 0
    
  }else if(position>0){
    # no trade
    # short in
    profit <- (stock_data[i, "close_sub"]-stock_data[i, "close_main"])*share_per_trade - cost
    stock_data[i, "position"] <- position
    stock_data[i, "cum_profit"] <- cum_profit + profit
  }else if(position<0){
    # no trade
    # short in
    profit <- (stock_data[i, "close_main"]-stock_data[i, "close_sub"])*share_per_trade - cost
    stock_data[i, "position"] <- position
    stock_data[i, "cum_profit"] <- cum_profit + profit
  }else{
    # no trade
    # no position
    stock_data[i, "position"] <- position
    stock_data[i, "cum_profit"] <- cum_profit
  }
  
}


