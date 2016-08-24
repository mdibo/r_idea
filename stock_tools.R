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

############################################################################################################
# 给股票加一个昨日收盘价格和调整昨日收盘价
# 调整昨日收盘价是将昨日收盘价复权到今天的情形时候的理论收盘价（以今日定点向前复权）
############################################################################################################
pre_close_add <- function(stock_data){
  
  stock_data$pre_close <- c(NA, stock_data[1:(nrow(stock_data)-1), "close"])
  
  stock_data$adj_pre_close <- NA
  
  for(i in 2:nrow(stock_data)){
      stock_data[i, "adj_pre_close"] <- round(stock_data[i-1, "close"]*stock_data[i-1, "factor"]/stock_data[i, "factor"], 2)
  }
  
  stock_data$up <- round(stock_data$adj_pre_close * 1.1, 2)
  stock_data$down <- round(stock_data$adj_pre_close * 0.9, 2)
  
  return(stock_data)
}
############################################################################################################
# 针对调整昨日收盘价增加一个今日涨幅比例
# 仅应用于添加过前日收盘价之后的情形
############################################################################################################


############################################################################################################
# OHLC 分类
# 标示当天股票的涨跌情形
# 仅用于 pre_close_add 之后
############################################################################################################
ohlc_limit_type_table_ <- data.frame(
  type = c("DDDD", "DNDD", "DUDD", "NNDD", "NUDD", 
           "DNDN", "DUDN", "NNDN", "NUDN", "UUDD", 
           "UUDN", "NNNN", "DUDU", "NUDU", "NUNN", 
           "UUNN", "NUNU", "UUNU", "UUDU", "UUUU"), 
  type_code = c(933, 333, 533, 303, 503,
                330, 530, 300, 500, 523,
                520, 0, 532, 502, 200,
                220, 202, 222, 522, 422),
  stringsAsFactors=FALSE, row.names = "type")

ohlc_limit_classify <- function(stock_data, tolerance=1e-6, ohlc_limit_type_table=ohlc_limit_type_table_){
  
  stock_data$ohlc_limit_type <- NA
  
  check_udn <- function(x, u, d, tolerance){
    if(abs(x-u)<tolerance){
      return("U")
    }else if(abs(x-d)<tolerance){
      return("D")
    }else{
      return("N")
    }
  }
  
  for(i in 2:nrow(stock_data)){
    
    type <- paste(
      check_udn(stock_data[i, "open"], stock_data[i, "up"], stock_data[i, "down"], tolerance),
      check_udn(stock_data[i, "high"], stock_data[i, "up"], stock_data[i, "down"], tolerance),
      check_udn(stock_data[i, "low"], stock_data[i, "up"], stock_data[i, "down"], tolerance),
      check_udn(stock_data[i, "close"], stock_data[i, "up"], stock_data[i, "down"], tolerance),
      sep=""
    )
    
    stock_data[i, "ohlc_limit_type"] <- ohlc_limit_type_table[type, "type_code"]
    
  }
  
  return(stock_data)
  
}

############################################################################################################
# na filling
############################################################################################################

na_filling  <- function(x){
  for(i in 2:length(x)){
    if(is.na(x[i])){
      x[i]<- x[i-1]
    }
  }
  return(x)
}
