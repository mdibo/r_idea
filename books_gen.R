############################################################################################################
#         建立回测结果判定的四种books
############################################################################################################
# 具体介绍：
# https://zhuzhi.github.io/2016/08/25/2016-08-25-backtest_books/
############################################################################################################
books_gen <- function(...){
  
  T_RCC <- data.frame(RQ = character(), 
                      ZQDM = character(),
                      CJJG = numeric(),
                      BBJG = numeric(),
                      ZXJG = numeric(),
                      ZXFQJG = numeric(),
                      CCSL = numeric(),
                      CCJE = numeric(), 
                      CCFQJE = numeric(), 
                      CCLR = numeric(), 
                      stringsAsFactors=FALSE)
  
  T_RCJ <- data.frame(RQ = character(), 
                      ZQDM = character(),
                      CJJG = numeric(),
                      CJFQJG = numeric(),
                      CJSL = numeric(),
                      CJJE = numeric(),
                      CJFQJE = numeric(),
                      YJ = numeric(),
                      YHS = character(),
                      MMFX = character(),
                      stringsAsFactors=FALSE)
  
  T_RWT <- data.frame(RQ = character(), 
                      ZQDM = character(),
                      WTJG = numeric(),
                      WTFQJG = numeric(),
                      WTSL = numeric(),
                      WTJE = numeric(), 
                      WTFQJE = numeric(), 
                      MMFX = character(),
                      CJBZ = character(),
                      stringsAsFactors=FALSE)
  
  T_RZHZK <- data.frame(RQ = character(), 
                        ZZC = numeric(),
                        ZSZ = numeric(),
                        XJ = numeric(),
                        stringsAsFactors=FALSE)
  
  return(list(T_RCC=T_RCC, T_RWT=T_RWT, T_RCJ=T_RCJ, T_RZHZK=T_RZHZK))
  
}

