# AR(1) without constant

n_sim <- 10000
res_NB <- numeric(n_sim)
res_DF <- numeric(n_sim)

for(i in 1:n_sim){
  n <- 10000
  X_n <- rnorm(n)
  S_n <- cumsum(X_n)
  
  WdW <- sum(S_n[1:(n-1)]*X_n[2:n])/n
  W2dt <- sum(S_n^2)/n^2
  
  res_NB[i] <- WdW/W2dt
  res_DF[i] <- WdW/sqrt(W2dt)
  
}

library(MASS)

truehist(res_DF, col="darkblue", h=0.2, border="white", xlim=c(-4,4), main="Simulated Dickey-Fuller Distribution")
quantile(res_DF, probs=c(0.01,0.05,0.1))

truehist(res_NB, col="darkblue", h=0.2, border="white", xlim=c(-30,10), main="Simulated Normalized Bias Distribution")
quantile(res_NB, probs=c(0.01,0.05,0.1))


# install.packages("fUnitRoots")
library("fUnitRoots")
qadf(c(0.01,0.05,0.1), N=10000, trend="nc", statistic="t")
qadf(c(0.01,0.05,0.1), N=10000, trend="nc", statistic="n")

qunitroot(c(0.01,0.05,0.1), N=10000, trend="nc", statistic="t")
qunitroot(c(0.01,0.05,0.1), N=10000, trend="nc", statistic="n")

#

res_test <- unitrootTest(S_n, lags=0, type="nc")

slot(res_test, "test")

psi_hat <- sum(S_n[1:(n-1)]*X_n[2:n])/sum(S_n[1:(n-1)]^2)
psi_resid <- X_n[2:n]-S_n[1:(n-1)]*psi_hat
psi_hat_sd <- sqrt(sum(psi_resid^2)/(n-2)/(sum(S_n[1:(n-1)]^2)))# n-1 minus only one freedom degree

psi_hat/psi_hat_sd


res_tmp <- lm(X_n[2:n]~S_n[1:(n-1)]-1)
res_tmp_tmp <- summary(res_tmp)
res_tmp_tmp$coefficients

punitroot(-2.4275, N=10000, trend = "nc", statistic = "t")

punitroot(-2.4275, N=10000, trend = "nc", statistic = "n")


# AR(1) with constant but no time trend

n_sim <- 10000
res_NB <- numeric(n_sim)
res_DF <- numeric(n_sim)

for(i in 1:n_sim){
  n <- 10000
  X_n <- rnorm(n)
  S_n <- cumsum(X_n)/sqrt(n)
  
  Wdr <- sum(S_n)/n
  WudW <- sum((S_n[1:(n-1)]-Wdr)*X_n[2:n] )/sqrt(n)
  Wu2dt <- sum((S_n-Wdr)^2)/n
  
  res_NB[i] <- WudW/Wu2dt
  res_DF[i] <- WudW/sqrt(Wu2dt)
  
}

library(MASS)

truehist(res_DF, col="darkblue", h=0.2, border="white", xlim=c(-4,4), main="Simulated Dickey-Fuller Distribution")
quantile(res_DF, probs=c(0.01,0.05,0.1))

truehist(res_NB, col="darkblue", h=0.2, border="white", xlim=c(-30,10), main="Simulated Normalized Bias Distribution")
quantile(res_NB, probs=c(0.01,0.05,0.1))

qunitroot(c(0.01,0.05,0.1), N=10000, trend="c", statistic="t")
qunitroot(c(0.01,0.05,0.1), N=10000, trend="c", statistic="n")

# test calculate
# constant = 0.1
# psi = 1

X_n <- rnorm(10000)
S_n <- 0.1*(0:(n-1)) + cumsum(X_n) + rnorm(1)
plot(S_n, type = "l")

res_test <- unitrootTest(S_n, lags=0, type="c")
slot(res_test, "test")

lm_c <- lm(diff(S_n)~S_n[0:(n-1)])
res_lm <- summary(lm_c)


slot(res_test, "test")$statistic
res_lm$coefficients

slot(res_test, "test")$p.value
punitroot(q=slot(res_test, "test")$statistic, N=10000, trend = "c", statistic = "t")
punitroot(q=slot(res_test, "test")$statistic, N=10000, trend = "c", statistic = "n")


# constant and time trend

X_n <- rnorm(10000)
S_n <- 0.1*(0:(n-1)) + 0.1*(1:n) + cumsum(X_n) + rnorm(1)

res_test <- unitrootTest(S_n, lags=0, type="ct")
slot(res_test, "test")$statistic

lm_ct <- lm(diff(S_n)~ trend + S_n[1:(n-1)] )
res_lm <- summary(lm_ct)

res_lm$coefficients
slot(res_test, "test")$statistic

slot(res_test, "test")$p.value
punitroot(q=slot(res_test, "test")$statistic, N=10000, trend = "ct", statistic = "t")
punitroot(q=slot(res_test, "test")$statistic, N=10000, trend = "ct", statistic = "n")


# sim
n_sim <- 10000
res_NB <- numeric(n_sim)
res_DF <- numeric(n_sim)

for(i in 1:n_sim){
  n <- 10000
  X_n <- rnorm(n)
  S_n <- cumsum(X_n)/sqrt(n)
  
  Wdr <- sum(S_n)/n
  rWdr <- sum( ((1:n)/n) *S_n )/n
  a <- 4*Wdr - 6*rWdr
  d <- -6*Wdr + 12*rWdr
    
  S_tau_n <- S_n - a - d*(1:n)/n
  
  W_tau_dW <- sum((S_tau_n[2:n]-S_tau_n[1:(n-1)])*(S_n[2:n]-S_n[1:(n-1)]))
  W_tau_2dr <- sum((S_tau_n)^2)/n
  
  res_NB[i] <- W_tau_dW/W_tau_2dr
  res_DF[i] <- W_tau_dW/sqrt(W_tau_2dr)
  
}

library(MASS)

truehist(res_DF, col="darkblue", h=0.2, border="white", xlim=c(-4,4), main="Simulated Dickey-Fuller Distribution")
quantile(res_DF, probs=c(0.01,0.05,0.1))

truehist(res_NB, col="darkblue", h=0.2, border="white", xlim=c(-30,10), main="Simulated Normalized Bias Distribution")
quantile(res_NB, probs=c(0.01,0.05,0.1))

qunitroot(c(0.01,0.05,0.1), N=10000, trend="ct", statistic="t")
qunitroot(c(0.01,0.05,0.1), N=10000, trend="ct", statistic="n")
