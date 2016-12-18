setwd("/home/zhuzhi/r_idea/")

x <- rnorm(500, mean = 10, sd = 1)
y <- rnorm(500, mean = 10, sd = 1)
z <- rnorm(500, mean = 30, sd = 3)
# z1 <- rnorm(500, mean = 30, sd = 9)
# z2 <- rnorm(500, mean = 30, sd = 9)

cor(x,y);cor(x,z);cor(y,z)
cor(x/z,y/z)


n <- 10000
res <- numeric(n)
for(i in 1:n){
  cat(i, fill=TRUE)
  x <- rnorm(500, mean = 10, sd = 1)
  y <- rnorm(500, mean = 10, sd = 1)
  z <- rnorm(500, mean = 30, sd = 9)
  
  res[i] <- cor(x/z, y/z)
  
}

hist(res)


0.3^2/(0.1^2+0.3^2)



rnorm(10, mean=10, sd=1)
rnorm(10, mean=20, sd=2)



