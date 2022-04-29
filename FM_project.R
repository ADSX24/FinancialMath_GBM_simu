rm(list = ls())

#loading the needed packages
library(quantmod)
library(tseries)
library(PerformanceAnalytics)
library(randomcoloR)
library(timeDate)
library(MASS)

#Setting up the active directory
setwd("~/Unif/MASTER 1 IG/Financial mathematics & stochastic calculus/Project")

#Importing the data via Yahoo Finance
PG <- read.csv(file = 'PG.csv', header = TRUE, sep=",")

#Inspecting the first lines of the data set and the full chart
head(x = PG, n = 6)

#Isolating the close price
price.close <- as.data.frame(PG[,c('Date','Adj.Close')])
colnames(price.close) <- c("Date","Adj.Close")

price.close$Date <- as.Date(price.close$Date)

#plotting the stock price
plot(
  x = price.close$Date,
  y = price.close$Adj.Close,
  main = "Daily closing stock price of Procter & Gamble",
  ylab = "Closing stock price",
  xlab = "Date",
  type = "l"
)

#Computing the log-returns
log_price <- log(x = price.close$Adj.Close)
log_returns <- diff(x = log_price, lag=1)

#drop the first line since the log-return is not available
log_returns <- na.omit(log_returns)
#https://www.r-bloggers.com/2020/05/basic-statistical-concepts-for-finance/

#other ways to compute the log-returns
#returns2 <- diff(x = log_price.close, lag=1)
#returns3 <- ROC(price.close[,1])

fitnorm <- fitdistr(log_returns, densfun = "normal")
hist(log_returns,nclass = 30,probability = TRUE)

x<-seq(from=min(log_returns),to=max(log_returns),length.out = 1000)
lines(x=x, y=dnorm(x,fitnorm$estimate["mean"],fitnorm$estimate["sd"]), col="red")

qqnorm(log_returns,pch=1,frame=FALSE, main='Normal Q-Q plot of log returns')
qqline(log_returns, col="steelblue", lwd=2)

#parameters of the GBM
#expected rate of return
mu <- mean(log_returns)

#volatility
sigma <- sd(log_returns)

#Simulating paths with GBM
dt <- .001
n <- 100
s_zero <- as.numeric(price.close[dim(price.close)[1],dim(price.close)[2]])


w <- matrix(data = 0, nrow = 31, ncol = 15)
s <- matrix(data = NA, nrow = 30, ncol = 15)
random <- rep(x = NA, times=30)
my_col_names <- rep(x = NA, times=15)


for(i in 1:ncol(w)){
  random <- rnorm(n=30, mean = 0, sd = sqrt(1))
  my_col_names[i] <- paste("Path",i,sep="")
  for(j in 1:(nrow(w)-1)){
    w[j+1,i] <- w[j,i] + random[j]
    s[j,i] <- s_zero*exp((mu-((sigma^2)/2))*j+sigma*w[j,i])
  }
  colnames(s) <- my_col_names
  colnames(w) <- my_col_names
}
simu_dates <- seq(from = as.Date("2022-01-01"), to = as.Date("2022-01-30"), by = "day")

full_dates <- c(price.close$Date,simu_dates)
full_price <- c(price.close$Adj.Close,s[,1])

colors <- distinctColorPalette(k=14)

#plot(x = full_dates, y=full_price,type='l', ylim =c(115,190))
#for(i in 2:ncol(s)){
  #lines(x=simu_dates, s[,i], col=colors[i])
#}



plot(x=simu_dates, y=s[,1], 
     ylim=c(min(s)-5,max(s)+5), 
     type = 'l',
     xlab = "Date",
     ylab = "Stock price",
     main = "15 simulated paths for the future stock prices of Procter & Gamble over one month")

for(i in 2:ncol(s)){
  lines(x=simu_dates, s[,i], col=colors[i])
}

#accounting for weekdays
# https://stackoverflow.com/questions/26441700/how-to-determine-if-date-is-a-weekend-or-not-not-using-lubridate 

#expected price after one month
exp_price_1month <- s_zero * exp(mu*30) #because mu is the mean daily returns

abline(h = exp_price_1month, col='blue')
legend(x=1,y=max(s)-3,legend=c("Expected price in 1 month"), col=c("blue"),lwd=5)

#confidence interval
#parameters of the normal distribution
t=1
mean <- log(s_zero)+(mu*30-((sqrt(30)*sigma)^2)/2)*t

variance <- (sqrt(30)*sigma^2) * t

sd <- sqrt(variance)

p <- 0.9

quantile <- qnorm(p, mean = 0, sd = 1)

lower_bound <- exp(mean-quantile*sd)

upper_bound <- exp(mean+quantile*sd)

print(paste("There is a probability of",p*100,"% that the stock price will lie between",
            lower_bound,"and",upper_bound, "in one month"))

abline(h = lower_bound, col="red",lty=2)
abline(h = upper_bound, col="red",lty=2)
legend(x="topleft",legend=c("Expected price in 1 month","Confidence interval"), col=c("blue","red"), lty= c(1,2), lwd(5,1))









