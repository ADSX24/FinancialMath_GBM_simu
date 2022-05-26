rm(list = ls())

#loading the needed packages
library(randomcoloR)
library(MASS)

#Setting up the active directory. Put your path here!
setwd("~/Unif/MASTER 1 IG/Financial mathematics & stochastic calculus/Project")

##### TASK 1 #####

#Importing the data 
PG <- read.csv(file = 'PG.csv', header = TRUE, sep=",")

#Inspecting the first lines of the data set 
head(x = PG, n = 6)

#Isolating the adjusted close price and the dates
prices <- as.data.frame(PG[,c('Date','Adj.Close')])

colnames(prices) <- c("Date","Adj.Close")

prices$Date <- as.Date(prices$Date)

head(x = prices, n = 6)

##### TASK 2 #####

#plotting the adjusted stock prices
plot(
  x = prices$Date,
  y = prices$Adj.Close,
  main = "Daily adjusted closing stock price of Procter & Gamble",
  ylab = "Closing adjusted stock price",
  xlab = "Date",
  type = "l"
)

##### TASK 3 #####

#Computing the log-returns
log_prices <- log(x = prices$Adj.Close)
log_returns <- diff(x = log_prices, lag=1)

#drop the first line since the log-return is not available
log_returns <- na.omit(log_returns)

#plot time series of log-returns
plot(x = prices[2:dim(prices)[1],1], 
     y=log_returns, 
     type = 'l', 
     col = 'blue',
     main = "Log-returns of Procter & Gamble in 2021",
     ylab = "Log-returns",
     xlab = "Dates")

#Histogram of log-returns and study their distribution
x<-seq(from=min(log_returns),to=max(log_returns),length.out = 1000)

par(mfrow=c(1,2))

hist(log_returns,
     nclass = 30,
     probability = TRUE, 
     main='Distribution of log-returns')

legend("topleft",
       legend = c("Normal density"), 
       col=c("red"), 
       lty=1, 
       cex=0.8)

lines(x=x, 
      y=dnorm(x,mean(log_returns),sd(log_returns)),
      col="red")

#Building a QQ plot for log-returns
qqnorm(log_returns,pch=1,frame=FALSE, main='Normal Q-Q plot of log returns')

qqline(log_returns, col="steelblue", lwd=2)

par(mfrow=c(1,1))

##### TASK 4 #####

#Estimating parameters of the GBM
mu <- mean(log_returns)#expected rate of return
sigma <- sd(log_returns)#volatility

##### TASK 5 #####

#Simulating paths
#Creation of a general function to simulate stock prices path during n days
simuGBMpath <- function(n,p,s_zero,drift,volatility){
  
  # p = number of path
  # n = number of days
  # s_zero = numeric value = last stock price value
  
  w <- matrix(data = 0, nrow = n+1, ncol = p) #empty matrix to stock simulated Wj
  s <- matrix(data = NA, nrow = n, ncol = p) #empty matrix to stock simulated prices
  random <- rep(x = NA, times=n) # empty vector to stock random numbers
  my_col_names <- rep(x = NA, times=p) 
  
  
  for(i in 1:ncol(w)){
    random <- rnorm(n=n, mean = 0, sd = sqrt(1))
    my_col_names[i] <- paste("Path",i,sep="")
    for(j in 1:(nrow(w)-1)){
      w[j+1,i] <- w[j,i] + random[j]
      s[j,i] <- s_zero*exp((mu-((sigma^2)/2))*j+sigma*w[j,i])
    }
    colnames(s) <- my_col_names
    colnames(w) <- my_col_names
  }
  
  #Plotting simulated paths
  colors <- distinctColorPalette(k=n-1) #generating n-1 different colors
  
  plot(x=seq(1,n, by=1), y=s[,1], 
       ylim=c(min(s)-5,max(s)+10), 
       type = 'l',
       xlab = "Date",
       ylab = "Stock price",
       main = paste("Simulated paths for the future stock prices of Procter & Gamble over", n, "days"))
  
  for(i in 2:ncol(s)){
    lines(x=seq(1,n, by=1), s[,i], col=colors[i])
  }
}

#calling the function to simulate stock prices over 1 month
s_zero <- as.numeric(prices[dim(prices)[1],dim(prices)[2]])
p <- 15 #number of paths
n <- 252*(1/12) #number of simulation days

simuGBMpath(n = n, p = p, s_zero = s_zero, drift = mu, volatility = sigma)


##### TASK 6 #####

#Definition of a function to calculate the expected stock price after n days
ExpectedPrice <- function(drift, n, s_zero){
   x <- s_zero * exp(drift*n)
   abline(h = x, col='blue') #adding a horizontal line on the previous plot
   return(print(paste("The expected price after", n, "days is", x)))
}

#Expected price after one month
ExpectedPrice(drift = mu, n = n, s_zero = s_zero)


##### TASK 7 #####

#Definition of a function to calculate confidence intervals
CI <- function(drift, volatility, s_zero, n, conflevel){
  
  #Parameters of the normal distribution that the stock prices follow at time t=n
  mean <- log(s_zero)+(mu-((sigma)^2)*0.5)*n
  variance <- (sigma^2)*n
  sd <- sqrt(variance)
  
  #Computing confidence interval
  alpha <- 1-conflevel 
  
  quantile <- qnorm(1-alpha/2, mean = 0, sd = 1) #quantile 1-alpha/2 of the standard normal distribution
  
  lower_bound <- exp(mean-quantile*sd)
  
  upper_bound <- exp(mean+quantile*sd)
  
  print(paste("There is a probability of",conflevel*100,"% that the stock price will lie between",
              lower_bound,"and",upper_bound, "in", n, "days"))
  
  #adding the upper and lower bound on the previous plot
  abline(h = lower_bound, col="red",lty=2)
  abline(h = upper_bound, col="red",lty=2)
  
  legend(x="topleft",legend=c(paste("Expected price in",n,"days"),paste(conflevel*100,"%","Confidence interval")), col=c("blue","red"), lty= c(1,2), lwd(5,1))
}

#Confidence interval at level 0.9 for 21 days of simulation
conf <- 0.90 #confidence level
CI(drift = mu, volatility = sigma, s_zero = s_zero, n = n, conflevel = conf)

##### TASK 8 #####
log_returns2 <- log_returns[125:250]#last six months
mu2 <- mean(log_returns2)
sigma2 <- sd(log_returns2)

simuGBMpath(n = 21, p = p, s_zero = s_zero, drift = mu2, volatility = sigma2)
ExpectedPrice(drift = mu2, n = 21, s_zero = s_zero)
CI(drift = mu2, volatility = sigma2, s_zero = s_zero, n = 21, conflevel = conf)
#clearly we see larger volatility and thus larger confidence interval 


##### TASK 9 #####

#Forecasting prices over the coming 3 months or 63 days
simuGBMpath(n = 63, p = p, s_zero = s_zero, drift = mu, volatility = sigma)
ExpectedPrice(drift = mu, n = 63, s_zero = s_zero)
CI(drift = mu, volatility = sigma, s_zero = s_zero, n = 63, conflevel = conf)