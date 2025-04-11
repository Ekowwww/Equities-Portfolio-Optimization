install.packages("quantmod")
install.packages("tidyverse")
install.packages("forecast")
install.packages("PerformanceAnalytics")
install.packages("PortfolioAnalytics")
install.packages("factoextra")
install.packages('dplr')
install.packages('tidyr')
install.packages("ggplot2")
install.packages("cluster")
install.packages('ROI.plugin.glpk')
install.packages("ROI.plugin.quadprog")
install.packages("ROI.plugin.symphony")

library(quantmod)
library(tidyverse)
library(forecast)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(factoextra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cluster)

######################################################################################
###            NOTES:

###   PortfolioPrices = big time series
###   stockdata = dataframe of close prices with dates removed
###   stocks = stock symbols with their respective average returns and variance
###   clusterstocks = standardized version of returns and variance for each stock 

######################################################################################



###################  PICKING STOCKS   ###############################


# General electric, IBM, Walmart, simon property group, morgan stanley, disney, exxon, chevron, JPMorgan, Johnson and Johnson,T-mobile, Verizon 
tickers = c("GE", "IBM", "WMT", "SPG", "MS", "DIS", "XOM", "CVX", "JPM", "JNJ","TMUS", "VZ")

# Initialize to null first
PortfolioPrices = NULL

# get the close price of selected stocks from yahoo. 2009-06-01 to 2019-12-30
for (ticker in tickers) {
  PortfolioPrices = cbind(PortfolioPrices,
                          getSymbols.yahoo(ticker, from='2009-06-01', to= '2019-12-30', periodicity='daily',auto.assign=FALSE)[,4])
  
}

################  PREP FOR CLUSTERING      ###################################

## Make sure there is no missing data.
colSums(is.na(PortfolioPrices))

# change data from xts to dataframe and do some cleaning
stockdata = as.data.frame(PortfolioPrices)
stockdata$dates = rownames(stockdata)
rownames(stockdata)= NULL
stockdata <- stockdata[, -ncol(stockdata)]

#calculate annual mean returns and variances
daily_returns <- diff(log(as.matrix(stockdata)), lag = 1) * 100         # percent change of stocks
annual_mean_returns <- colMeans(daily_returns, na.rm = TRUE) * 252         # percent change average returns by year
annual_return_variance <- apply(daily_returns, 2, var, na.rm = TRUE) * 252   # percent change average variance by year

symbols <- colnames(stockdata)   # put stock symbols in a variable to make life easier

# create new dataframe 'stocks' that has the close price, annual returns, annual variance
stocks <- data.frame(symbols = symbols, returns = annual_mean_returns, variance = annual_return_variance)

# remove the rownames 
rownames(stocks)=NULL

# standardize the values for clustering, name the standardized version "clusterstocks"
clusterstocks = stocks %>% select(-symbols) %>% scale()


################### CLUSTERING    #########################################

#create clusters
kmeans(clusterstocks, centers = 2, iter.max = 10, nstart = 5)

# find best number of clusters for data. Using silhouette method
fviz_nbclust(clusterstocks, kmeans, method = "silhouette")
# Optimal number of clusters is 4

# create cluster plot 
fviz_cluster(kmeans(clusterstocks, centers=4,iter.max = 10, nstart=5),data= clusterstocks)

# write the cluster assignments to the dataframe (labeling purposes)
clusters= kmeans(clusterstocks, centers=4,iter.max = 10, nstart=5)
stocks = stocks |> mutate(clusters$cluster)


#### Alternatively #######

# using k values from 1 to 10. adjust if number of stocks change
#wss <- sapply(1:10, function(k) {
#  kmeans(clusterstocks, centers=k, nstart=20, iter.max=10)$tot.withinss
#})

# plot the within-cluster sum of squares against k
#ggplot() + 
#  geom_line(aes(x=1:10, y=wss), color="blue") + 
#  geom_point(aes(x=1:10, y=wss), color="red", size=3) +
#  scale_x_continuous(breaks=1:10) +
#  xlab("Number of clusters (k)") + 
#  ylab("Within-cluster sum of squares (wss)") + 
#  ggtitle("Elbow method")




####################################################################################################

### Use PortfolioPrices to compute weights and performance metrics   #####################

#12 weights for each stock
# General electric, IBM, Walmart, simon property group, morgan stanley, disney, exxon, chevron, JPMorgan, Johnson and Johnson,T-mobile, Verizon 
weights = c(0.039, 0.097, 0.112, 0.120, 0.136, 0.150, 0.157, 0.163, 0.183, 0.213, 0.236, 0.445)

# Get rate of change in price for portfolio for each day
PortfolioReturns = na.omit(ROC(PortfolioPrices))


# Use S&P500 as benchmark to see how well our portfolio has performed
Benchmark = getSymbols.yahoo("^GSPC", from='2009-06-01', to= '2019-12-30', periodicity='daily',auto.assign=FALSE)[,4]

#Get rate of change for S&P500 returns for each day
BenchmarkReturns = na.omit(ROC(Benchmark))


# Total portfolio returns with weights
Total_Portfolio_Returns = Return.portfolio(PortfolioReturns)


# Calculate the portfolio Beta (How risky it is compared to market) risk-free rate -0.4
beta = CAPM.beta(Total_Portfolio_Returns, BenchmarkReturns, 0.40/252)
# market has beta=1 so we ere slightly less risky

# Calculate portfolio Alpha (How well it beat the market)
alpha = CAPM.jensenAlpha(Total_Portfolio_Returns, BenchmarkReturns, 0.40/252)
# market has alpha=0 so we did terrible

# Calculate Sharpe Ratio (return relative to added risk)
sharpe_ratio =SharpeRatio(Total_Portfolio_Returns, 0.40/252)
# -0.145 which is very bad, ie. we are taking losses on our added risks

#Efficient Frontier Chart Analyzing all stocks except JNJ, JPM, TMUS and VZ
# General electric, IBM, Walmart, simon property group, morgan stanley, disney, exxon, chevron
tickers = c("GE", "IBM", "WMT", "SPG", "MS", "DIS", "XOM", "CVX")

# Initialize to null first
PortfolioPrices = NULL

# get the close price of selected stocks from yahoo. 2009-06-01 to 2019-12-30
for (ticker in tickers) {
  PortfolioPrices = cbind(PortfolioPrices,
                          getSymbols.yahoo(ticker, from='2009-06-01', to= '2019-12-30', periodicity='daily',auto.assign=FALSE)[,4])
  
}

### Portfolio Optimization using Efficient Frontier and ROI#####################
#use only close prices or adjust prices for portfolio
portfolioReturns <- na.omit(ROC(Cl(PortfolioPrices)))
portf <- portfolio.spec(colnames(portfolioReturns))

portf <- add.constraint(portf, type = "weight_sum", min_sum=1, max_sum=1)
portf <- add.constraint(portf, type = "box", min=.10, max=.40)   # min 10% & max 40% in each asset 
portf <- add.objective(portf, type = "return", name = "mean")
portf <- add.objective(portf, type = "risk", name = "StdDev")

# make sure to add `trace = TRUE` argument to below code otherwise it will give error in efficient Frontier
optPort <- optimize.portfolio(portfolioReturns, portf, optimize_method = "ROI",
                              trace = TRUE)
optPort

chart.Weights(optPort)


ef <- extractEfficientFrontier(optPort, match.col = "StdDev", n.portfolios = 10, risk_aversion = NULL)

chart.EfficientFrontier(ef, match.col = "StdDev", n.portfolios = 10, xlim = NULL, ylim = NULL, cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)

#Efficient Frontier Chart Analyzing all stocks except JNJ, JPM, TMUS and VZ
# General electric, IBM, Walmart, simon property group, morgan stanley, disney, exxon, chevron
tickers = c("WMT", "SPG", "DIS", "JNJ", "JPM")

# Initialize to null first
PortfolioPrices = NULL

# get the close price of selected stocks from yahoo. 2009-06-01 to 2019-12-30
for (ticker in tickers) {
  PortfolioPrices = cbind(PortfolioPrices,
                          getSymbols.yahoo(ticker, from='2009-06-01', to= '2019-12-30', periodicity='daily',auto.assign=FALSE)[,4])
  
}

### Taking only the stocks that are closest to the ideal line#####################
#This will be the final portfolio to maximize returns. 
#use only close prices or adjust prices for portfolio
portfolioReturns <- na.omit(ROC(Cl(PortfolioPrices)))
portf <- portfolio.spec(colnames(portfolioReturns))

portf <- add.constraint(portf, type = "weight_sum", min_sum=1, max_sum=1)
portf <- add.constraint(portf, type = "box", min=.10, max=.40)   # min 10% & max 40% in each asset 
portf <- add.objective(portf, type = "return", name = "mean")
portf <- add.objective(portf, type = "risk", name = "StdDev")

# make sure to add `trace = TRUE` argument to below code otherwise it will give error in efficient Frontier
optPort <- optimize.portfolio(portfolioReturns, portf, optimize_method = "ROI",
                              trace = TRUE)
optPort

chart.Weights(optPort)


ef <- extractEfficientFrontier(optPort, match.col = "StdDev", n.portfolios = 10, risk_aversion = NULL)

chart.EfficientFrontier(ef, match.col = "StdDev", n.portfolios = 10, xlim = NULL, ylim = NULL, cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)





