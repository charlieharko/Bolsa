#####################################################################
#       Script from Joy Garcia Harjanto    23.08.2018               #
#    & coded and improved from Carlos Renero Lecuna 14.04.2020      #
#                                                                   #
#       For the analysis of Stock Exchange Market taking data       #
#                     from the internet                             #
#          Original article from: https://bit.ly/2V49BK1            #
#####################################################################

# Remove all variables

rm(list=ls())

# Setting the working directory where the R script is

setwd("~/R/La bolsa")

# Needed packages from the source in the title

library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)

                            ###################################
                            # Initialazing variables for sims #
                            ###################################

start_date <- "2008-08-01"    # Start taking date from here
finish_date <- "2018-12-31"   # Taking date until ... Sys.Date() (today)

N <- 1000                     # Number of simulations used for the predictions
time_scale <- 252*4           # 252 trade days * 4 years

                            ###################################
                            # Getting data from Amazon stocks #
                            ###################################

# Log returns of Amazon's stock beginning August 1st, 2008 to August 17th, 2018

getSymbols("AMZN", from = start_date, to = finish_date)
AMZN_log_returns<-AMZN%>%Ad()%>%dailyReturn(type='log')

# The log returns doesn't make sense at this stage but it is actually the fundamental
# of my analysis. I will explain later.

#################################################################################################
#####             EXPLANATION OF TECHNICAL ANALYSIS  -  PLOT & SCRIPTS                      #####
#################################################################################################

# The first chart series graph is straightforward as it shows Amazon's price chart
AMZN%>%Ad()%>%chartSeries(name = "AMAZON")
readline(prompt="Press [enter] to continue")

# The second chart series show the Bollinger Band chart, % Bollinger change, 
# Volume Traded and Moving Average Convergence Diverence in 2018 alone
AMZN%>%chartSeries(TA='addBBands();addVo();addMACD()',subset='2018', name = "AMAZON")
readline(prompt="Press [enter] to continue")

#################################################################################################
#####                   EXPLANATION OF TECHNICAL ANALYSIS  -  COMENTS                       #####
#################################################################################################

# The moving average is important to understanding Amazon(AMZN)'s technical charts. 
# It smoothes out daily price fluctuations by averaging stock prices and is effective in 
# identifying potential trends.

# The Bollinger Band chart plots two standard deviations away from the moving average and is used
# to measure the stock's volatiliy. The Volume chart shows how its stocks are traded on the daily.
# The Moving Average Convergence Divergence gives technical analysts buy/sell signals. The rule of
# thumb is: If it falls below the line, it is time to sell. If it rises above the line, it is
# experiencing an upward momentum.
# 
# The charts above are usually used to decide whether to buy/sell a stock. Since I am not a
# certified financial analyst, I decided to do additional research to convince myself.

#################################################################################################
#####                          PREDICTIONS  -  PLOT & SCRIPTS                               #####
#################################################################################################

# Comparison: Compare our stock (AMAZON) with others in the market (same sector, TECH):
# GOOGLE (GOOGL), FACEBOOK (FB), TESLA (TSLA) & APPLE (AAPL)

getSymbols("FB", from = start_date, to = finish_date)
FB_log_returns<-FB%>%Ad()%>%dailyReturn(type='log')

getSymbols("GOOGL", from = start_date, to = finish_date)
GOOGL_log_returns<-GOOGL%>%Ad()%>%dailyReturn(type='log')

getSymbols("AAPL", from = start_date, to = finish_date)
AAPL_log_returns<-AAPL%>%Ad()%>%dailyReturn(type='log')

getSymbols("TSLA", from = start_date, to = finish_date)
TSLA_log_returns<-TSLA%>%Ad()%>%dailyReturn(type='log')

# Risk/Return rate of each stock: defined as -> mean of log return and standard deviation 
# of log return (mean = Return, SD = risk)

return <- c(mean(AMZN_log_returns), mean(FB_log_returns), mean(GOOGL_log_returns), mean(AAPL_log_returns), mean(TSLA_log_returns))
risk <- c(c(sd(AMZN_log_returns), sd(FB_log_returns), sd(GOOGL_log_returns), sd(AAPL_log_returns), sd(TSLA_log_returns)))
flags <- c("AMAZON", "FACEBOOK", "GOOGLE", "AAPLE", "TESLA")

RR_data <- data.frame(flags,return,risk)
names(RR_data) <- c("Companies", "Return ($)", "Risk ($)")

# Plotting the data using plotly

fig_RRplot <- plot_ly(data = RR_data, x = ~return, y = ~risk, color = ~flags,
               type = 'scatter',
               mode = 'markers',
               marker = list(size = 12))

fig_RRplot <- fig_RRplot %>% layout(title = 'Risk vs. Reward',
                      yaxis = list(title = 'Risk ($)', 
                                   zeroline = TRUE, 
                                   showline = TRUE, 
                                   mirror = 'ticks'),
                      xaxis = list(title = 'Reward ($)', 
                                   zeroline = TRUE, 
                                   showline = TRUE,
                                   mirror = 'ticks'))

print(fig_RRplot)
readline(prompt="Press [enter] to continue")

#   It is plotted the daily Risk vs. Reward, we have to change the log_XX to obtaine different
#   rewards timing, if we plann to invest long temr, change it to yearly instead daily.
#
# If you want to use a manual range to plot, use the modifier range = to do it in each axis
# xaxis = list(range = c(2, 5)
# yaxis = list(range = c(2, 5)
#
# To plot the graph you just have to type the name of the plot "fig_RRplot" in this case
#
# When purchasing stocks you should try to purchase stocks that share a small correlation
# because you want to maximize the total rate of return. Correlation analysis follows:

data<-cbind(diff(log(Cl(AMZN))),diff(log(Cl(GOOGL))),diff(log(Cl(AAPL))),diff(log(Cl(FB))),diff(log(Cl(TSLA))))

chart.Correlation(data)
readline(prompt="Press [enter] to continue")

# The smallest the correlation value, the less has to do one company results with the other

                            ###################################
                            #        Price Prediction         #
                            ###################################
# 
# I went on to predict the prices for Amazon (AMZN)'s stock. I achieved this by 
# the random walk theory and monte carlo method.
# The random walk theory is suited for a stock's price prediction because it is 
# rooted in the believe that past performance is not an indicator of future results 
# and price fluctuations can not be predicted with accuracy.
# I simulated the prices Amazon (AMZN)'s stock for 252*4 trading days 
# (Since a year has ~252 trading days). That is 4 years worth of trading!
# I generated the prices using the data I have earlier from log returns 
# and used exponential growth rate to predict how much the stock will grow per day.
# The growth rate is randomly generated and dependent on the input values of mu and sigma.

mu <- mean(AMZN_log_returns)         # mean of log returns
sig <- sd(AMZN_log_returns)          # sd of log returns 

price<-rep(NA,time_scale)
price[1] <- as.numeric(AMZN$AMZN.Adjusted[length(AMZN$AMZN.Adjusted),]) # Initialize value

# Start simulating prices (only 1)

for(i in 2:time_scale){
  price[i] <- price[i-1]*exp(rnorm(1,mu,sig))
}

random_data <- cbind(price,1:(time_scale))
colnames(random_data) <-c ("Price","Day")
random_data <- as.data.frame(random_data)
random_plot <- random_data %>% ggplot(aes(Day,Price)) + geom_line() + labs(title="Amazon (AMZN) price simulation for 4 years") + theme_bw()
print(random_plot)

readline(prompt="Press [enter] to continue")

# Start N simulations

mc_matrix<-matrix(nrow=time_scale,ncol=N)
mc_matrix[1,1]<-as.numeric(AMZN$AMZN.Adjusted[length(AMZN$AMZN.Adjusted),])

for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-as.numeric(AMZN$AMZN.Adjusted[length(AMZN$AMZN.Adjusted),])
  for(i in 2:nrow(mc_matrix)){
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}

name <- str_c("Sim ",seq(1,N))
name <- c("Day",name)
final_mat <- cbind(1:(time_scale),mc_matrix)
final_mat <- as.tibble(final_mat)
colnames(final_mat) <- name
dim(final_mat) #1008 N+1

gg_final <- final_mat%>%gather("Simulation","Price",2:N+1)%>%ggplot(aes(x=Day,y=Price,Group=Simulation))+geom_line(alpha=0.2)+labs(title="Amazon Stock (AMZN): 500 Monte Carlo Simulations for 4 Years")+theme_bw()
print(gg_final) # Plot the graph

readline(prompt="Press [enter] to continue")

# Results - summary

probs = c(0.005, 0.025, 0.25, 0.50, 0.75, 0.975, 0.995)
resume <- final_mat[N,-1]%>%as.numeric()%>%quantile(probs=probs)
print(resume) # Print the resume data