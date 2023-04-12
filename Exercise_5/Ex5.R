library(readr)
library(lubridate)
library(dplyr)
library("tidyr")
library(ggplot2)
library(nnet)
library(Metrics)

# Store the pre-retrofit data set in a variable and start analyzing it
file_path_preretrofit = "./Desktop/ProgettoR/data/BaselinePeriod.csv"
file_path_preretrofit = "./Desktop/ProgettoR/data/OpaqueEnvelopeRetrofit.csv"

OpaqueEnvelopeRetrofit <- read.csv(file_path_preretrofit)
BaselinePeriod <- read.csv(file_path_preretrofit)

# Data pre processing:
# 1) Change date format
# 2) Factorize the day of the week 
# 3) Eliminate rows that corresponds to Sunday
BaselinePeriod$date <- as.POSIXct(BaselinePeriod$date, format="%Y-%m-%d")
OpaqueEnvelopeRetrofit$date <- as.POSIXct(OpaqueEnvelopeRetrofit$date, format="%Y-%m-%d") 

BaselinePeriod$DayOfTheWeek <- as.factor(BaselinePeriod$DayOfTheWeek)
OpaqueEnvelopeRetrofit$DayOfTheWeek <- as.factor(OpaqueEnvelopeRetrofit$DayOfTheWeek)

BaselinePeriod <- BaselinePeriod[BaselinePeriod$Energy != 0, ]
OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$Energy == 0 & OpaqueEnvelopeRetrofit$DayOfTheWeek != 1] <- NA 

X <- OpaqueEnvelopeRetrofit$Iext
Y <- OpaqueEnvelopeRetrofit$Energy

y_approx <- approx(x = X, y = Y, xout = which(is.na(OpaqueEnvelopeRetrofit$Energy)))$y
OpaqueEnvelopeRetrofit$Energy  <- ifelse(is.na(OpaqueEnvelopeRetrofit$Energy), y_approx, OpaqueEnvelopeRetrofit$Energy)
OpaqueEnvelopeRetrofit <- OpaqueEnvelopeRetrofit[OpaqueEnvelopeRetrofit$Energy != 0, ]

# Visualize data
View(OpaqueEnvelopeRetrofit)
View(BaselinePeriod)

# Build linear model without normalization 
lm_preretrofit <-  lm(Energy  ~ Text, data= BaselinePeriod)
new <- data.frame(Text = OpaqueEnvelopeRetrofit$Text)
y_pred <- predict.lm(lm_preretrofit, new)
y_true <- OpaqueEnvelopeRetrofit$Energy
text <- OpaqueEnvelopeRetrofit$Text


# Line plot for the consumption of the energy over the days 
plot(text, y_pred, type="o", xlab="External Temp (°C)", pch=23, ylab="Energy", main="Energy consumption (LM)", col="red")
points(text, y_true, col= "blue", pch=2,  legend="Ground Truth")
segments(text, y_true, text, y_pred, lty="dotted")
legend("topright", legend=c(" Prediction", "True"),
       col=c("red", "blue"), pch = c(23,2), cex=0.8)


# As you can see there is a linear negative relationship between external temperature and energy consumption
set.seed(100)
nn_preretrofit <- nnet(Energy~., data=BaselinePeriod, size=4, linout=TRUE, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=500, )
y_pred <- predict(nn_preretrofit, OpaqueEnvelopeRetrofit)
y_true <- OpaqueEnvelopeRetrofit$Energy
text <- OpaqueEnvelopeRetrofit$Text


# Line plot for the consumption of the energy over the days 
plot(text, y_pred, type="o", xlab="External Temp (°C)", pch=23, ylab="Energy", main="Energy consumption", col="red")
points(text, y_true, col= "blue", pch=2,  legend="Ground Truth")
segments(text, y_true, text, y_pred, lty="dotted")
legend("topright", legend=c(" Prediction", "True"),
       col=c("red", "blue"), pch = c(23,2), cex=0.8)