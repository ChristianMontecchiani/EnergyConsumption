library(readr)
library(lubridate)
library(dplyr)
#install.packages("tidyr")              # Install & load tidyr
library("tidyr")
library(ggplot2)
library(nnet)

# Store the pre-retrofit data set in a variable and start analyzing it
file_path_preretrofit = "./Desktop/ProgettoR/data/BaselinePeriod.csv"
BaselinePeriod <- read.csv(file_path_preretrofit)

# Data pre processing:
# 1) Change date format
# 2) Factorize the day of the week 
# 3) Eliminate rows that corresponds to Sunday
BaselinePeriod$date <- as.POSIXct(BaselinePeriod$date, format="%Y-%m-%d") 
BaselinePeriod$DayOfTheWeek <- as.factor(BaselinePeriod$DayOfTheWeek)
BaselinePeriod <- BaselinePeriod[BaselinePeriod$Energy != 0, ]
View(BaselinePeriod)

# Build linear model without normalization 
energy_cons_reg <-  lm(Energy  ~ Text, data= BaselinePeriod)
summary(energy_cons_reg)

# We can normalize all data that are numeric 
min_max_scale <- function(data) {
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      data[col] <- (data[[col]] - min(data[[col]])) / (max(data[[col]]) - min(data[[col]]))
    }
  }
  return(data)
}
# Build linear model with normalization 
BaselinePeriodNorm <- min_max_scale(BaselinePeriod)
energy_cons_reg <-  lm(Energy  ~ Text, data= BaselinePeriodNorm)
summary(energy_cons_reg)

# As you can see there is a linear negative relationship between external temperature and energy consumption
set.seed(100)
nn <- nnet(Energy~., data=BaselinePeriod, size=4, linout=TRUE, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=500, )
# Explain variables 
# linout  -> TRUE for linear output units. FALSE for logistic output units.
# skip    -> TRUE to add skip-layer connections from input to output.
# MaxNWts -> The maximum allowable number of weights. There is no intrinsic limit in the code, but increasing MaxNWts will probably allow fits that are very slow and time-consuming.
# trace   -> FALSE no tracing optimization. Default TRUE.
# maxit   -> Max iteration number.
summary(nn)

# Neural NetWork with normalization
set.seed(100)
nn <- nnet(Energy~., data=BaselinePeriodNorm, size=4, linout=TRUE, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=500, )
# Explain variables 
# linout  -> TRUE for linear output units. FALSE for logistic output units.
# skip    -> TRUE to add skip-layer connections from input to output.
# MaxNWts -> The maximum allowable number of weights. There is no intrinsic limit in the code, but increasing MaxNWts will probably allow fits that are very slow and time-consuming.
# trace   -> FALSE no tracing optimization. Default TRUE.
# maxit   -> Max iteration number.
summary(nn)
