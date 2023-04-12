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
OpaqueEnvelopeRetrofit <- OpaqueEnvelopeRetrofit[OpaqueEnvelopeRetrofit$Energy != 0]

# Visualize data
View(OpaqueEnvelopeRetrofit)
View(BaselinePeriod)

# Build linear model without normalization 
lm_preretrofit <-  lm(Energy  ~ Text, data= BaselinePeriod)
predict.lm(lm_preretrofit, OpaqueEnvelopeRetrofit$Energy)

# Compute RMSE 
RMSE <- rmse(actual = BaselinePeriod$Energy, predicted = energy_cons_reg$fitted.values)
# Compute CVRMSE
CVRMSE <- (1/mean(BaselinePeriod$Energy)) * RMSE
# Compute MAPE 
MAPE <- mape(actual = BaselinePeriod$Energy, predicted = energy_cons_reg$fitted.values)

# Check the norm of the residuals
residuals <- energy_cons_reg$coefficients
hist(residuals)
shapiro.test(residuals)
plot(residuals, pch = 16, col = "red")

# Plot the model 
plot(energy_cons_reg)


# We can normalize all data that are numeric 
min_max_scale <- function(data) {
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      data[col] <- (data[[col]] - min(data[[col]])) / (max(data[[col]]) - min(data[[col]]))
    }
  }
  return(data)
}

evaluate_model <- function(data){
  lin_model <- lm(Energy  ~ Text, data=data)
  # Compute RMSE 
  RMSE <- rmse(actual = data$Energy, predicted = lin_model$fitted.values)
  print(paste("RMSE:", RMSE))
  # Compute CVRMSE
  CVRMSE <- (1/mean(data$Energy)) * RMSE
  print(paste("CVRMSE:", CVRMSE))
  # Compute MAPE 
  MAPE <- mape(actual = data$Energy, predicted = lin_model$fitted.values)
  print(paste("MAPE:", MAPE))
  
  
  return(lin_model)
}
# Build linear model with normalization 
BaselinePeriodNorm <- min_max_scale(BaselinePeriod)
norm_lm <- evaluate_model(BaselinePeriodNorm)
lm <- evaluate_model(BaselinePeriod)

# Check the norm of the residuals
residuals <- norm_lm$coefficients
hist(residuals)
shapiro.test(residuals)
plot(residuals, pch = 16, col = "red")

# Plot the model 
plot(norm_lm)

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
names(nn)
# Compute RMSE 
RMSE <- rmse(actual = BaselinePeriod$Energy, predicted = nn$fitted.values)
print(paste("Neural Network RMSE:", RMSE))
# Compute CVRMSE
CVRMSE <- (1/mean(BaselinePeriod$Energy)) * RMSE
print(paste("Neural Network CVRMSE:", CVRMSE))
# Compute MAPE 
MAPE <- mape(actual = BaselinePeriod$Energy, predicted = nn$fitted.values)
print(paste("Neural Network MAPE:", MAPE))


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
# Compute RMSE 
RMSE <- rmse(actual = BaselinePeriod$Energy, predicted = nn$fitted.values)
print(paste("Neural Network RMSE:", RMSE))
# Compute CVRMSE
CVRMSE <- (1/mean(BaselinePeriod$Energy)) * RMSE
print(paste("Neural Network CVRMSE:", CVRMSE))
# Compute MAPE 
MAPE <- mape(actual = BaselinePeriod$Energy, predicted = nn$fitted.values)
print(paste("Neural Network MAPE:", MAPE))