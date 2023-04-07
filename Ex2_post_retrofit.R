library(readr)
library(lubridate)
library(dplyr)
#install.packages("tidyr")              # Install & load tidyr
library("tidyr")
library(ggplot2)

# Store the pre-retrofit data set in a variable and start analyzing it
file_path_preretrofit = "./Desktop/ProgettoR/data/OpaqueEnvelopeRetrofit.csv"
OpaqueEnvelopeRetrofit <- read.csv(file_path_preretrofit)

# Before begin working on the data set, let's have a look
# at the raw data
View(OpaqueEnvelopeRetrofit)

# Let's now understanding the structure of the data set:
# 1.) data type of each attribute
# 2.) number of rows and columns present in the data set
str(OpaqueEnvelopeRetrofit)

# Summary is one of the most important functions that help in summarizing 
# each attribute in the data set. It gives a set of descriptive statistics, 
# depending on the type of variable:
#   1) *Numerical*: gives mean, median, mode, range and quartiles
#   2) *Factor*: gives a table with the frequencies
#   3) *Factor + Numerical*: gives the number of missing values
#   4) *Char*: gives the length and the class
summary(OpaqueEnvelopeRetrofit)

# In this data set there are some missing values so we must replace with a techinique
# Let's start analyzing each column

# The first column is the date. Since it is a date it is possible to convert in datetime,
# which will be useful to plot data
OpaqueEnvelopeRetrofit$date <- as.POSIXct(OpaqueEnvelopeRetrofit$date, format="%Y-%m-%d") 

# It contains all the dates from the 01/11/2017 to 31/03/2018

# The second column is 'DayOfTheWeek', which is an int variable but it represents Monday, Tuesday, ..., Sunday
# So it does not make sense having operation such as +, -, /, * over days. So we convert to factor.
OpaqueEnvelopeRetrofit$DayOfTheWeek <- as.factor(OpaqueEnvelopeRetrofit$DayOfTheWeek)
str(OpaqueEnvelopeRetrofit)

# Third column is the daily mean temperature in Celsius
# It can be analyzed through the use of box plots to highlight some outliers 
boxplot(OpaqueEnvelopeRetrofit$Text, main="Daily mean average temperature", col="red")
# As it is possible to see from the box plot there are no outliers 

# Fourth column is the "Iext" daily average solar radiance
# It can be analyzed through the use of box plots to highlight some outliers 
boxplot(OpaqueEnvelopeRetrofit$Iext, main="Daily mean solar radiance", col="blue")
# As it is possible to see there is a single outlier which corresponds to the max value of the data set 
# We can delete the single outlier
max_iext <-max(OpaqueEnvelopeRetrofit$Iext)
OpaqueEnvelopeRetrofit <- OpaqueEnvelopeRetrofit[OpaqueEnvelopeRetrofit$Iext != max_iext, ]
View(OpaqueEnvelopeRetrofit)
# The value of the 20/02/2018 has been deleted



# Let's analyze the last variable which is the Energy Consumption
boxplot(OpaqueEnvelopeRetrofit$Energy, main="Daily Energy consumption", col="green")
# As it is possible to see from the box plot there are no outliers
# First of all I put to NA all the data that I want to interpolate 
# Data that has 0 energy during Sunday should stay zero. 
OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$Energy == 0 & OpaqueEnvelopeRetrofit$DayOfTheWeek != 1] <- NA 

X <- OpaqueEnvelopeRetrofit$Iext
Y <- OpaqueEnvelopeRetrofit$Energy

y_approx <- approx(x = X, y = Y, xout = which(is.na(OpaqueEnvelopeRetrofit$Energy)))$y
OpaqueEnvelopeRetrofit$Energy  <- ifelse(is.na(OpaqueEnvelopeRetrofit$Energy), y_approx, OpaqueEnvelopeRetrofit$Energy)
View(OpaqueEnvelopeRetrofit)

# We can normalize all data that are numeric 
# So we define a function to do that 
min_max_scale <- function(data) {
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      data[col] <- (data[[col]] - min(data[[col]])) / (max(data[[col]]) - min(data[[col]]))
    }
  }
  return(data)
}
OpaqueEnvelopeRetrofitNorm <- min_max_scale(OpaqueEnvelopeRetrofit)
View(OpaqueEnvelopeRetrofitNorm)
# We can normalize all data that are numeric 
# So we define a function to do that 
min_max_scale <- function(data) {
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      data[col] <- (data[[col]] - min(data[[col]])) / (max(data[[col]]) - min(data[[col]]))
    }
  }
  return(data)
}

# Eliminate data relative to Sunday that has zero energy value 
OpaqueEnvelopeRetrofit <- OpaqueEnvelopeRetrofit[OpaqueEnvelopeRetrofit$Energy != 0, ]

# Let's plot the behavior of the Energy and the External temperature
# in the same space, so we can easily compare them 
with(OpaqueEnvelopeRetrofit, {
  par(mfrow = c(1, 2))
  # Line plot for the consumption of the energy over the days 
  plot(datetime, energy, type="l", xlab="Months", ylab="Energy", main="Energy consumption", col="red")
  plot(datetime, text, type="l",  xlab="Months", ylab="External Temp (°C)", main="External temperature")
})
# It is possible to observe that the energy consumption increase when the external temperature decrease.
# Moreover, in the graph the energy consumption is equal to zero on Sunday.
with(OpaqueEnvelopeRetrofit, {
  par(mfrow = c(1, 2))
  scatter.smooth(datetime, energy, col = "red", xlab="Months", ylab="Energy", main="Energy consumption")
  scatter.smooth(datetime, text, col = "red", xlab="Months", ylab="External Temp (°C)", main="External temperature")
})

# Mean and std temperature and consumption over months 
OpaqueEnvelopeRetrofit$month <- month(OpaqueEnvelopeRetrofit$date)

mean_nov <- mean(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 11])
mean_dic <- mean(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 12])
mean_jan <- mean(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 1])
mean_feb <- mean(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 2])
mean_mar <- mean(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 3])

std_nov <- sd(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 11])
std_dic <- sd(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 12])
std_jan <- sd(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 1])
std_feb <- sd(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 2])
std_mar <- sd(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 3])

mean_energys <- c(mean_nov, mean_dic, mean_jan, mean_feb, mean_mar)
std_energys <- c(std_nov, std_dic, std_jan, std_feb, std_mar)
months <- c("Nov", "Dic", "Jan", "Feb", "Mar")

df <- data.frame(Month=months, MeanEn=mean_energys, StdEn=std_energys)

# Plot Mean and Std of the Energy consumption
ggplot(df , aes(x=Month, y=MeanEn)) + 
  geom_errorbar(aes(ymin=MeanEn - StdEn, ymax=MeanEn + StdEn), width=.3) +
  geom_point(size=2) +
  ggtitle("Energy Consumption") +
  labs(x="Months" ,y="Mean/Std")
mean_nov <- mean(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 11])
mean_dic <- mean(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 12])
mean_jan <- mean(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 1])
mean_feb <- mean(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 2])
mean_mar <- mean(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 3])

std_nov <- sd(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 11])
std_dic <- sd(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 12])
std_jan <- sd(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 1])
std_feb <- sd(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 2])
std_mar <- sd(OpaqueEnvelopeRetrofit$Energy[OpaqueEnvelopeRetrofit$month == 3])

mean_energys <- c(mean_nov, mean_dic, mean_jan, mean_feb, mean_mar)
std_energys <- c(std_nov, std_dic, std_jan, std_feb, std_mar)
months <- c("Nov", "Dic", "Jan", "Feb", "Mar")

df <- data.frame(Month=months, MeanEn=mean_energys, StdEn=std_energys)

# Plot Mean and Std of the Energy consumption
ggplot(df , aes(x=Month, y=MeanEn)) + 
  geom_errorbar(aes(ymin=MeanEn - StdEn, ymax=MeanEn + StdEn), width=.3) +
  geom_point(size=2) +
  ggtitle("Energy Consumption") +
  labs(x="Months" ,y="Mean/Std")

# Do the same for the irradiance 
mean_nov <- mean(OpaqueEnvelopeRetrofit$Iext[OpaqueEnvelopeRetrofit$month == 11])
mean_dic <- mean(OpaqueEnvelopeRetrofit$Iext[OpaqueEnvelopeRetrofit$month == 12])
mean_jan <- mean(OpaqueEnvelopeRetrofit$Iext[OpaqueEnvelopeRetrofit$month == 1])
mean_feb <- mean(OpaqueEnvelopeRetrofit$Iext[OpaqueEnvelopeRetrofit$month == 2])
mean_mar <- mean(OpaqueEnvelopeRetrofit$Iext[OpaqueEnvelopeRetrofit$month == 3])

std_nov <- sd(OpaqueEnvelopeRetrofit$Iext[OpaqueEnvelopeRetrofit$month == 11])
std_dic <- sd(OpaqueEnvelopeRetrofit$Iext[OpaqueEnvelopeRetrofit$month == 12])
std_jan <- sd(OpaqueEnvelopeRetrofit$Iext[OpaqueEnvelopeRetrofit$month == 1])
std_feb <- sd(OpaqueEnvelopeRetrofit$Iext[OpaqueEnvelopeRetrofit$month == 2])
std_mar <- sd(OpaqueEnvelopeRetrofit$Iext[OpaqueEnvelopeRetrofit$month == 3])

mean_energys <- c(mean_nov, mean_dic, mean_jan, mean_feb, mean_mar)
std_energys <- c(std_nov, std_dic, std_jan, std_feb, std_mar)
months <- c("Nov", "Dic", "Jan", "Feb", "Mar")

df <- data.frame(Month=months, MeanEn=mean_energys, StdEn=std_energys)

# Plot Mean and Std of the Energy consumption
ggplot(df , aes(x=Month, y=MeanEn)) + 
  geom_errorbar(aes(ymin=MeanEn - StdEn, ymax=MeanEn + StdEn), width=.3) +
  geom_point(size=2) +
  ggtitle("Irradiance") +
  labs(x="Months" ,y="Mean/Std")



