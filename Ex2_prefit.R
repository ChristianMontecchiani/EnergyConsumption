library(readr)

# Store the pre-retrofit data set in a variable and start analyzing it
file_path_preretrofit = "./Desktop/ProgettoR/data/BaselinePeriod.csv"
BaselinePeriod <- read.csv(file_path_preretrofit)

# In this data set there are no missing values
# Let's start analyzing each column

# The first column is the date. Since it is a date it is possible to convert in datetime,
# which will be useful to plot data
BaselinePeriod$date <- as.POSIXct(BaselinePeriod$date, format="%Y-%m-%d") 

# It contains all the dates from the 01/11/2017 to 31/03/2018

# The second column is 'DayOfTheWeek', which is an int variable but it represents Monday, Tuesday, ..., Sunday
# So it does not make sense having operation such as +, -, /, * over days. So we convert to factor.
BaselinePeriod$DayOfTheWeek <- as.factor(BaselinePeriod$DayOfTheWeek)
str(BaselinePeriod)

# Third column is the daily mean temperature in Celsius
# It can be analyzed through the use of box plots to highlight some outliers 
boxplot(BaselinePeriod$Text, main="Daily mean average temperature", col="red")
# As it is possible to see from the box plot there are no outliers 

# Fourth column is the "Iext" daily average solar radiance
# It can be analyzed through the use of box plots to highlight some outliers 
boxplot(BaselinePeriod$Iext, main="Daily mean solar radiance", col="blue")
# As it is possible to see there is a single outlier which corresponds to the max value of the data set 
# We can delete the single outlier
max_iext <-max(BaselinePeriod$Iext)
BaselinePeriod <- BaselinePeriod[BaselinePeriod$Iext != max_iext, ]
View(BaselinePeriod)
# The value of the 20/02/2018 has been deleted

# Let's analyze the last variable which is the Energy Consumption
boxplot(BaselinePeriod$Energy, main="Daily Energy consumption", col="green")
# As it is possible to see from the box plot there are no outliers

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
BaselinePeriodNorm <- min_max_scale(BaselinePeriod)

# Save each column in a variable to plot them
datetime <- BaselinePeriod$date
energy <- BaselinePeriod$Energy
text <- BaselinePeriod$Text
iext <- BaselinePeriod$Iext

# Let's plot the behavior of the Energy and the External temperature
# in the same space, so we can easily compare them 
with(BaselinePeriod, {
  par(mfrow = c(1, 2))
  # Line plot for the consumption of the energy over the days 
  plot(datetime, energy, type="l", xlab="Months", ylab="Energy", main="Energy consumption", col="red")
  plot(datetime, text, type="l",  xlab="Months", ylab="External Temp (°C)", main="External temperature")
})
# It is possible to observe that the energy consumption increase when the external temperature decrease.
# Moreover, in the graph the energy consumption is equal to zero on Sunday.


with(BaselinePeriod, {
  par(mfrow = c(1, 3))
  scatter.smooth(datetime, energy, col = "red")
  scatter.smooth(datetime, text, col = "red")
  scatter.smooth(datetime, iext, col = "red", span = .3) # tweak with span
})

