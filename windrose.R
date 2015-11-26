# www.openair-project.org
#install.packages("openair")
library(openair)

# http://ram-n.github.io/weatherData/
# install.packages("weatherData")
library(weatherData)

airport <- read.csv("Butte_2Winters/Airport_hourly.csv")

# change column names
names(airport)[names(airport)=="wind_speed.m.s."] <- "ws"
names(airport)[names(airport)=="wind_direction.deg."] <- "wd"
names(airport)[names(airport)=="pm25.ug.m3."] <- "pm25"
names(airport)[names(airport)=="temp.degC."] <- "tempC"


#airport <- airport[complete.cases(airport),]

# convert string to posix timestamp
airport$date <- as.POSIXct(airport$date, tz = "MST", format="%Y-%m-%d %H:%M:%S")

windRose(airport,pollutant = "pm25", paddle = FALSE, angle = 22.5, breaks = c(0,5,10,15,20))
smoothTrend(airport,pollutant = "pm25", deseason = TRUE, statistic = "percentile", percentile = c(25,50,75,95), type = "wd")
calendarPlot(airport,pollutant = "pm25",year=2014)
#mydata <- mydata

plot(x=airport$tempC,y=airport$pm25)

airport.start.date <- min(airport$date)
airport.end.date <- max(airport$date)

# butte airport code: KBTM

showAvailableColumns("KBTM","2012-10-01",opt_detailed =T)
weather <- getWeatherForDate("KBTM", 
                  start_date = toString(airport.start.date),
                  end_date = toString(airport.end.date),
                  opt_detailed = TRUE,
                  opt_write_to_file = FALSE,
                  opt_all_columns = TRUE
                  )

# weather <- read.csv("weather.csv")
# weather$X <- NULL

getNearestWeatherData <- function (timestamp, x=weather$Time){
  index <- which(abs(x-timestamp) == min(abs(x-timestamp)))[[1]]
  weather[index,c(3:7,9,11,13,14)]
}

cToF <- function(tempC){
  tempC*1.8 + 32
}


# test to see which method gets better results
airport.weather1 <- as.data.frame(t(sapply(airport$date,getNearestWeatherData)))
airport.weather2 <- as.data.frame(t(sapply(airport$date+30*60,getNearestWeatherData))) # use this one

airport$tempF <- cToF(airport$tempC)
airport$TemperatureF1 <- unlist(airport.weather1$TemperatureF)
airport$TemperatureF2 <- unlist(airport.weather2$TemperatureF)

diff1 <- sum((airport$tempF - airport$TemperatureF1)^2,na.rm = T)
diff2 <- sum((airport$tempF - airport$TemperatureF2)^2,na.rm = T)

airport.weather <- airport.weather2
rm(airport.weather1)
rm(airport.weather2)
airport$TemperatureF1 <- NULL
airport$TemperatureF2 <- NULL


airport$w.TemperatureF <- unlist(airport.weather$TemperatureF)
airport$w.Dew_PointF <- unlist(airport.weather$Dew_PointF)
airport$w.Humidity <- unlist(airport.weather$Humidity)
airport$w.Sea_Level_PressureIn <- unlist(airport.weather$Sea_Level_PressureIn)
airport$w.VisibilityMPH <- unlist(airport.weather$VisibilityMPH)
airport$Wind_SpeedMPH <- unlist(airport.weather$Wind_SpeedMPH)
airport$PrecipitationIn <- unlist(airport.weather$PrecipitationIn)
airport$Conditions <- unlist(airport.weather$Conditions)
airport$w.WindDirDegrees <- unlist(airport.weather$WindDirDegrees)

plot(cToF(airport$tempC),airport$TemperatureF)
plot(airport$wd,airport$w.WindDirDegrees)
