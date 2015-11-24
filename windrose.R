# www.openair-project.org
#install.packages("openair")
#library(openair)

airport <- read.csv("Butte_2Winters/Airport_hourly.csv")

# change column names
names(airport)[names(airport)=="wind_speed.m.s."] <- "ws"
names(airport)[names(airport)=="wind_direction.deg."] <- "wd"
names(airport)[names(airport)=="pm25.ug.m3."] <- "pm25"
names(airport)[names(airport)=="temp.degC."] <- "tempC"


#airport <- airport[complete.cases(airport),]

# convert string to posix timestamp
airport$date <- as.POSIXct(airport$date)

windRose(airport,pollutant = "pm25", paddle = FALSE, angle = 22.5, breaks = c(0,5,10,15,20))
smoothTrend(airport,pollutant = "pm25", deseason = TRUE, statistic = "percentile", percentile = c(25,50,75,95), type = "wd")
calendarPlot(airport,pollutant = "pm25",year=2014)
#mydata <- mydata

plot(x=airport$tempC,y=airport$pm25)
