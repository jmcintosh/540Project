# www.openair-project.org
#install.packages("openair")
library(openair)

# http://ram-n.github.io/weatherData/
# install.packages("weatherData")
library(weatherData)

# http://www.inside-r.org/r-doc/rpart/rpart
# install.packages("rpart")
library(rpart)

# http://www.inside-r.org/packages/cran/e1071/docs/naiveBayes
#install.packages("e1071")
library(e1071)

# http://www.inside-r.org/r-doc/class/knn
#install.packages("class")
library(class)

all <- read.csv("Butte_2Winters/All_hourly.csv")


# change column names
names(all)[names(all)=="wind_speed.m.s."] <- "ws"
names(all)[names(all)=="wind_direction.deg."] <- "wd"
names(all)[names(all)=="pm25.ug.m3."] <- "pm25"
names(all)[names(all)=="temp.degC."] <- "tempC"

all <- all[!is.na(all$pm25),] # remove rows where pm25 is NA


# convert string to posix timestamp
all$date <- as.POSIXct(all$date, tz = "MST", format="%Y-%m-%d %H:%M:%S")

windRose(all,pollutant = "pm25", paddle = FALSE, angle = 22.5, breaks = c(0,5,10,15,20))
smoothTrend(all,pollutant = "pm25", deseason = TRUE, statistic = "percentile", percentile = c(25,50,75,95), type = "wd")
calendarPlot(all,pollutant = "pm25",year=2014)
#mydata <- mydata

plot(x=all$tempC,y=all$pm25)

all.start.date <- min(all$date)
all.end.date <- max(all$date)

# butte airport code: KBTM

# showAvailableColumns("KBTM","2012-10-01",opt_detailed =T)
# weather <- getWeatherForDate("KBTM", 
#                   start_date = toString(all.start.date),
#                   end_date = toString(all.end.date),
#                   opt_detailed = TRUE,
#                   opt_write_to_file = FALSE,
#                   opt_all_columns = TRUE
#                   )
# save(weather,file="weather.RData")


getNearestWeatherData <- function (timestamp, x=weather$Time){
  index <- which(abs(x-timestamp) == min(abs(x-timestamp)))[[1]]
  weather[index,c(3:7,9,11,13,14)]
}

cToF <- function(tempC){
  tempC*1.8 + 32
}


# test to see which method gets better results
# all.weather1 <- as.data.frame(t(sapply(all$date,getNearestWeatherData)))
# all.weather2 <- as.data.frame(t(sapply(all$date+30*60,getNearestWeatherData))) 
all.weather <- as.data.frame(t(sapply(all$date+60*60,getNearestWeatherData))) # use this one
# all.weather4 <- as.data.frame(t(sapply(all$date+90*60,getNearestWeatherData)))

# all$tempF <- cToF(all$tempC)
# all$TemperatureF1 <- unlist(all.weather1$TemperatureF)
# all$TemperatureF2 <- unlist(all.weather2$TemperatureF)
# all$TemperatureF3 <- unlist(all.weather3$TemperatureF)
# all$TemperatureF4 <- unlist(all.weather4$TemperatureF)
# 
# diff1 <- sum((all$tempF - all$TemperatureF1)^2,na.rm = T)
# diff2 <- sum((all$tempF - all$TemperatureF2)^2,na.rm = T)
# diff3 <- sum((all$tempF - all$TemperatureF3)^2,na.rm = T)
# diff4 <- sum((all$tempF - all$TemperatureF4)^2,na.rm = T)

# all.weather <- all.weather3
# rm(all.weather1)
# rm(all.weather2)
# rm(all.weather3)
# rm(all.weather4)
# all$TemperatureF1 <- NULL
# all$TemperatureF2 <- NULL
# all$TemperatureF3 <- NULL
# all$TemperatureF4 <- NULL


all$w.TemperatureF <- unlist(all.weather$TemperatureF)
all$w.Dew_PointF <- unlist(all.weather$Dew_PointF)
all$w.Humidity <- unlist(all.weather$Humidity)
all$w.Sea_Level_PressureIn <- unlist(all.weather$Sea_Level_PressureIn)
all$w.VisibilityMPH <- unlist(all.weather$VisibilityMPH)
all$w.Wind_SpeedMPH <- unlist(all.weather$Wind_SpeedMPH)
all$w.PrecipitationIn <- unlist(all.weather$PrecipitationIn)
all$w.Conditions <- unlist(all.weather$Conditions)
all$w.WindDirDegrees <- unlist(all.weather$WindDirDegrees)

# sanity checks
plot(cToF(all$tempC),all$w.TemperatureF)
plot(all$wd,all$w.WindDirDegrees)

#clean data
all$w.Dew_PointF[which(all$w.Dew_PointF == -9999)] <- NA
all$w.Humidity <- as.numeric(all$w.Humidity)
all$w.VisibilityMPH[which(all$w.VisibilityMPH == -9999)] <- NA
all$w.Wind_SpeedMPH[which(all$w.Wind_SpeedMPH < 0)] <- NA
all$w.Wind_SpeedMPH[which(all$w.Wind_SpeedMPH == "Calm")] <- 0
all$w.Wind_SpeedMPH <- as.numeric(all$w.Wind_SpeedMPH)
all$w.WindDirDegrees[which(all$w.Wind_SpeedMPH == 0)] <- NA
all$w.Conditions <- as.factor(all$w.Conditions)
all$w.PrecipitationIn[which(all$w.PrecipitationIn == 0.00)] <- 0.005
all$w.PrecipitationIn[which(all$w.PrecipitationIn == "N/A")] <- 0
all$w.PrecipitationIn <- as.numeric(all$w.PrecipitationIn)

# plot pm25 vs everything
plot(x= all$w.TemperatureF, y=all$pm25, na.rm = TRUE)
plot(x= all$w.Dew_PointF, y=all$pm25, na.rm = TRUE)
plot(x= all$w.Humidity, y=all$pm25, na.rm = TRUE)
plot(x= all$w.Sea_Level_PressureIn, y=all$pm25, na.rm = TRUE)
plot(x= all$w.VisibilityMPH, y=all$pm25, na.rm = TRUE) 
plot(x= all$w.Wind_SpeedMPH, y=all$pm25, na.rm = TRUE)
plot(x= all$w.PrecipitationIn, y=all$pm25, na.rm = TRUE)
plot(x= all$w.Conditions, y=all$pm25, na.rm = TRUE)
plot(x= all$w.WindDirDegrees, y=all$pm25, na.rm = TRUE)


airport <- all[all$site == "Airport",]


plot(x= airport$w.TemperatureF, y=airport$pm25, na.rm = TRUE)
plot(x= airport$w.Dew_PointF, y=airport$pm25, na.rm = TRUE)
plot(x= airport$w.Humidity, y=airport$pm25, na.rm = TRUE)
plot(x= airport$w.Sea_Level_PressureIn, y=airport$pm25, na.rm = TRUE)
plot(x= airport$w.VisibilityMPH, y=airport$pm25, na.rm = TRUE) 
plot(x= airport$w.Wind_SpeedMPH, y=airport$pm25, na.rm = TRUE)
plot(x= airport$w.PrecipitationIn, y=airport$pm25, na.rm = TRUE)
plot(x= airport$w.Conditions, y=airport$pm25, na.rm = TRUE)
plot(x= airport$w.WindDirDegrees, y=airport$pm25, na.rm = TRUE)


# let's classify >= 15 as bad, < 15 as good
all$isBad <- all$pm25 >= 35

all.bad <- all[all$isBad,]
all.good <- all[!all$isBad,]
plot(x= all.bad$w.TemperatureF, y=all.bad$pm25, na.rm = TRUE)
plot(x= all.bad$w.Dew_PointF, y=all.bad$pm25, na.rm = TRUE)
plot(x= all.bad$w.Humidity, y=all.bad$pm25, na.rm = TRUE)
plot(x= all.bad$w.Sea_Level_PressureIn, y=all.bad$pm25, na.rm = TRUE)
plot(x= all.bad$w.VisibilityMPH, y=all.bad$pm25, na.rm = TRUE) 
plot(x= all.bad$w.Wind_SpeedMPH, y=all.bad$pm25, na.rm = TRUE)
plot(x= all.bad$w.PrecipitationIn, y=all.bad$pm25, na.rm = TRUE)
plot(x= all.bad$w.Conditions, y=all.bad$pm25, na.rm = TRUE)
plot(x= all.bad$w.WindDirDegrees, y=all.bad$pm25, na.rm = TRUE)
plot(x= all.bad$site, y=all.bad$pm25, na.rm = TRUE)

plot(x= all$site, y=all$pm25, na.rm=TRUE)

plot(x= all.good$w.TemperatureF, y=all.good$pm25, na.rm = TRUE)
plot(x= all.good$w.Dew_PointF, y=all.good$pm25, na.rm = TRUE)
plot(x= all.good$w.Humidity, y=all.good$pm25, na.rm = TRUE)
plot(x= all.good$w.Sea_Level_PressureIn, y=all.good$pm25, na.rm = TRUE)
plot(x= all.good$w.VisibilityMPH, y=all.good$pm25, na.rm = TRUE) 
plot(x= all.good$w.Wind_SpeedMPH, y=all.good$pm25, na.rm = TRUE)
plot(x= all.good$w.PrecipitationIn, y=all.good$pm25, na.rm = TRUE)
plot(x= all.good$w.Conditions, y=all.good$pm25, na.rm = TRUE)
plot(x= all.good$w.WindDirDegrees, y=all.good$pm25, na.rm = TRUE)


numbad <- length(which(all$isBad))
numgood <- length(which(!all$isBad))


# separate training and test
set.seed(456458)
test.bad.idx <- sample(numbad, size = floor(numbad/3))
test.good.idx <- sample(numgood, size = floor(numgood/3))

all.test <- rbind(all[which(all$isBad),][test.bad.idx,], all[which(!all$isBad),][test.good.idx,])
all.train <- rbind(all[which(all$isBad),][-test.bad.idx,], all[which(!all$isBad),][-test.good.idx,])

# plot(x= airport.train$w.TemperatureF, y=airport.train$pm25)
# plot(x= airport.test$w.TemperatureF, y=airport.test$pm25)

# recursive partitioning
rpart.fit <- rpart(isBad ~ pm25,
             data = all.train, method ="class")
rpart.fit <- rpart(isBad ~ w.TemperatureF + w.Humidity + w.Wind_SpeedMPH + w.PrecipitationIn + w.WindDirDegrees,
             data = all.train, method ="class")

rpart.pred <- predict(rpart.fit, newdata = all.test, type = "class")

which(rpart.pred == TRUE)
all.test$isBad[which(rpart.pred==TRUE)]



# naive-bayes
nb.fit <- naiveBayes(isBad ~ pm25,
                     data = airport.train)
# nb.fit <- naiveBayes(isBad ~ w.TemperatureF + w.Humidity + w.Wind_SpeedMPH + w.PrecipitationIn + w.WindDirDegrees,
#                      data = airport.train)

nb.pred <- predict(nb.fit, newdata = airport.test)

which(nb.pred == TRUE)
airport.test$isBad[which(nb.pred==TRUE)]




# k nearest neighbors
knn(airport.train,airport.test,)


# anova
aov.fit <- aov(isBad ~ tempC+ws+w.Humidity+w.PrecipitationIn, data = all)
summary(aov.fit)

