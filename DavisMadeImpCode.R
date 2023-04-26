# TEST PROGRAM FOR 3-CAVS WEATHER DATA EXTRACTION AND CLEANING
#
# READ IN RAW DATA, Convert to numeric
#install.packages("lubridate")
#install.packages("readxl")
library(readxl)
library(lubridate)
library(dplyr)

Station.test=read_excel("PHF.working.xlsx",na="NA")
temp=as.numeric(Station.test$`Air Temp (°C)`)
tdew=as.numeric(Station.test$`Dew Point (°C)`)
wind=as.numeric(Station.test$`Wind Speed (sknt)`)

Station.test$test = as.POSIXct(Station.test$DateTime, format="%m-%d-%Y %H:%M:%OS")
#Station.test <- select(-Station.test$`Valid Date`,-Station.test$`Valid Time`)

***###FROM HERE TO CREATION OF VECTOR WITH CORRECT TIMES EVERYTHING IS A JUNK COMMAND***

#Station.test$test = as.POSIXct.Date(Station.test$DateTime, c('mdy_HMS','mdy_HMS'))

#t = as.POSIXct(Station.test$DateTime)

#Station.test$test= as.Date(parse_date_time(Station.test$DateTime, c('mdy HMS','mdy_HMS')))
#Station.test$test = as.POSIXct(Station.test$DateTime, format="%m-%d-%Y %H:%M:%OS")
#Station.test$test2 = as.Date()

#Station.test$DateTime = as.POSIXct(Station.test$DateTime, tz = "EST", format, tryFormats = c(%Y-%m-%d %H:%M:%OS""))
#Station.test$observed = ymd_hms(Station.test$DateTime)
#Station.test$observed = force_tz(mdy_hm(Station.test$DateTime),tzone='EST')

***###RESTART OF GOOD PROGRAM***

#Create vector with correct times (365 days, 4 obs. per day) as the reference
#
correct_time=seq(as.POSIXlt("2005-01-01 01:00:00", tz='EST'),
                 +      as.POSIXlt("2020-12-31 19:00:00", tz="EST"), by="360 min")

#
#
# i is index of the correct times (correct_time)
# j is index of observed times (Station.test$observed)
#
print ("start loop")
j=1
for (i in 1:23376) {
  diff=difftime(correct_time[i],Station.test$test[j],units="mins")
  #print(c(i, j, diff))
  if ((abs(diff))<=90) {
    print(c("1st loop",Station.test$test[j]))
    write.table(Station.test[j,],file="~/Desktop/EVSC_Research/PHF.test.att6.complete.output.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
    j=j+1
  }  else {print(correct_time[i])
    print(c("2nd loop",correct_time[i]))
    write.table(correct_time[i],file="~/Desktop/EVSC_Research/PHF.test.att6.complete.output.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  }
}
#
#Time, Temp, Dew point, wind Interpolation for entirety of writtent length




