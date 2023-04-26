# PROGRAM FOR 3-CAVS WEATHER DATA EXTRACTION AND CLEANING
#
# READ IN RAW DATA, Convert to numeric
library(readxl)
library(lubridate)
library(dplyr)
#
# Create Null Data Frame
#
df=data.frame(col1=numeric(),col2=numeric(),col3=numeric(),col4=numeric())
#
# CHANGE THE FOLLOWING LINE TO ACCESS YOUR "XXX.working.xlsx" FILE.  BE SURE COLUMNS HAVE CORRECT LABELS.
Station.test=read_excel("SHD.working.xlsx",na="NA")
temp=as.numeric(Station.test$Temp)
tdew=as.numeric(Station.test$DewPoint)
wind=as.numeric(Station.test$Wind)
Station.test$test = as.POSIXct(Station.test$DateTime, format="%m-%d-%Y %H:%M:%OS", tz='EST')
##
#Create vector with correct times (365 days, 4 obs. per day) as the reference
#
correct_time=seq(as.POSIXlt("2005-01-01 01:00:00", tz='EST'),
                 +      as.POSIXlt("2020-12-31 19:00:00", tz="EST"), by="360 min")
#
# i is index of the correct times (correct_time)
# j is index of observed times (Station.test$observed)
#
# 23376 is size of full data set
#
print ("start loop")
i=1
j=1
diff=0
for (i in 1:23376) {
  diff=difftime(correct_time[i],Station.test$test[j],units="mins")
  print(c(i, j))
  if ((abs(diff))<=90) {
    print("Time diff < 90 minutes")
    vec=c(Station.test$test[j],temp[j],tdew[j],wind[j])
    df=rbind(df,vec)
    j=j+1
  }  else {vec=c(correct_time[i],"NA","NA","NA")
  df=rbind(df,vec)
  print("Diff > 90 min—Null observation")
  }
}
#
#  Output dataframe as csv file
#
# CHANGE NAME AND DIRECTORY OF OUTPUT FILE
write.csv(df,"~/Desktop/EVSC_Research/SHD.test.DavisFill.output.csv",row.names=FALSE)
#
print("Weather Cleaner 5 has successfully run.")
#
#

#Time, Temp, Dew point, wind Interpolation for entirety of written length

raw <- read.csv("SHD.test.DavisFill.output.csv", sep = ",")
Final <- na_interpolation(raw)
write.csv(Final, "~/Desktop/EVSC_Research/SHD.DavisFill.FullInt.csv")
