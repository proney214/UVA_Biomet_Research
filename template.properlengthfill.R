******Filling Length with Dates **********
  
  # PROGRAM FOR 3-CAVS WEATHER DATA EXTRACTION AND CLEANING
  # Max-Min Temperature Files
  #
  # READ IN RAW DATA, Convert to numeric
library(readxl)
library(lubridate)
library(dplyr)
#
#
# CHANGE THE FOLLOWING LINE TO ACCESS YOUR "XXX.working.xlsx" FILE.  BE SURE COLUMNS HAVE CORRECT LABELS.
Station.test= read_excel("BLU.maxmin-1.xlsx", na="NA")
Station.test$Time = as.POSIXct(Station.test$Time, format="%Y-%m-%d", tz='UTC')

df=data.frame(Station.test$Max,Station.test$Min,Station.test$Time)
df_output=data.frame(col1=numeric(),col2=numeric(),col3=numeric())
Tmax=as.numeric(Station.test$Max)
Tmin=as.numeric(Station.test$Min)
##
#Create vector with correct times (365 days) as the reference
#
correct_time=seq(as.POSIXlt("2005-01-01",tz="UTC"),
                 +      as.POSIXlt("2020-12-31",tz="UTC"), by="days")
#
# i is index of the correct times (correct_time)
# j is index of observed times (Station.test$observed)
#
# 5844 is size of full data set, one observation per day
#
print ("start loop")
i=1
j=1
diff=0
for (i in 1:5844) { 
  print(c(i, j))
  diff=difftime(correct_time[i],Station.test$Time[j],units="days")
  print(c(i, j))
  if ((abs(diff))==0) {
    print("Dates match")
    vec=c(Station.test$Time[j],Tmax[j],Tmin[j])
    df_output=rbind(df_output,vec)
    j=j+1
  }  else {vec=c(correct_time[i],"NA","NA","NA")
  df_output=rbind(df_output,vec)
  print("Date mismatch")
  }
}
#
#  Output dataframe as csv file
#
# CHANGE NAME AND DIRECTORY OF OUTPUT FILE
write.csv(df_output,"BLFACT.maxmin.output.csv",row.names=FALSE)
#
print("Weather Cleaner MaxMin has successfully run.")
#
#

library(imputeTS)
raw <- read.csv("BLFACT.maxmin.output.csv", sep = ",")
Final <- na_interpolation(raw)
write.csv(Final, "~/Desktop/EVSC_Research/BLFACT.maxmin.FullInt3.csv")

