*****Filling Length with Dates **********
  
  # PROGRAM FOR 3-CAVS AIR QUALITY CLEANING
  # Air Quality Files
  #
  # READ IN RAW DATA, Convert to numeric
  library(readxl)
library(lubridate)
library(dplyr)
#
#
# CHANGE THE FOLLOWING LINE TO ACCESS YOUR "XXX.working.xlsx" FILE.  BE SURE COLUMNS HAVE CORRECT LABELS.
VABeach_PM2_5_Combined <- read_excel("~/Downloads/RIC.PM2.5.Combined.xlsx",
                                     sheet = "Henrico2",na="NA")
Station.test<-VABeach_PM2_5_Combined
Station.test$Time = as.POSIXct(Station.test$Time, format="%Y-%m-%d", tz='UTC')

df<-Station.test %>%
    group_by(Time) %>%
      summarise(Mean=mean(ValueSad))

df=data.frame(df$Mean,df$Time)
df_output=data.frame(col1=numeric(),col2=numeric())
Value=df$df.Mean
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
  diff=difftime(correct_time[i],df$df.Time[j],units="days")
  print(c(i, j))
  if ((abs(diff))==0) {
    print("Dates match")
    vec=c(df$df.Time[j],Value[j])
    df_output=rbind(df_output,vec)
    j=j+1
  }  else {vec=c(correct_time[i],"NA","NA")
  df_output=rbind(df_output,vec)
  print("Date mismatch")
  }
}
#
#  Output dataframe as csv file
#
# CHANGE NAME AND DIRECTORY OF OUTPUT FILE
write.csv(df_output,"AQ.Henrico2.output.csv",row.names=FALSE)
#
print("Weather Cleaner Air Quality has successfully run.")
#
#