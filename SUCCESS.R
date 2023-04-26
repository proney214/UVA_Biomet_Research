# TEST PROGRAM FOR 3-CAVS WEATHER DATA EXTRACTION AND CLEANING
#
# READ IN RAW DATA, Convert to numeric

library(lubridate)

NewNew.test=read.csv("NewNewTEST.csv", na.strings="M")
temp=as.numeric(NewNew.test$tmpc)
tdew=as.numeric(NewNew.test$dwpc)
wind=as.numeric(NewNew.test$sknt)
press=as.numeric(NewNew.test$mslp)

NewNew.test$DTEST = ymd_hm(NewNew.test$valid)
NewNew.test$DTEST = force_tz(mdy_hm(NewNew.test$valid), tzone = 'EST')
NewNew.test$DTEST = as.POSIXct(NewNew.test$DTEST)

#
# READ IN FILE WITH CORRECT, REFERENCE TIMES (365 OBS)
#
Ref.time=read.csv("Ref.time.test.csv")
Ref.time$DTEST = ymd_hm(Ref.time$ref_time)
Ref.time$DTEST = force_tz(mdy_hm(Ref.time$ref_time), tzone = 'EST')
Ref.time$DTEST = as.POSIXct(Ref.time$DTEST)
#
# INITIATE VALUES TO PASS INTO LOOP
#
# Compare time in raw data file to sequential times in the reference data file. 
# Reference time is the outer loop.
#
print (nrow(NewNew.test))
for (i in 1:365) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(NewNew.test)) {
    test_diff=difftime(NewNew.test$DTEST[j],Ref.time$DTEST[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  print(c(keep_row,NewNew.test$valid[keep_row]))
  write.table(NewNew.test[keep_row,],file="~/Desktop/EVSC_Research/NewNewoutput.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(NewNew.test[keep_row,])
}

Missing_Test = read.csv("NewNewoutput.csv", na.strings = 'NA')
NewMiss = na.omit(Missing_Test)
