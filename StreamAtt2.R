# PHF ATTEMPT 2 STREAMLINED PROGRAM FOR 3-CAVS WEATHER DATA EXTRACTION AND CLEANING
#
#  INSTALL PACKAGES AND LOAD LIBRARIES
#
library(readxl)
library(zoo)
library(lubridate)
#
# READ IN RAW DATA 2005-2006, Convert to numeric
#
PHF.test0506=read_excel("Newport2005_2006.xlsx",na="M")
temp=as.numeric(PHF.test0506$tmpc)
tdew=as.numeric(PHF.test0506$dwpc)
wind=as.numeric(PHF.test0506$sknt)
#
# LINEARLY INTERPOLATE ALL MISSING VALUES
#
temp.interp=na.approx(temp,rule=2, maxgap = 5)
tdew.interp=na.approx(tdew,rule=2, maxgap = 5)
wind.interp=na.approx(wind, rule=2, maxgap = 5)
#
# CREATE NEW DATA FRAME USING THE INTERPOLATED VALUES
#
PHFFinal0506=data.frame(PHF.test0506$station,PHF.test0506$valid,temp.interp,tdew.interp,wind.interp)
i = which(is.na(PHFFinal0506)) #find remaining NA's
length(i) #Completely full data with interpolated values
#
#
#
# READ IN FILE WITH CORRECT, REFERENCE TIMES (720 OBS)
#
Ref.time0506=read_excel("Date.Time.Actual.20052006.xlsx")
#
# INITIATE VALUES TO PASS INTO LOOP
#
# Compare time in raw data file to sequential times in the reference data file. 
# Reference time is the outer loop.
#
### 2005 EXTRACTION ###

#If error occurs, check column names after $ in the loop
for (i in 1:1460) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(PHFFinal0506)) {
    test_diff=difftime(PHFFinal0506$PHF.test0506.valid[j],Ref.time0506$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(PHFFinal0506[keep_row,],file="~/Desktop/EVSC_Research/PHFInt2005.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(PHFFinal0506[keep_row,])
}
A = read.table("PHFInt2005.csv")

### 2006 EXTRACTION ###

#If error occurs, check column names after $ in the loop
for (i in 1461:2920) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(PHFFinal0506)) {
    test_diff=difftime(PHFFinal0506$PHF.test0506.valid[j],Ref.time0506$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(PHFFinal0506[keep_row,],file="~/Desktop/EVSC_Research/PHFInt2006.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(PHFFinal0506[keep_row,])
}
B = read.table("PHFInt2006.csv")


# READ IN RAW DATA 2007-2008, Convert to numeric
#
PHF.test0708=read_excel("Newport20072008.xlsx",na="M")
temp=as.numeric(PHF.test0708$tmpc)
tdew=as.numeric(PHF.test0708$dwpc)
wind=as.numeric(PHF.test0708$sknt)
#
# LINEARLY INTERPOLATE ALL MISSING VALUES
#
temp.interp=na.approx(temp,rule=2, maxgap = 5)
tdew.interp=na.approx(tdew,rule=2, maxgap = 5)
wind.interp=na.approx(wind, rule=2, maxgap = 5)
#
# CREATE NEW DATA FRAME USING THE INTERPOLATED VALUES
#
PHFFinal0708=data.frame(PHF.test0708$station,PHF.test0708$valid,temp.interp,tdew.interp,wind.interp)
i = which(is.na(PHFFinal0708)) #find remaining NA's
length(i) #Completely full data with interpolated values
#
#
#
# READ IN FILE WITH CORRECT, REFERENCE TIMES (721 OBS)
#
Ref.time0708=read_excel("Date.Time.Actual.20072008.xlsx")
#
# INITIATE VALUES TO PASS INTO LOOP
#
# Compare time in raw data file to sequential times in the reference data file. 
# Reference time is the outer loop.
#
### 2007 EXTRACTION ###

#If error occurs, check column names after $ in the loop
for (i in 1:1460) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(PHFFinal0708)) {
    test_diff=difftime(PHFFinal0708$PHF.test0708.valid[j],Ref.time0708$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(PHFFinal0708[keep_row,],file="~/Desktop/EVSC_Research/PHFInt2007.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(PHFFinal0708[keep_row,])
}
C = read.table("PHFInt2007.csv")

### 2008 EXTRACTION INVOLVES LEAP YEAR###

#If error occurs, check column names after $ in the loop
for (i in 1461:2921) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(PHFFinal0708)) {
    test_diff=difftime(PHFFinal0708$PHF.test0708.valid[j],Ref.time0708$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(PHFFinal0708[keep_row,],file="~/Desktop/EVSC_Research/PHFInt2008.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(PHFFinal0708[keep_row,])
}
D = read.table("PHFInt2008.csv")