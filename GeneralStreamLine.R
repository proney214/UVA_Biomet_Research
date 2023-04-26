# GENERAL STREAMLINED PROGRAM FOR 3-CAVS WEATHER DATA EXTRACTION AND CLEANING
#
#  INSTALL PACKAGES AND LOAD LIBRARIES
#
library(readxl)
library(zoo)
library(lubridate)
#
# READ IN RAW DATA 2005-2006, Convert to numeric
#
CITY.test0506=read_excel("NewExcelTest0506.xlsx",na="M")
temp=as.numeric(CITY.test0506$tmpc)
tdew=as.numeric(CITY.test0506$dwpc)
wind=as.numeric(CITY.test0506$sknt)
#
# LINEARLY INTERPOLATE ALL MISSING VALUES
#
temp.interp=na.approx(temp,rule=2, maxgap = 5)
tdew.interp=na.approx(tdew,rule=2, maxgap = 5)
wind.interp=na.approx(wind, rule=2, maxgap = 5)
#
# CREATE NEW DATA FRAME USING THE INTERPOLATED VALUES
#
CITYFinal0506=data.frame(CITY.test0506$station,CITY.test0506$valid,temp.interp,tdew.interp,wind.interp)
i = which(is.na(CITYFinal0506)) #find remaining NA's
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
  for (j in 1:nrow(CITYFinal0506)) {
    test_diff=difftime(CITYFinal0506$CITYF.test.valid[j],Ref.time$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(CITYFinal0506[keep_row,],file="~/Desktop/EVSC_Research/CITYInt2005.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(CITYFinal0506[keep_row,])
}
A = read.table("CITYInt2005.csv")

### 2006 EXTRACTION ###

#If error occurs, check column names after $ in the loop
for (i in 1461:2920) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(CITYFinal0506)) {
    test_diff=difftime(CITYFinal0506$CITYFINALvalid[j],Ref.time$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(CITYFinal0506[keep_row,],file="~/Desktop/EVSC_Research/CITYInt2006.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(CITYFinal0506[keep_row,])
}
B = read.table("CITYInt2006.csv")


# READ IN RAW DATA 2007-2008, Convert to numeric
#
CITY.test0708=read_excel("NewExcelTest0708.xlsx",na="M")
temp=as.numeric(CITY.test0708$tmpc)
tdew=as.numeric(CITY.test0708$dwpc)
wind=as.numeric(CITY.test0708$sknt)
#
# LINEARLY INTERPOLATE ALL MISSING VALUES
#
temp.interp=na.approx(temp,rule=2, maxgap = 5)
tdew.interp=na.approx(tdew,rule=2, maxgap = 5)
wind.interp=na.approx(wind, rule=2, maxgap = 5)
#
# CREATE NEW DATA FRAME USING THE INTERPOLATED VALUES
#
CITYFinal0708=data.frame(CITY.test0708$station,CITY.test0708$valid,temp.interp,tdew.interp,wind.interp)
i = which(is.na(CITYFinal0708)) #find remaining NA's
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
  for (j in 1:nrow(CITYFinal0708)) {
    test_diff=difftime(CITYFinal0708$CITYF.test.valid[j],Ref.time$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(CITYFinal0708[keep_row,],file="~/Desktop/EVSC_Research/CITYInt2007.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(CITYFinal0708[keep_row,])
}
C = read.table("CITYInt2007.csv")

### 2008 EXTRACTION INVOLVES LEAP YEAR###

#If error occurs, check column names after $ in the loop
for (i in 1461:2921) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(CITYFinal0708)) {
    test_diff=difftime(CITYFinal0708$CITYFINALvalid[j],Ref.time$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(CITYFinal0506[keep_row,],file="~/Desktop/EVSC_Research/CITYInt2008.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(CITYFinal0506[keep_row,])
}
D = read.table("CITYInt2008.csv")


### 2009 EXTRACTION ###

#If error occurs, check column names after $ in the loop
for (i in 5842:7301) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(CITYFinal)) {
    test_diff=difftime(CITYFinal$CITYFINALvalid[j],Ref.time$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(CITYFinal[keep_row,],file="~/Desktop/EVSC_Research/CITYInt2009.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(CITYFinal[keep_row,])
}
E = read.table("CITYInt2009.csv")


### 2010 EXTRACTION ###

#If error occurs, check column names after $ in the loop
for (i in 7302:8761) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(CITYFinal)) {
    test_diff=difftime(CITYFinal$CITYFINALvalid[j],Ref.time$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(CITYFinal[keep_row,],file="~/Desktop/EVSC_Research/CITYInt2010.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(CITYFinal[keep_row,])
}
F = read.table("CITYInt2010.csv")


### 2011 EXTRACTION ###

#If error occurs, check column names after $ in the loop
for (i in 8762:10221) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(CITYFinal)) {
    test_diff=difftime(CITYFinal$CITYFINALvalid[j],Ref.time$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(CITYFinal[keep_row,],file="~/Desktop/EVSC_Research/CITYInt2011.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(CITYFinal[keep_row,])
}
G = read.table("CITYInt2011.csv")

### 2012 EXTRACTION ###

### LEAP YEAR INSERT EXTRA DAY

#If error occurs, check column names after $ in the loop
for (i in 10222:11682) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(CITYFinal)) {
    test_diff=difftime(CITYFinal$CITYFINALvalid[j],Ref.time$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(CITYFinal[keep_row,],file="~/Desktop/EVSC_Research/CITYInt2012.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(CITYFinal[keep_row,])
}
H = read.table("CITYInt2012.csv")


### 2013 EXTRACTION ###

#If error occurs, check column names after $ in the loop
for (i in 11683:13142) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(CITYFinal)) {
    test_diff=difftime(CITYFinal$CITYFINALvalid[j],Ref.time$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(CITYFinal[keep_row,],file="~/Desktop/EVSC_Research/CITYInt2013.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(CITYFinal[keep_row,])
}
I = read.table("CITYInt2013.csv")


### 2014 EXTRACTION ###

#If error occurs, check column names after $ in the loop
for (i in 13143:14602) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(CITYFinal)) {
    test_diff=difftime(CITYFinal$CITYFINALvalid[j],Ref.time$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(CITYFinal[keep_row,],file="~/Desktop/EVSC_Research/CITYInt2014.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(CITYFinal[keep_row,])
}
J = read.table("CITYInt2014.csv")

### 2015 EXTRACTION ###

#If error occurs, check column names after $ in the loop
for (i in 14603:16062) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(CITYFinal)) {
    test_diff=difftime(CITYFinal$CITYFINALvalid[j],Ref.time$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(CITYFinal[keep_row,],file="~/Desktop/EVSC_Research/CITYInt2015.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(CITYFinal[keep_row,])
}
K = read.table("CITYInt2015.csv")

### 2016 EXTRACTION ###

### LEAP YEAR INSERT AN EXTRA DAY

#If error occurs, check column names after $ in the loop
for (i in 17523:18983) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(CITYFinal)) {
    test_diff=difftime(CITYFinal$CITYFINALvalid[j],Ref.time$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(CITYFinal[keep_row,],file="~/Desktop/EVSC_Research/CITYInt2016.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(CITYFinal[keep_row,])
}
L = read.table("CITYInt2016.csv")


### 2017 EXTRACTION ###

#If error occurs, check column names after $ in the loop
for (i in 18984:20443) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(CITYFinal)) {
    test_diff=difftime(CITYFinal$CITYFINALvalid[j],Ref.time$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(CITYFinal[keep_row,],file="~/Desktop/EVSC_Research/CITYInt2017.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(CITYFinal[keep_row,])
}
M = read.table("CITYInt2017.csv")


### 2018 EXTRACTION ###

#If error occurs, check column names after $ in the loop
for (i in 20444:21903) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(CITYFinal)) {
    test_diff=difftime(CITYFinal$CITYFINALvalid[j],Ref.time$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(CITYFinal[keep_row,],file="~/Desktop/EVSC_Research/CITYInt2018.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(CITYFinal[keep_row,])
}
N = read.table("CITYInt2018.csv")


### 2019 EXTRACTION ###
#Skip O since it looks like a 0 and went to P for table variable name
#If error occurs, check column names after $ in the loop
for (i in 21904:23363) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(CITYFinal)) {
    test_diff=difftime(CITYFinal$CITYFINALvalid[j],Ref.time$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(CITYFinal[keep_row,],file="~/Desktop/EVSC_Research/CITYInt2019.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(CITYFinal[keep_row,])
}
P = read.table("CITYInt2019.csv")


### 2020 EXTRACTION ###
### LEAP YEAR INSERT EXTRA DAY
#If error occurs, check column names after $ in the loop
for (i in 23364:24824) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(CITYFinal)) {
    test_diff=difftime(CITYFinal$CITYFINALvalid[j],Ref.time$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  write.table(CITYFinal[keep_row,],file="~/Desktop/EVSC_Research/CITYInt2020.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(CITYFinal[keep_row,])
}
Q = read.table("CITYInt2020.csv")

### You've made it to the end of CITY filtration stream. Congratulaitons :)
