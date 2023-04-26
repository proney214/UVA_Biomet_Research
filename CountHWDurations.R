#Attempt to count heat waves and durations

library(readxl)
library(dplyr)

RIC_working_weatherprep <- read_excel("~/Downloads/RIC.working.weatherprep-2.xlsx")

HeatWavesModerate <- data.frame(RIC_working_weatherprep$HeatWavesModerate)
mort <- data.frame(RIC_working_weatherprep$mort)

HW <- cbind(HeatWavesModerate, mort)
durationtable <- data.frame(matrix(nrow=5844,ncol=4))

HWcount <- 0
HWdur <- 0
N <- 0

for (i in 1:5844) {
  if (HW[i,1] == 1) {
    N = N + 1 
  }
  else {
    if (N > 0)
      HWcount = HWcount + 1
    durationtable[i,1] = HWcount
    durationtable[i,2] = N
    durationtable[i,3] = i - N
    durationtable[i,4] = HW[i,2]
    N=0
  }
}

example_df <- data.frame(durationtable)
desiredtable <- example_df[!duplicated(example_df$X1), ]
colnames(desiredtable) <- c("HeatWaveCount", "HeatWaveDuration","RowNumber", "mort")
desiredtable[1:2,] <- c(0,0,0,0,0,5)
desiredtable[length(desiredtable$HeatWaveCount)+1,] <- c(length(desiredtable$HeatWaveCount)-1,desiredtable[length(desiredtable$HeatWaveCount),2]+60,desiredtable[length(desiredtable$HeatWaveCount),3]+60)


#Use this for mortalitiy control for heat wave analysis (Actual)

newtable <- data.frame(matrix(data=NA, nrow = 5844,ncol=3))
colnames(newtable) <- c("mortality", "control mortality", "HeatWave")
x <- length(desiredtable$HeatWaveCount)
y <- desiredtable$HeatWaveDuration
z <- desiredtable$RowNumber

#Remove J influence (Winner!) (x-3 includes HW 43 but not 44)

for(i in 2:(x-1)) {
  if (y[[i]] <= z[[i+1]] - z[[i]]) {
    for(j in 1:y[[i]]) {
      print(i)
      newtable[z[[i]]+j-1,1] = mort[z[[i]]+j-1,] #Mortality (Correct)
      newtable[z[[i]]+j-1,2] = mort[z[[i]]-y[[i]]+j-1,] #Control Mortality (Correct)
      newtable[z[[i]]+j-1,3] = i - 2 #HeatWave (Correct)
    }
  }
  else {NULL}
}
#View(newtable)

#Check for overlaps
#y is duration, z are row numbers (start dates)

for(i in 2:(x-1)) {
  if (z[[i]] - y[[i]] < z[[i-1]] + y[[i-1]]) {
    for(j in 1:y[[i]]) {
      newtable[z[[i]]+j-1,1] = NA #Mortality (Correct)
      newtable[z[[i]]+j-1,2] = NA #Control Mortality (Correct)
      newtable[z[[i]]+j-1,3] = i - 2 #HeatWave (Correct)
    }
  }
}

#This provides information that has length 5844
#View(newtable) 

#Remove NAs, reduces table length
finaltable <- na.omit(newtable)
View(finaltable)

