#Attempt to count heat waves and durations

library(readxl)
library(dplyr)

VJI <- read_excel("~/Desktop/VJI.Final.Weather.xlsx",na="NA")
VJI <- VJI[1:5844, ]

HeatWavesModerate <- data.frame(VJI$HeatWavesModerate)
mort <- data.frame(VJI$Mort)

HWM <- HeatWavesModerate
durationtable <- data.frame(matrix(nrow=5844,ncol=3))

HWMcount <- 0
HWMdur <- 0
N <- 0

for (i in 1:5844) {
  if (HWM[i,] == 1) {
    N = N + 1 
  }
  else {
    if (N > 0)
      HWMcount = HWMcount + 1
    durationtable[i,1] = HWMcount
    durationtable[i,2] = N
    durationtable[i,3] = i - N
    N=0
  }
}

example_df <- data.frame(durationtable)
HWM_desiredtable <- example_df[!duplicated(example_df$X1), ]
colnames(HWM_desiredtable) <- c("HeatWavesModerateCount", "HeatWavesModerateDuration","RowNumber")
HWM_desiredtable[1:2,] <- c(0,0,0,0,0,5)
#desiredtable[length(desiredtable$HeatWaveCount)+1,] <- c(length(desiredtable$HeatWaveCount)-1,desiredtable[length(desiredtable$HeatWaveCount),2]+60,desiredtable[length(desiredtable$HeatWaveCount),3]+60)
HWM_desiredtable_final <- HWM_desiredtable[3:(length(HWM_desiredtable$HeatWavesModerateCount)), ]
#desiredtable[length(desiredtable$HeatWaveCount)+1,] <- c(length(desiredtable$HeatWaveCount)-1,desiredtable[length(desiredtable$HeatWaveCount),2]+60,desiredtable[length(desiredtable$HeatWaveCount),3]+60)
tableinfo_HWM_avgdur <- (sum(HWM_desiredtable_final$HeatWavesModerateDuration) / 5844) *  365.25
tableinfo_HWM_count <- HWM_desiredtable_final[length(HWM_desiredtable_final$HeatWavesModerateCount),1]

##### HW Severe
HeatWavesSevere <- data.frame(VJI$HeatWavesSevere)
mort <- data.frame(VJI$Mort)

HWS <- HeatWavesSevere
durationtable <- data.frame(matrix(nrow=5844,ncol=3))

HWScount <- 0
HWSdur <- 0
N <- 0

for (i in 1:5844) {
  if (HWS[i,] == 1) {
    N = N + 1 
  }
  else {
    if (N > 0)
      HWScount = HWScount + 1
    durationtable[i,1] = HWScount
    durationtable[i,2] = N
    durationtable[i,3] = i - N
    N=0
  }
}

example_df <- data.frame(durationtable)
HWS_desiredtable <- example_df[!duplicated(example_df$X1), ]
colnames(HWS_desiredtable) <- c("HeatWavesSevereCount", "HeatWavesSevereDuration","RowNumber")
HWS_desiredtable[1:2,] <- c(0,0,0,0,0,5)
HWS_desiredtable_final <- HWS_desiredtable[3:(length(HWS_desiredtable$HeatWavesSevereCount)), ]
#desiredtable[length(desiredtable$HeatWaveCount)+1,] <- c(length(desiredtable$HeatWaveCount)-1,desiredtable[length(desiredtable$HeatWaveCount),2]+60,desiredtable[length(desiredtable$HeatWaveCount),3]+60)
tableinfo_HWS_avgdur <- (sum(HWS_desiredtable_final$HeatWavesSevereDuration) / 5844) *  365.25
tableinfo_HWS_count <- HWS_desiredtable_final[length(HWS_desiredtable_final$HeatWavesSevereCount),1]


##### HW Extreme
HeatWavesExtreme <- data.frame(VJI$HeatWavesExtreme)
mort <- data.frame(VJI$Mort)

HWE <- HeatWavesExtreme
durationtable <- data.frame(matrix(nrow=5844,ncol=3))

HWEcount <- 0
HWEdur <- 0
N <- 0

for (i in 1:5844) {
  if (HWE[i,] == 1) {
    N = N + 1 
  }
  else {
    if (N > 0)
      HWEcount = HWEcount + 1
    durationtable[i,1] = HWEcount
    durationtable[i,2] = N
    durationtable[i,3] = i - N
    N=0
  }
}

example_df <- data.frame(durationtable)
HWE_desiredtable <- example_df[!duplicated(example_df$X1), ]
colnames(HWE_desiredtable) <- c("HeatWavesExtremeCount", "HeatWavesExtremeDuration","RowNumber")
HWE_desiredtable[1:2,] <- c(0,0,0,0,0,5)
HWE_desiredtable_final <- HWE_desiredtable[3:(length(HWE_desiredtable$HeatWavesExtremeCount)), ]
#desiredtable[length(desiredtable$HeatWaveCount)+1,] <- c(length(desiredtable$HeatWaveCount)-1,desiredtable[length(desiredtable$HeatWaveCount),2]+60,desiredtable[length(desiredtable$HeatWaveCount),3]+60)
tableinfo_HWX_avgdur <- (sum(HWE_desiredtable_final$HeatWavesExtremeDuration) / 5844) *  365.25
tableinfo_HWX_count <- HWE_desiredtable_final[length(HWE_desiredtable_final$HeatWavesExtremeCount),1]

######### CW Moderate
ColdWavesModerate <- data.frame(VJI$ColdWavesModerate)
mort <- data.frame(VJI$Mort)

CWM <- ColdWavesModerate
durationtable <- data.frame(matrix(nrow=5844,ncol=3))

CWMcount <- 0
CWMdur <- 0
N <- 0

for (i in 1:5844) {
  if (CWM[i,] == 1) {
    N = N + 1 
  }
  else {
    if (N > 0)
      CWMcount = CWMcount + 1
    durationtable[i,1] = CWMcount
    durationtable[i,2] = N
    durationtable[i,3] = i - N
    N=0
  }
}

example_df <- data.frame(durationtable)
CWM_desiredtable <- example_df[!duplicated(example_df$X1), ]
colnames(CWM_desiredtable) <- c("ColdWavesModerateCount", "ColdWavesModerateDuration","RowNumber")
CWM_desiredtable[1:2,] <- c(0,0,0,0,0,5)
#desiredtable[length(desiredtable$ColdWaveCount)+1,] <- c(length(desiredtable$ColdWaveCount)-1,desiredtable[length(desiredtable$ColdWaveCount),2]+60,desiredtable[length(desiredtable$ColdWaveCount),3]+60)
CWM_desiredtable_final <- CWM_desiredtable[3:(length(CWM_desiredtable$ColdWavesModerateCount)), ]
#desiredtable[length(desiredtable$ColdWaveCount)+1,] <- c(length(desiredtable$ColdWaveCount)-1,desiredtable[length(desiredtable$ColdWaveCount),2]+60,desiredtable[length(desiredtable$ColdWaveCount),3]+60)
tableinfo_CWM_avgdur <- (sum(CWM_desiredtable_final$ColdWavesModerateDuration) / 5844) *  365.25
tableinfo_CWM_count <- CWM_desiredtable_final[length(CWM_desiredtable_final$ColdWavesModerateCount),1]

##### CW Severe
ColdWavesSevere <- data.frame(VJI$ColdWavesSevere)
mort <- data.frame(VJI$Mort)

CWS <- ColdWavesSevere
durationtable <- data.frame(matrix(nrow=5844,ncol=3))

CWScount <- 0
CWSdur <- 0
N <- 0

for (i in 1:5844) {
  if (CWS[i,] == 1) {
    N = N + 1 
  }
  else {
    if (N > 0)
      CWScount = CWScount + 1
    durationtable[i,1] = CWScount
    durationtable[i,2] = N
    durationtable[i,3] = i - N
    N=0
  }
}

example_df <- data.frame(durationtable)
CWS_desiredtable <- example_df[!duplicated(example_df$X1), ]
colnames(CWS_desiredtable) <- c("ColdWavesSevereCount", "ColdWavesSevereDuration","RowNumber")
CWS_desiredtable[1:2,] <- c(0,0,0,0,0,5)
CWS_desiredtable_final <- CWS_desiredtable[3:(length(CWS_desiredtable$ColdWavesSevereCount)), ]
#desiredtable[length(desiredtable$ColdWaveCount)+1,] <- c(length(desiredtable$ColdWaveCount)-1,desiredtable[length(desiredtable$ColdWaveCount),2]+60,desiredtable[length(desiredtable$ColdWaveCount),3]+60)
tableinfo_CWS_avgdur <- (sum(CWS_desiredtable_final$ColdWavesSevereDuration) / 5844) *  365.25
tableinfo_CWS_count <- CWS_desiredtable_final[length(CWS_desiredtable_final$ColdWavesSevereCount),1]


##### CW Extreme
ColdWavesExtreme <- data.frame(VJI$ColdWavesExtreme)
mort <- data.frame(VJI$Mort)

CWE <- ColdWavesExtreme
durationtable <- data.frame(matrix(nrow=5844,ncol=3))

CWEcount <- 0
CWEdur <- 0
N <- 0

for (i in 1:5844) {
  if (CWE[i,] == 1) {
    N = N + 1 
  }
  else {
    if (N > 0)
      CWEcount = CWEcount + 1
    durationtable[i,1] = CWEcount
    durationtable[i,2] = N
    durationtable[i,3] = i - N
    N=0
  }
}

example_df <- data.frame(durationtable)
CWE_desiredtable <- example_df[!duplicated(example_df$X1), ]
colnames(CWE_desiredtable) <- c("ColdWavesExtremeCount", "ColdWavesExtremeDuration","RowNumber")
CWE_desiredtable[1:2,] <- c(0,0,0,0,0,5)
CWE_desiredtable_final <- CWE_desiredtable[3:(length(CWE_desiredtable$ColdWavesExtremeCount)), ]
#desiredtable[length(desiredtable$ColdWaveCount)+1,] <- c(length(desiredtable$ColdWaveCount)-1,desiredtable[length(desiredtable$ColdWaveCount),2]+60,desiredtable[length(desiredtable$ColdWaveCount),3]+60)
tableinfo_CWX_avgdur <- (sum(CWE_desiredtable_final$ColdWavesExtremeDuration) / 5844) *  365.25
tableinfo_CWX_count <- CWE_desiredtable_final[length(CWE_desiredtable_final$ColdWavesExtremeCount),1]


