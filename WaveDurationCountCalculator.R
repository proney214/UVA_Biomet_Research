ColdWavesModerate <- data.frame(EMV$ColdWavesModerate)
mort <- data.frame(EMV$Mort)

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

##### HW Severe
ColdWavesSevere <- data.frame(EMV$ColdWavesSevere)
mort <- data.frame(EMV$Mort)

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


##### HW Extreme
ColdWavesExtreme <- data.frame(EMV$ColdWavesExtreme)
mort <- data.frame(EMV$Mort)

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


