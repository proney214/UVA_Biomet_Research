library(readxl)
library(dplyr)

CHO <- read_excel("~/Desktop/CHO.Final.Weather.xlsx",na="NA")
CHO <- CHO[1:5844, ]
#Convert Data to numeric
CHO %>% mutate_if(is.character, as.numeric)
#head(CHO)
for(i in 1:ncol(CHO)){
  assign(names(CHO)[i], CHO[[i]])
}

MDC_Counting_CHO <- data.frame(matrix(NA))

MDC_Counting_CHO[1,1] <- sum(CHO$Mort==0)
MDC_Counting_CHO[1,2] <- sum(CHO$Mort, na.rm = T)
MDC_Counting_CHO[2,1] <- sum(CHO$MDC.01==0)
MDC_Counting_CHO[2,2] <- sum(CHO$MDC.01, na.rm = T)
MDC_Counting_CHO[3,1] <- sum(CHO$MDC.04==0)
MDC_Counting_CHO[3,2] <- sum(CHO$MDC.04, na.rm = T)
MDC_Counting_CHO[4,1] <- sum(CHO$MDC.05==0)
MDC_Counting_CHO[4,2] <- sum(CHO$MDC.05, na.rm = T)
MDC_Counting_CHO[5,1] <- sum(CHO$MDC.19==0)
MDC_Counting_CHO[5,2] <- sum(CHO$MDC.19, na.rm = T)
MDC_Counting_CHO[6,1] <- sum(CHO$Non.billable.ICD==0)
MDC_Counting_CHO[6,2] <- sum(CHO$Non.billable.ICD, na.rm = T)

MDC_Counting_CHO[1,3] <- round((1-(MDC_Counting_CHO[1,1] / 5844))*100,digits = 2)
MDC_Counting_CHO[2,3] <- round((1-(MDC_Counting_CHO[2,1] / 5844))*100,digits = 2)
MDC_Counting_CHO[3,3] <- round((1-(MDC_Counting_CHO[3,1] / 5844))*100,digits = 2)
MDC_Counting_CHO[4,3] <- round((1-(MDC_Counting_CHO[4,1] / 5844))*100,digits = 2)
MDC_Counting_CHO[5,3] <- round((1-(MDC_Counting_CHO[5,1] / 5844))*100,digits = 2)
MDC_Counting_CHO[6,3] <- round((1-(MDC_Counting_CHO[6,1] / 5844))*100,digits = 2)

colnames(MDC_Counting_CHO) <- c("Number of 0 Days", "Total number of cases","Percent of Days with cases")
rownames(MDC_Counting_CHO) <- c("CHO_Mort","CHO_MDC01","CHO_MDC04","CHO_MDC05","CHO_MDC19","CHO_Non-Billable ICD")

EMV <- read_excel("~/Desktop/EMV.Final.Weather.xlsx",na="NA")
EMV <- EMV[1:5844, ]
#Convert Data to numeric
EMV %>% mutate_if(is.character, as.numeric)
#head(EMV)
for(i in 1:ncol(EMV)){
  assign(names(EMV)[i], EMV[[i]])
}

MDC_Counting_EMV <- data.frame(matrix(NA))

MDC_Counting_EMV[1,1] <- sum(EMV$Mort==0)
MDC_Counting_EMV[1,2] <- sum(EMV$Mort, na.rm = T)
MDC_Counting_EMV[2,1] <- sum(EMV$MDC.01==0)
MDC_Counting_EMV[2,2] <- sum(EMV$MDC.01, na.rm = T)
MDC_Counting_EMV[3,1] <- sum(EMV$MDC.04==0)
MDC_Counting_EMV[3,2] <- sum(EMV$MDC.04, na.rm = T)
MDC_Counting_EMV[4,1] <- sum(EMV$MDC.05==0)
MDC_Counting_EMV[4,2] <- sum(EMV$MDC.05, na.rm = T)
MDC_Counting_EMV[5,1] <- sum(EMV$MDC.19==0)
MDC_Counting_EMV[5,2] <- sum(EMV$MDC.19, na.rm = T)
MDC_Counting_EMV[6,1] <- sum(EMV$Non.billable.ICD==0)
MDC_Counting_EMV[6,2] <- sum(EMV$Non.billable.ICD, na.rm = T)

MDC_Counting_EMV[1,3] <- round((1-(MDC_Counting_EMV[1,1] / 5844))*100,digits = 2)
MDC_Counting_EMV[2,3] <- round((1-(MDC_Counting_EMV[2,1] / 5844))*100,digits = 2)
MDC_Counting_EMV[3,3] <- round((1-(MDC_Counting_EMV[3,1] / 5844))*100,digits = 2)
MDC_Counting_EMV[4,3] <- round((1-(MDC_Counting_EMV[4,1] / 5844))*100,digits = 2)
MDC_Counting_EMV[5,3] <- round((1-(MDC_Counting_EMV[5,1] / 5844))*100,digits = 2)
MDC_Counting_EMV[6,3] <- round((1-(MDC_Counting_EMV[6,1] / 5844))*100,digits = 2)

colnames(MDC_Counting_EMV) <- c("Number of 0 Days", "Total number of cases","Percent of Days with cases")
rownames(MDC_Counting_EMV) <- c("EMV_Mort","EMV_MDC01","EMV_MDC04","EMV_MDC05","EMV_MDC19","EMV_Non-Billable ICD")

EZF <- read_excel("~/Desktop/EZF.Final.Weather.xlsx",na="NA")
EZF <- EZF[1:5844, ]
#Convert Data to numeric
EZF %>% mutate_if(is.character, as.numeric)
#head(EZF)
for(i in 1:ncol(EZF)){
  assign(names(EZF)[i], EZF[[i]])
}

MDC_Counting_EZF <- data.frame(matrix(NA))

MDC_Counting_EZF[1,1] <- sum(EZF$Mort==0)
MDC_Counting_EZF[1,2] <- sum(EZF$Mort, na.rm = T)
MDC_Counting_EZF[2,1] <- sum(EZF$MDC.01==0)
MDC_Counting_EZF[2,2] <- sum(EZF$MDC.01, na.rm = T)
MDC_Counting_EZF[3,1] <- sum(EZF$MDC.04==0)
MDC_Counting_EZF[3,2] <- sum(EZF$MDC.04, na.rm = T)
MDC_Counting_EZF[4,1] <- sum(EZF$MDC.05==0)
MDC_Counting_EZF[4,2] <- sum(EZF$MDC.05, na.rm = T)
MDC_Counting_EZF[5,1] <- sum(EZF$MDC.19==0)
MDC_Counting_EZF[5,2] <- sum(EZF$MDC.19, na.rm = T)
MDC_Counting_EZF[6,1] <- sum(EZF$Non.billable.ICD==0)
MDC_Counting_EZF[6,2] <- sum(EZF$Non.billable.ICD, na.rm = T)

MDC_Counting_EZF[1,3] <- round((1-(MDC_Counting_EZF[1,1] / 5844))*100,digits = 2)
MDC_Counting_EZF[2,3] <- round((1-(MDC_Counting_EZF[2,1] / 5844))*100,digits = 2)
MDC_Counting_EZF[3,3] <- round((1-(MDC_Counting_EZF[3,1] / 5844))*100,digits = 2)
MDC_Counting_EZF[4,3] <- round((1-(MDC_Counting_EZF[4,1] / 5844))*100,digits = 2)
MDC_Counting_EZF[5,3] <- round((1-(MDC_Counting_EZF[5,1] / 5844))*100,digits = 2)
MDC_Counting_EZF[6,3] <- round((1-(MDC_Counting_EZF[6,1] / 5844))*100,digits = 2)

colnames(MDC_Counting_EZF) <- c("Number of 0 Days", "Total number of cases","Percent of Days with cases")
rownames(MDC_Counting_EZF) <- c("EZF_Mort","EZF_MDC01","EZF_MDC04","EZF_MDC05","EZF_MDC19","EZF_Non-Billable ICD")

IAD <- read_excel("~/Desktop/IAD.Final.Weather.xlsx",na="NA")
IAD <- IAD[1:5844, ]
#Convert Data to numeric
IAD %>% mutate_if(is.character, as.numeric)
#head(IAD)
for(i in 1:ncol(IAD)){
  assign(names(IAD)[i], IAD[[i]])
}

MDC_Counting_IAD <- data.frame(matrix(NA))

MDC_Counting_IAD[1,1] <- sum(IAD$Mort==0)
MDC_Counting_IAD[1,2] <- sum(IAD$Mort, na.rm = T)
MDC_Counting_IAD[2,1] <- sum(IAD$MDC.01==0)
MDC_Counting_IAD[2,2] <- sum(IAD$MDC.01, na.rm = T)
MDC_Counting_IAD[3,1] <- sum(IAD$MDC.04==0)
MDC_Counting_IAD[3,2] <- sum(IAD$MDC.04, na.rm = T)
MDC_Counting_IAD[4,1] <- sum(IAD$MDC.05==0)
MDC_Counting_IAD[4,2] <- sum(IAD$MDC.05, na.rm = T)
MDC_Counting_IAD[5,1] <- sum(IAD$MDC.19==0)
MDC_Counting_IAD[5,2] <- sum(IAD$MDC.19, na.rm = T)
MDC_Counting_IAD[6,1] <- sum(IAD$Non.billable.ICD==0)
MDC_Counting_IAD[6,2] <- sum(IAD$Non.billable.ICD, na.rm = T)

MDC_Counting_IAD[1,3] <- round((1-(MDC_Counting_IAD[1,1] / 5844))*100,digits = 2)
MDC_Counting_IAD[2,3] <- round((1-(MDC_Counting_IAD[2,1] / 5844))*100,digits = 2)
MDC_Counting_IAD[3,3] <- round((1-(MDC_Counting_IAD[3,1] / 5844))*100,digits = 2)
MDC_Counting_IAD[4,3] <- round((1-(MDC_Counting_IAD[4,1] / 5844))*100,digits = 2)
MDC_Counting_IAD[5,3] <- round((1-(MDC_Counting_IAD[5,1] / 5844))*100,digits = 2)
MDC_Counting_IAD[6,3] <- round((1-(MDC_Counting_IAD[6,1] / 5844))*100,digits = 2)

colnames(MDC_Counting_IAD) <- c("Number of 0 Days", "Total number of cases","Percent of Days with cases")
rownames(MDC_Counting_IAD) <- c("IAD_Mort","IAD_MDC01","IAD_MDC04","IAD_MDC05","IAD_MDC19","IAD_Non-Billable ICD")

LYH <- read_excel("~/Desktop/LYH.Final.Weather.xlsx",na="NA")
LYH <- LYH[1:5844, ]
#Convert Data to numeric
LYH %>% mutate_if(is.character, as.numeric)
#head(LYH)
for(i in 1:ncol(LYH)){
  assign(names(LYH)[i], LYH[[i]])
}

MDC_Counting_LYH <- data.frame(matrix(NA))

MDC_Counting_LYH[1,1] <- sum(LYH$Mort==0)
MDC_Counting_LYH[1,2] <- sum(LYH$Mort, na.rm = T)
MDC_Counting_LYH[2,1] <- sum(LYH$MDC.01==0)
MDC_Counting_LYH[2,2] <- sum(LYH$MDC.01, na.rm = T)
MDC_Counting_LYH[3,1] <- sum(LYH$MDC.04==0)
MDC_Counting_LYH[3,2] <- sum(LYH$MDC.04, na.rm = T)
MDC_Counting_LYH[4,1] <- sum(LYH$MDC.05==0)
MDC_Counting_LYH[4,2] <- sum(LYH$MDC.05, na.rm = T)
MDC_Counting_LYH[5,1] <- sum(LYH$MDC.19==0)
MDC_Counting_LYH[5,2] <- sum(LYH$MDC.19, na.rm = T)
MDC_Counting_LYH[6,1] <- sum(LYH$Non.billable.ICD==0)
MDC_Counting_LYH[6,2] <- sum(LYH$Non.billable.ICD, na.rm = T)

MDC_Counting_LYH[1,3] <- round((1-(MDC_Counting_LYH[1,1] / 5844))*100,digits = 2)
MDC_Counting_LYH[2,3] <- round((1-(MDC_Counting_LYH[2,1] / 5844))*100,digits = 2)
MDC_Counting_LYH[3,3] <- round((1-(MDC_Counting_LYH[3,1] / 5844))*100,digits = 2)
MDC_Counting_LYH[4,3] <- round((1-(MDC_Counting_LYH[4,1] / 5844))*100,digits = 2)
MDC_Counting_LYH[5,3] <- round((1-(MDC_Counting_LYH[5,1] / 5844))*100,digits = 2)
MDC_Counting_LYH[6,3] <- round((1-(MDC_Counting_LYH[6,1] / 5844))*100,digits = 2)

colnames(MDC_Counting_LYH) <- c("Number of 0 Days", "Total number of cases","Percent of Days with cases")
rownames(MDC_Counting_LYH) <- c("LYH_Mort","LYH_MDC01","LYH_MDC04","LYH_MDC05","LYH_MDC19","LYH_Non-Billable ICD")

OKV <- read_excel("~/Desktop/OKV.Final.Weather.xlsx",na="NA")
OKV <- OKV[1:5844, ]
#Convert Data to numeric
OKV %>% mutate_if(is.character, as.numeric)
#head(OKV)
for(i in 1:ncol(OKV)){
  assign(names(OKV)[i], OKV[[i]])
}

MDC_Counting_OKV <- data.frame(matrix(NA))

MDC_Counting_OKV[1,1] <- sum(OKV$Mort==0)
MDC_Counting_OKV[1,2] <- sum(OKV$Mort, na.rm = T)
MDC_Counting_OKV[2,1] <- sum(OKV$MDC.01==0)
MDC_Counting_OKV[2,2] <- sum(OKV$MDC.01, na.rm = T)
MDC_Counting_OKV[3,1] <- sum(OKV$MDC.04==0)
MDC_Counting_OKV[3,2] <- sum(OKV$MDC.04, na.rm = T)
MDC_Counting_OKV[4,1] <- sum(OKV$MDC.05==0)
MDC_Counting_OKV[4,2] <- sum(OKV$MDC.05, na.rm = T)
MDC_Counting_OKV[5,1] <- sum(OKV$MDC.19==0)
MDC_Counting_OKV[5,2] <- sum(OKV$MDC.19, na.rm = T)
MDC_Counting_OKV[6,1] <- sum(OKV$Non.billable.ICD==0)
MDC_Counting_OKV[6,2] <- sum(OKV$Non.billable.ICD, na.rm = T)

MDC_Counting_OKV[1,3] <- round((1-(MDC_Counting_OKV[1,1] / 5844))*100,digits = 2)
MDC_Counting_OKV[2,3] <- round((1-(MDC_Counting_OKV[2,1] / 5844))*100,digits = 2)
MDC_Counting_OKV[3,3] <- round((1-(MDC_Counting_OKV[3,1] / 5844))*100,digits = 2)
MDC_Counting_OKV[4,3] <- round((1-(MDC_Counting_OKV[4,1] / 5844))*100,digits = 2)
MDC_Counting_OKV[5,3] <- round((1-(MDC_Counting_OKV[5,1] / 5844))*100,digits = 2)
MDC_Counting_OKV[6,3] <- round((1-(MDC_Counting_OKV[6,1] / 5844))*100,digits = 2)

colnames(MDC_Counting_OKV) <- c("Number of 0 Days", "Total number of cases","Percent of Days with cases")
rownames(MDC_Counting_OKV) <- c("OKV_Mort","OKV_MDC01","OKV_MDC04","OKV_MDC05","OKV_MDC19","OKV_Non-Billable ICD")

ORF <- read_excel("~/Desktop/ORF.Final.Weather.xlsx",na="NA")
ORF <- ORF[1:5844, ]
#Convert Data to numeric
ORF %>% mutate_if(is.character, as.numeric)
#head(ORF)
for(i in 1:ncol(ORF)){
  assign(names(ORF)[i], ORF[[i]])
}

MDC_Counting_ORF <- data.frame(matrix(NA))

MDC_Counting_ORF[1,1] <- sum(ORF$Mort==0)
MDC_Counting_ORF[1,2] <- sum(ORF$Mort, na.rm = T)
MDC_Counting_ORF[2,1] <- sum(ORF$MDC.01==0)
MDC_Counting_ORF[2,2] <- sum(ORF$MDC.01, na.rm = T)
MDC_Counting_ORF[3,1] <- sum(ORF$MDC.04==0)
MDC_Counting_ORF[3,2] <- sum(ORF$MDC.04, na.rm = T)
MDC_Counting_ORF[4,1] <- sum(ORF$MDC.05==0)
MDC_Counting_ORF[4,2] <- sum(ORF$MDC.05, na.rm = T)
MDC_Counting_ORF[5,1] <- sum(ORF$MDC.19==0)
MDC_Counting_ORF[5,2] <- sum(ORF$MDC.19, na.rm = T)
MDC_Counting_ORF[6,1] <- sum(ORF$Non.billable.ICD==0)
MDC_Counting_ORF[6,2] <- sum(ORF$Non.billable.ICD, na.rm = T)

MDC_Counting_ORF[1,3] <- round((1-(MDC_Counting_ORF[1,1] / 5844))*100,digits = 2)
MDC_Counting_ORF[2,3] <- round((1-(MDC_Counting_ORF[2,1] / 5844))*100,digits = 2)
MDC_Counting_ORF[3,3] <- round((1-(MDC_Counting_ORF[3,1] / 5844))*100,digits = 2)
MDC_Counting_ORF[4,3] <- round((1-(MDC_Counting_ORF[4,1] / 5844))*100,digits = 2)
MDC_Counting_ORF[5,3] <- round((1-(MDC_Counting_ORF[5,1] / 5844))*100,digits = 2)
MDC_Counting_ORF[6,3] <- round((1-(MDC_Counting_ORF[6,1] / 5844))*100,digits = 2)

colnames(MDC_Counting_ORF) <- c("Number of 0 Days", "Total number of cases","Percent of Days with cases")
rownames(MDC_Counting_ORF) <- c("ORF_Mort","ORF_MDC01","ORF_MDC04","ORF_MDC05","ORF_MDC19","ORF_Non-Billable ICD")

PHF <- read_excel("~/Desktop/PHF.Final.Weather.xlsx",na="NA")
PHF <- PHF[1:5844, ]
#Convert Data to numeric
PHF %>% mutate_if(is.character, as.numeric)
#head(PHF)
for(i in 1:ncol(PHF)){
  assign(names(PHF)[i], PHF[[i]])
}

MDC_Counting_PHF <- data.frame(matrix(NA))

MDC_Counting_PHF[1,1] <- sum(PHF$Mort==0)
MDC_Counting_PHF[1,2] <- sum(PHF$Mort, na.rm = T)
MDC_Counting_PHF[2,1] <- sum(PHF$MDC.01==0)
MDC_Counting_PHF[2,2] <- sum(PHF$MDC.01, na.rm = T)
MDC_Counting_PHF[3,1] <- sum(PHF$MDC.04==0)
MDC_Counting_PHF[3,2] <- sum(PHF$MDC.04, na.rm = T)
MDC_Counting_PHF[4,1] <- sum(PHF$MDC.05==0)
MDC_Counting_PHF[4,2] <- sum(PHF$MDC.05, na.rm = T)
MDC_Counting_PHF[5,1] <- sum(PHF$MDC.19==0)
MDC_Counting_PHF[5,2] <- sum(PHF$MDC.19, na.rm = T)
MDC_Counting_PHF[6,1] <- sum(PHF$Non.billable.ICD==0)
MDC_Counting_PHF[6,2] <- sum(PHF$Non.billable.ICD, na.rm = T)

MDC_Counting_PHF[1,3] <- round((1-(MDC_Counting_PHF[1,1] / 5844))*100,digits = 2)
MDC_Counting_PHF[2,3] <- round((1-(MDC_Counting_PHF[2,1] / 5844))*100,digits = 2)
MDC_Counting_PHF[3,3] <- round((1-(MDC_Counting_PHF[3,1] / 5844))*100,digits = 2)
MDC_Counting_PHF[4,3] <- round((1-(MDC_Counting_PHF[4,1] / 5844))*100,digits = 2)
MDC_Counting_PHF[5,3] <- round((1-(MDC_Counting_PHF[5,1] / 5844))*100,digits = 2)
MDC_Counting_PHF[6,3] <- round((1-(MDC_Counting_PHF[6,1] / 5844))*100,digits = 2)

colnames(MDC_Counting_PHF) <- c("Number of 0 Days", "Total number of cases","Percent of Days with cases")
rownames(MDC_Counting_PHF) <- c("PHF_Mort","PHF_MDC01","PHF_MDC04","PHF_MDC05","PHF_MDC19","PHF_Non-Billable ICD")

RIC <- read_excel("~/Desktop/RIC.Final.Weather.xlsx",na="NA")
RIC <- RIC[1:5844, ]
#Convert Data to numeric
RIC %>% mutate_if(is.character, as.numeric)
#head(RIC)
for(i in 1:ncol(RIC)){
  assign(names(RIC)[i], RIC[[i]])
}

MDC_Counting_RIC <- data.frame(matrix(NA))

MDC_Counting_RIC[1,1] <- sum(RIC$Mort==0)
MDC_Counting_RIC[1,2] <- sum(RIC$Mort, na.rm = T)
MDC_Counting_RIC[2,1] <- sum(RIC$MDC.01==0)
MDC_Counting_RIC[2,2] <- sum(RIC$MDC.01, na.rm = T)
MDC_Counting_RIC[3,1] <- sum(RIC$MDC.04==0)
MDC_Counting_RIC[3,2] <- sum(RIC$MDC.04, na.rm = T)
MDC_Counting_RIC[4,1] <- sum(RIC$MDC.05==0)
MDC_Counting_RIC[4,2] <- sum(RIC$MDC.05, na.rm = T)
MDC_Counting_RIC[5,1] <- sum(RIC$MDC.19==0)
MDC_Counting_RIC[5,2] <- sum(RIC$MDC.19, na.rm = T)
MDC_Counting_RIC[6,1] <- sum(RIC$Non.billable.ICD==0)
MDC_Counting_RIC[6,2] <- sum(RIC$Non.billable.ICD, na.rm = T)

MDC_Counting_RIC[1,3] <- round((1-(MDC_Counting_RIC[1,1] / 5844))*100,digits = 2)
MDC_Counting_RIC[2,3] <- round((1-(MDC_Counting_RIC[2,1] / 5844))*100,digits = 2)
MDC_Counting_RIC[3,3] <- round((1-(MDC_Counting_RIC[3,1] / 5844))*100,digits = 2)
MDC_Counting_RIC[4,3] <- round((1-(MDC_Counting_RIC[4,1] / 5844))*100,digits = 2)
MDC_Counting_RIC[5,3] <- round((1-(MDC_Counting_RIC[5,1] / 5844))*100,digits = 2)
MDC_Counting_RIC[6,3] <- round((1-(MDC_Counting_RIC[6,1] / 5844))*100,digits = 2)

colnames(MDC_Counting_RIC) <- c("Number of 0 Days", "Total number of cases","Percent of Days with cases")
rownames(MDC_Counting_RIC) <- c("RIC_Mort","RIC_MDC01","RIC_MDC04","RIC_MDC05","RIC_MDC19","RIC_Non-Billable ICD")

ROA <- read_excel("~/Desktop/ROA.Final.Weather.xlsx",na="NA")
ROA <- ROA[1:5844, ]
#Convert Data to numeric
ROA %>% mutate_if(is.character, as.numeric)
#head(ROA)
for(i in 1:ncol(ROA)){
  assign(names(ROA)[i], ROA[[i]])
}

MDC_Counting_ROA <- data.frame(matrix(NA))

MDC_Counting_ROA[1,1] <- sum(ROA$Mort==0)
MDC_Counting_ROA[1,2] <- sum(ROA$Mort, na.rm = T)
MDC_Counting_ROA[2,1] <- sum(ROA$MDC.01==0)
MDC_Counting_ROA[2,2] <- sum(ROA$MDC.01, na.rm = T)
MDC_Counting_ROA[3,1] <- sum(ROA$MDC.04==0)
MDC_Counting_ROA[3,2] <- sum(ROA$MDC.04, na.rm = T)
MDC_Counting_ROA[4,1] <- sum(ROA$MDC.05==0)
MDC_Counting_ROA[4,2] <- sum(ROA$MDC.05, na.rm = T)
MDC_Counting_ROA[5,1] <- sum(ROA$MDC.19==0)
MDC_Counting_ROA[5,2] <- sum(ROA$MDC.19, na.rm = T)
MDC_Counting_ROA[6,1] <- sum(ROA$Non.billable.ICD==0)
MDC_Counting_ROA[6,2] <- sum(ROA$Non.billable.ICD, na.rm = T)

MDC_Counting_ROA[1,3] <- round((1-(MDC_Counting_ROA[1,1] / 5844))*100,digits = 2)
MDC_Counting_ROA[2,3] <- round((1-(MDC_Counting_ROA[2,1] / 5844))*100,digits = 2)
MDC_Counting_ROA[3,3] <- round((1-(MDC_Counting_ROA[3,1] / 5844))*100,digits = 2)
MDC_Counting_ROA[4,3] <- round((1-(MDC_Counting_ROA[4,1] / 5844))*100,digits = 2)
MDC_Counting_ROA[5,3] <- round((1-(MDC_Counting_ROA[5,1] / 5844))*100,digits = 2)
MDC_Counting_ROA[6,3] <- round((1-(MDC_Counting_ROA[6,1] / 5844))*100,digits = 2)

colnames(MDC_Counting_ROA) <- c("Number of 0 Days", "Total number of cases","Percent of Days with cases")
rownames(MDC_Counting_ROA) <- c("ROA_Mort","ROA_MDC01","ROA_MDC04","ROA_MDC05","ROA_MDC19","ROA_Non-Billable ICD")

SHD <- read_excel("~/Desktop/SHD.Final.Weather.xlsx",na="NA")
SHD <- SHD[1:5844, ]
#Convert Data to numeric
SHD %>% mutate_if(is.character, as.numeric)
#head(SHD)
for(i in 1:ncol(SHD)){
  assign(names(SHD)[i], SHD[[i]])
}

MDC_Counting_SHD <- data.frame(matrix(NA))

MDC_Counting_SHD[1,1] <- sum(SHD$Mort==0)
MDC_Counting_SHD[1,2] <- sum(SHD$Mort, na.rm = T)
MDC_Counting_SHD[2,1] <- sum(SHD$MDC.01==0)
MDC_Counting_SHD[2,2] <- sum(SHD$MDC.01, na.rm = T)
MDC_Counting_SHD[3,1] <- sum(SHD$MDC.04==0)
MDC_Counting_SHD[3,2] <- sum(SHD$MDC.04, na.rm = T)
MDC_Counting_SHD[4,1] <- sum(SHD$MDC.05==0)
MDC_Counting_SHD[4,2] <- sum(SHD$MDC.05, na.rm = T)
MDC_Counting_SHD[5,1] <- sum(SHD$MDC.19==0)
MDC_Counting_SHD[5,2] <- sum(SHD$MDC.19, na.rm = T)
MDC_Counting_SHD[6,1] <- sum(SHD$Non.billable.ICD==0)
MDC_Counting_SHD[6,2] <- sum(SHD$Non.billable.ICD, na.rm = T)

MDC_Counting_SHD[1,3] <- round((1-(MDC_Counting_SHD[1,1] / 5844))*100,digits = 2)
MDC_Counting_SHD[2,3] <- round((1-(MDC_Counting_SHD[2,1] / 5844))*100,digits = 2)
MDC_Counting_SHD[3,3] <- round((1-(MDC_Counting_SHD[3,1] / 5844))*100,digits = 2)
MDC_Counting_SHD[4,3] <- round((1-(MDC_Counting_SHD[4,1] / 5844))*100,digits = 2)
MDC_Counting_SHD[5,3] <- round((1-(MDC_Counting_SHD[5,1] / 5844))*100,digits = 2)
MDC_Counting_SHD[6,3] <- round((1-(MDC_Counting_SHD[6,1] / 5844))*100,digits = 2)

colnames(MDC_Counting_SHD) <- c("Number of 0 Days", "Total number of cases","Percent of Days with cases")
rownames(MDC_Counting_SHD) <- c("SHD_Mort","SHD_MDC01","SHD_MDC04","SHD_MDC05","SHD_MDC19","SHD_Non-Billable ICD")

VJI <- read_excel("~/Desktop/VJI.Final.Weather.xlsx",na="NA")
VJI <- VJI[1:5844, ]
#Convert Data to numeric
VJI %>% mutate_if(is.character, as.numeric)
#head(VJI)
for(i in 1:ncol(VJI)){
  assign(names(VJI)[i], VJI[[i]])
}

MDC_Counting_VJI <- data.frame(matrix(NA))

MDC_Counting_VJI[1,1] <- sum(VJI$Mort==0)
MDC_Counting_VJI[1,2] <- sum(VJI$Mort, na.rm = T)
MDC_Counting_VJI[2,1] <- sum(VJI$MDC.01==0)
MDC_Counting_VJI[2,2] <- sum(VJI$MDC.01, na.rm = T)
MDC_Counting_VJI[3,1] <- sum(VJI$MDC.04==0)
MDC_Counting_VJI[3,2] <- sum(VJI$MDC.04, na.rm = T)
MDC_Counting_VJI[4,1] <- sum(VJI$MDC.05==0)
MDC_Counting_VJI[4,2] <- sum(VJI$MDC.05, na.rm = T)
MDC_Counting_VJI[5,1] <- sum(VJI$MDC.19==0)
MDC_Counting_VJI[5,2] <- sum(VJI$MDC.19, na.rm = T)
MDC_Counting_VJI[6,1] <- sum(VJI$Non.billable.ICD==0)
MDC_Counting_VJI[6,2] <- sum(VJI$Non.billable.ICD, na.rm = T)

MDC_Counting_VJI[1,3] <- round((1-(MDC_Counting_VJI[1,1] / 5844))*100,digits = 2)
MDC_Counting_VJI[2,3] <- round((1-(MDC_Counting_VJI[2,1] / 5844))*100,digits = 2)
MDC_Counting_VJI[3,3] <- round((1-(MDC_Counting_VJI[3,1] / 5844))*100,digits = 2)
MDC_Counting_VJI[4,3] <- round((1-(MDC_Counting_VJI[4,1] / 5844))*100,digits = 2)
MDC_Counting_VJI[5,3] <- round((1-(MDC_Counting_VJI[5,1] / 5844))*100,digits = 2)
MDC_Counting_VJI[6,3] <- round((1-(MDC_Counting_VJI[6,1] / 5844))*100,digits = 2)

colnames(MDC_Counting_VJI) <- c("Number of 0 Days", "Total number of cases","Percent of Days with cases")
rownames(MDC_Counting_VJI) <- c("VJI_Mort","VJI_MDC01","VJI_MDC04","VJI_MDC05","VJI_MDC19","VJI_Non-Billable ICD")
