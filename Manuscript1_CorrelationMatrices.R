## Correlation matrices for best models

CHO <- read_excel("~/Desktop/CHO.Final.Weather.xlsx",na="NA")
CHO <- CHO[1:5844, ]
CHOtest <- data.frame(cbind(CHO$`MaxT(C)`,CHO$`MinT(C)`,CHO$`AT(C)1pm`,CHO$`AT(C)7pm`,CHO$`Td(C)1pm`,CHO$`Td(C)7pm`))

CHOfinal<-data.frame(cor(CHOtest,method = "pearson", use = "complete.obs"))
CHOfinal <- round(CHOfinal, digits=2)
CHOfinal <- (CHOfinal)
rownames(CHOfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")
colnames(CHOfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")


EMV <- read_excel("~/Desktop/EMV.Final.Weather.xlsx",na="NA")
EMV <- EMV[1:5844, ]
EMVtest <- data.frame(cbind(EMV$`MaxT(C)`,EMV$`MinT(C)`,EMV$`AT(C)1pm`,EMV$`AT(C)7pm`,EMV$`Td(C)1pm`,EMV$`Td(C)7pm`))

EMVfinal<-data.frame(cor(EMVtest,method = "pearson", use = "complete.obs"))
EMVfinal <- round(EMVfinal, digits=2)
EMVfinal <- (EMVfinal)
rownames(EMVfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")
colnames(EMVfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")


EZF <- read_excel("~/Desktop/EZF.Final.Weather.xlsx",na="NA")
EZF <- EZF[1:5844, ]
EZFtest <- data.frame(cbind(EZF$`MaxT(C)`,EZF$`MinT(C)`,EZF$`AT(C)1pm`,EZF$`AT(C)7pm`,EZF$`Td(C)1pm`,EZF$`Td(C)7pm`))

EZFfinal<-data.frame(cor(EZFtest,method = "pearson", use = "complete.obs"))
EZFfinal <- round(EZFfinal, digits=2)
EZFfinal <- (EZFfinal)
rownames(EZFfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")
colnames(EZFfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")

IAD <- read_excel("~/Desktop/IAD.Final.Weather.xlsx",na="NA")
IAD <- IAD[1:5844, ]
IADtest <- data.frame(cbind(IAD$`MaxT(C)`,IAD$`MinT(C)`,IAD$`AT(C)1pm`,IAD$`AT(C)7pm`,IAD$`Td(C)1pm`,IAD$`Td(C)7pm`))

IADfinal<-data.frame(cor(IADtest,method = "pearson", use = "complete.obs"))
IADfinal <- round(IADfinal, digits=2)
IADfinal <- (IADfinal)
rownames(IADfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")
colnames(IADfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")

LYH <- read_excel("~/Desktop/LYH.Final.Weather.xlsx",na="NA")
LYH <- LYH[1:5844, ]
LYHtest <- data.frame(cbind(LYH$`MaxT(C)`,LYH$`MinT(C)`,LYH$`AT(C)1pm`,LYH$`AT(C)7pm`,LYH$`Td(C)1pm`,LYH$`Td(C)7pm`))

LYHfinal<-data.frame(cor(LYHtest,method = "pearson", use = "complete.obs"))
LYHfinal <- round(LYHfinal, digits=2)
LYHfinal <- (LYHfinal)
rownames(LYHfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")
colnames(LYHfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")


OKV <- read_excel("~/Desktop/OKV.Final.Weather.xlsx",na="NA")
OKV <- OKV[1:5844, ]
OKVtest <- data.frame(cbind(OKV$`MaxT(C)`,OKV$`MinT(C)`,OKV$`AT(C)1pm`,OKV$`AT(C)7pm`,OKV$`Td(C)1pm`,OKV$`Td(C)7pm`))

OKVfinal<-data.frame(cor(OKVtest,method = "pearson", use = "complete.obs"))
OKVfinal <- round(OKVfinal, digits=2)
OKVfinal <- (OKVfinal)
rownames(OKVfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")
colnames(OKVfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")

ORF <- read_excel("~/Desktop/ORF.Final.Weather.xlsx",na="NA")
ORF <- ORF[1:5844, ]
ORFtest <- data.frame(cbind(ORF$`MaxT(C)`,ORF$`MinT(C)`,ORF$`AT(C)1pm`,ORF$`AT(C)7pm`,ORF$`Td(C)1pm`,ORF$`Td(C)7pm`))

ORFfinal<-data.frame(cor(ORFtest,method = "pearson", use = "complete.obs"))
ORFfinal <- round(ORFfinal, digits=2)
ORFfinal <- (ORFfinal)
rownames(ORFfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")
colnames(ORFfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")

PHF <- read_excel("~/Desktop/PHF.Final.Weather.xlsx",na="NA")
PHF <- PHF[1:5844, ]
PHFtest <- data.frame(cbind(PHF$`MaxT(C)`,PHF$`MinT(C)`,PHF$`AT(C)1pm`,PHF$`AT(C)7pm`,PHF$`Td(C)1pm`,PHF$`Td(C)7pm`))

PHFfinal<-data.frame(cor(PHFtest,method = "pearson", use = "complete.obs"))
PHFfinal <- round(PHFfinal, digits=2)
PHFfinal <- (PHFfinal)
rownames(PHFfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")
colnames(PHFfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")

RIC <- read_excel("~/Desktop/RIC.Final.Weather.xlsx",na="NA")
RIC <- RIC[1:5844, ]
RICtest <- data.frame(cbind(RIC$`MaxT(C)`,RIC$`MinT(C)`,RIC$`AT(C)1pm`,RIC$`AT(C)7pm`,RIC$`Td(C)1pm`,RIC$`Td(C)7pm`))

RICfinal<-data.frame(cor(RICtest,method = "pearson", use = "complete.obs"))
RICfinal <- round(RICfinal, digits=2)
RICfinal <- (RICfinal)
rownames(RICfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")
colnames(RICfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")

ROA <- read_excel("~/Desktop/ROA.Final.Weather.xlsx",na="NA")
ROA <- ROA[1:5844, ]
ROAtest <- data.frame(cbind(ROA$`MaxT(C)`,ROA$`MinT(C)`,ROA$`AT(C)1pm`,ROA$`AT(C)7pm`,ROA$`Td(C)1pm`,ROA$`Td(C)7pm`))

ROAfinal<-data.frame(cor(ROAtest,method = "pearson", use = "complete.obs"))
ROAfinal <- round(ROAfinal, digits=2)
ROAfinal <- (ROAfinal)
rownames(ROAfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")
colnames(ROAfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")

SHD <- read_excel("~/Desktop/SHD.Final.Weather.xlsx",na="NA")
SHD <- SHD[1:5844, ]
SHDtest <- data.frame(cbind(SHD$`MaxT(C)`,SHD$`MinT(C)`,SHD$`AT(C)1pm`,SHD$`AT(C)7pm`,SHD$`Td(C)1pm`,SHD$`Td(C)7pm`))

SHDfinal<-data.frame(cor(SHDtest,method = "pearson", use = "complete.obs"))
SHDfinal <- round(SHDfinal, digits=2)
SHDfinal <- (SHDfinal)
rownames(SHDfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")
colnames(SHDfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")

VJI <- read_excel("~/Desktop/VJI.Final.Weather.xlsx",na="NA")
VJI <- VJI[1:5844, ]
VJItest <- data.frame(cbind(VJI$`MaxT(C)`,VJI$`MinT(C)`,VJI$`AT(C)1pm`,VJI$`AT(C)7pm`,VJI$`Td(C)1pm`,VJI$`Td(C)7pm`))

VJIfinal<-data.frame(cor(VJItest,method = "pearson", use = "complete.obs"))
VJIfinal <- round(VJIfinal, digits=2)
VJIfinal <- (VJIfinal)
rownames(VJIfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")
colnames(VJIfinal) <- c("MaxT","MinT", "AT1pm","AT7pm","Td1pm","Td7pm")