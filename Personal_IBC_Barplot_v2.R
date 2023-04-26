#Attempt 2

VA_NasRem <- VA[!is.na(VA$`MaxT(C)`),]
VA_NasRem$`MaxT(C)`

VAMortMiddle <- VA_NasRem[quantile(VA_NasRem$`MaxT(C)`, probs = c(0.10),na.rm=T) < VA_NasRem$`MaxT(C)` & VA_NasRem$`MaxT(C)`< quantile(VA_NasRem$`MaxT(C)`, probs = c(0.90),na.rm=T), ]
VAMiddleMort <- sum(VAMortMiddle$Mort,na.rm=T)
VAMiddleMDC01 <- sum(VAMortMiddle$MDC01,na.rm=T)
VAMiddleMDC04 <- sum(VAMortMiddle$MDC04,na.rm=T)
VAMiddleMDC05 <- sum(VAMortMiddle$MDC05,na.rm=T)
VAMiddleMDC19 <- sum(VAMortMiddle$MDC19,na.rm=T)
VAMiddleMDCNonBillICD <- sum(VAMortMiddle$NonBillICD,na.rm=T)

VAMortHot <- VA_NasRem[quantile(VA_NasRem$`MaxT(C)`, probs = c(0.90),na.rm=T) <= VA_NasRem$`MaxT(C)`, ]

VAHotMort <- sum(VAMortHot$Mort,na.rm=T)
VAHotMDC01 <- sum(VAMortHot$MDC01,na.rm=T)
VAHotMDC04 <- sum(VAMortHot$MDC04,na.rm=T)
VAHotMDC05 <- sum(VAMortHot$MDC05,na.rm=T)
VAHotMDC19 <- sum(VAMortHot$MDC19,na.rm=T)
VAHotMDCNonBillICD <- sum(VAMortHot$NonBillICD,na.rm=T)

VAMortCold <- VA_NasRem[quantile(VA_NasRem$`MaxT(C)`, probs = c(0.10),na.rm=T) >= VA_NasRem$`MaxT(C)`, ]

VAColdMort <- sum(VAMortCold$Mort,na.rm=T)
VAColdMDC01 <- sum(VAMortCold$MDC01,na.rm=T)
VAColdMDC04 <- sum(VAMortCold$MDC04,na.rm=T)
VAColdMDC05 <- sum(VAMortCold$MDC05,na.rm=T)
VAColdMDC19 <- sum(VAMortCold$MDC19,na.rm=T)
VAColdMDCNonBillICD <- sum(VAMortCold$NonBillICD,na.rm=T)

sum(VAMiddleMort,VAHotMort,VAColdMort)
sum(VA_NasRem$Mort)

VAMortData <- data.frame(Total = c(VAColdMort,VAMiddleMort,VAHotMort),
                         Cardiovascular = c(VAColdMDC05, VAMiddleMDC05,VAHotMDC05),
                         Respiratory = c(VAColdMDC04,VAMiddleMDC04,VAHotMDC04),
                         Nerouvs = c(VAColdMDC01,VAMiddleMDC01,VAHotMDC01),
                         Mental = c(VAColdMDC19,VAMiddleMDC19,VAHotMDC19),
                         NonBillable = c(VAColdMDCNonBillICD,VAMiddleMDCNonBillICD,VAHotMDCNonBillICD))
rownames(VAMortData) <- c("Cold10%","Middle80%","Hot10%")

VAMortDataMatrix <- as.matrix(VAMortData)

barplot(VAMortDataMatrix, horiz = T,#beside = TRUE,
        main = "Virginia Mortality Breakdown",
        xlab = "Mortality Count",
        col = c("blue","grey","red")
)
legend("topright",
       c("Coldest 10%","Middle 80%","Warmest 10%"),
       fill = c("blue","grey","red")
)

#Percentages

VAMortDataPercents <- data.frame(Total = c(VAColdMort/sum(VA_NasRem$Mort),VAMiddleMort/sum(VA_NasRem$Mort),VAHotMort/sum(VA_NasRem$Mort)),
                         Cardiovascular = c(VAColdMDC05/sum(VA_NasRem$MDC05), VAMiddleMDC05/sum(VA_NasRem$MDC05),VAHotMDC05/sum(VA_NasRem$MDC05)),
                         Respiratory = c(VAColdMDC04/sum(VA_NasRem$MDC04),VAMiddleMDC04/sum(VA_NasRem$MDC04),VAHotMDC04/sum(VA_NasRem$MDC04)),
                         Nerouvs = c(VAColdMDC01/sum(VA_NasRem$MDC01),VAMiddleMDC01/sum(VA_NasRem$MDC01),VAHotMDC01/sum(VA_NasRem$MDC01)),
                         Mental = c(VAColdMDC19/sum(VA_NasRem$MDC19),VAMiddleMDC19/sum(VA_NasRem$MDC19),VAHotMDC19/sum(VA_NasRem$MDC19)),
                         NonBillable = c(VAColdMDCNonBillICD/sum(VA_NasRem$NonBillICD),VAMiddleMDCNonBillICD/sum(VA_NasRem$NonBillICD),VAHotMDCNonBillICD/sum(VA_NasRem$NonBillICD)))
rownames(VAMortDataPercents) <- c("Cold10%","Middle80%","Hot10%")

VAMortDataPercentsMatrix <- as.matrix(VAMortDataPercents)

barplot(VAMortDataPercentsMatrix, horiz = T,#beside = TRUE,
        main = "Virginia Mortality Breakdown",
        xlab = "Mortality Count",
        col = c("blue","grey","red")
)
legend("topright",
       c("Coldest 10%","Middle 80%","Warmest 10%"),
       fill = c("blue","grey","red")
)

#Percents No Middle
VAMortDataPercents_NM <- data.frame(Total = c(VAColdMort/sum(VA_NasRem$Mort),VAHotMort/sum(VA_NasRem$Mort)),
                                 Cardiovascular = c(VAColdMDC05/sum(VA_NasRem$MDC05),VAHotMDC05/sum(VA_NasRem$MDC05)),
                                 Respiratory = c(VAColdMDC04/sum(VA_NasRem$MDC04),VAHotMDC04/sum(VA_NasRem$MDC04)),
                                 Nerouvs = c(VAColdMDC01/sum(VA_NasRem$MDC01),VAHotMDC01/sum(VA_NasRem$MDC01)),
                                 Mental = c(VAColdMDC19/sum(VA_NasRem$MDC19),VAHotMDC19/sum(VA_NasRem$MDC19)),
                                 NonBillable = c(VAColdMDCNonBillICD/sum(VA_NasRem$NonBillICD),VAHotMDCNonBillICD/sum(VA_NasRem$NonBillICD)))
rownames(VAMortDataPercents_NM) <- c("Cold10%","Hot10%")

VAMortDataPercentsMatrix_NM <- as.matrix(VAMortDataPercents_NM)

barplot(VAMortDataPercentsMatrix_NM, beside = T,#horiz = T,#beside = TRUE,
        main = "Virginia Mortality Breakdown",
        xlab = "Mortality Type",
        ylab = "Percentage of Total",
        col = c("blue","red")
)
legend("topright",
       c("Coldest 10%","Warmest 10%"),
       fill = c("blue","red")
)
