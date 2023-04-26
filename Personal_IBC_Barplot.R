library(dplyr)

VAtotalMort <- sum(VA$Mort)
VAtotalMDC01 <- sum(VA$MDC01)
VAtotalMDC04 <- sum(VA$MDC04)
VAtotalMDC05 <- sum(VA$MDC05)
VAtotalMDC19 <- sum(VA$MDC19)
VAtotalMDCNonBillICD <- sum(VA$NonBillICD)

VAMiddle <- VA[quantile(MaxTC, probs = c(0.10),na.rm=T) < MaxTC & MaxTC< quantile(MaxTC, probs = c(0.90),na.rm=T), ]


#VAMortMiddle <- VA %>%
 # filter(MaxTC > quantile(MaxTC, probs = c(0.10),na.rm=T), MaxTC < quantile(MaxTC, probs = c(0.90),na.rm=T))
         
VAMiddleMort <- sum(VAMortMiddle$Mort)
VAMiddleMDC01 <- sum(VAMortMiddle$MDC01)
VAMiddleMDC04 <- sum(VAMortMiddle$MDC04)
VAMiddleMDC05 <- sum(VAMortMiddle$MDC05)
VAMiddleMDC19 <- sum(VAMortMiddle$MDC19)
VAMiddleMDCNonBillICD <- sum(VAMortMiddle$NonBillICD)

VAMortHot <- VA %>%
  filter(MaxTC >= quantile(MaxTC, probs = c(0.90),na.rm=T))
VAHotMort <- sum(VAMortHot$Mort)
VAHotMDC01 <- sum(VAMortHot$MDC01)
VAHotMDC04 <- sum(VAMortHot$MDC04)
VAHotMDC05 <- sum(VAMortHot$MDC05)
VAHotMDC19 <- sum(VAMortHot$MDC19)
VAHotMDCNonBillICD <- sum(VAMortHot$NonBillICD)

VAMortCold <- VA %>%
  filter(MaxTC <= quantile(MaxTC, probs = c(0.10),na.rm=T))
VAColdMort <- sum(VAMortCold$Mort)
VAColdMDC01 <- sum(VAMortCold$MDC01)
VAColdMDC04 <- sum(VAMortCold$MDC04)
VAColdMDC05 <- sum(VAMortCold$MDC05)
VAColdMDC19 <- sum(VAMortCold$MDC19)
VAColdMDCNonBillICD <- sum(VAMortCold$NonBillICD)

sum(VAMiddleMort,VAHotMort,VAColdMort)
sum(VA$Mort)

VAMortData <- data.frame(Total = c(VAMiddleMort,VAHotMort,VAColdMort),
                         Cardiovascular = c(VAMiddleMDC05,VAHotMDC05,VAColdMDC05),
                         Respiratory = c(VAMiddleMDC04,VAHotMDC04,VAColdMDC04),
                         Nerouvs = c(VAMiddleMDC01,VAHotMDC01,VAColdMDC01),
                         Mental = c(VAMiddleMDC19,VAHotMDC19,VAColdMDC19),
                         NonBillable = c(VAMiddleMDCNonBillICD,VAHotMDCNonBillICD,VAColdMDCNonBillICD))
rownames(VAMortData) <- c("Middle","Hot10%","Cold10%")

VAMortDataMatrix <- as.matrix(VAMortData)

barplot(VAMortDataMatrix, horiz = T,#beside = TRUE,
        main = "Virginia Mortality Breakdown",
        xlab = "Mortality Count",
        col = c("green","red","blue")
)
legend("topright",
       c("Coldest 10%","Warmest 10%","Middle"),
       fill = c("blue","red","green")
)

