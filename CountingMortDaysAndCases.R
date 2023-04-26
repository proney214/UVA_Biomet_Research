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