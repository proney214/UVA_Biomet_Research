
library(readxl)
library(zoo)

ROA <- read_excel("~/Desktop/Wx_Station_MDC_Groupings.xlsx", 
                                       sheet = "ROA", range = "A1:AD5845")
ROA <- data.frame(ROA)

ROA$index <- seq(1,length(ROA$Date),1)

for (i in (3:23)) {
  print(i)
  png(file = paste0("MDC_ROA_Cat_", i-2, ".png"))
  plot(ROA[,i],
       main = paste0("ROA ACME MDC", i-2),
       ylab = paste0("ROA MDC", i-2,"Daily Mortality"))
  lines(rollmean(ROA[,i],k=21), col = 'green')
  abline(v=seq(0,5844,by = 365))
  dev.off()
} 

png(file = paste0("MDC_ROA_Cat_", 23 , ".png"))
plot(ROA[,24],
     main = paste0("ROA ACME MDC", 23),
     ylab = paste0("ROA MDC", 23,"Daily Mortality"))
lines(rollmean(ROA[,24],k=21), col = 'green')
abline(v=seq(0,5844,by = 365))
dev.off()

png(file = paste0("MDC_ROA_Cat_25.png"))
plot(ROA[,25],
     main = paste0("ROA ACME MDC25"),
     ylab = paste0("ROA MDC25 Daily Mortality"))
lines(rollmean(ROA[,25],k=21), col = 'green')
abline(v=seq(0,5844,by = 365))
dev.off()


png(file = paste0("MDC_ROA_Cancer.png"))
plot(ROA[,26],
    main = paste0("ROA ACME MDC Cancer"),
    ylab = paste0("ROA MDC Cancer Daily Mortality"))
    lines(rollmean(ROA[,26],k=21), col = 'green')
    abline(v=seq(0,5844,by = 365))
    dev.off()
 
png(file = paste0("MDC_ROA_Nonbillable.png"))
plot(ROA[,27],
         main = paste0("ROA ACME MDC Nonbillable"),
                       ylab = paste0("ROA MDC Nonbillable Daily Mortality"))
         lines(rollmean(ROA[,27],k=21), col = 'green')
         abline(v=seq(0,5844,by = 365))
         dev.off()
 
png(file = paste0("MDC_ROA_Daily.png"))
plot(ROA[,30],
    main = paste0("ROA ACME MDC Daily"),
    ylab = paste0("ROA MDC", i-2,"Daily Mortality"))
    lines(rollmean(ROA[,30],k=21), col = 'green')
    abline(v=seq(0,5844,by = 365))
    dev.off()
