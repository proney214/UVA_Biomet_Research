library(readxl)
library(zoo)

CHO <- read_excel("~/Desktop/CHO.Final.Weather.xlsx",na="NA")
CHO <- CHO[1:5844,]
EMV <- read_excel("~/Desktop/EMV.Final.Weather.xlsx",na="NA")
EZF <- read_excel("~/Desktop/EZF.Final.Weather.xlsx",na="NA")
IAD <- read_excel("~/Desktop/IAD.Final.Weather.xlsx",na="NA")
LYH <- read_excel("~/Desktop/LYH.Final.Weather.xlsx",na="NA")
OKV <- read_excel("~/Desktop/OKV.Final.Weather.xlsx",na="NA")
ORF <- read_excel("~/Desktop/ORF.Final.Weather.xlsx",na="NA")
PHF <- read_excel("~/Desktop/PHF.Final.Weather.xlsx",na="NA")
RIC <- read_excel("~/Desktop/RIC.Final.Weather.xlsx",na="NA")
ROA <- read_excel("~/Desktop/ROA.Final.Weather.xlsx",na="NA")
SHD <- read_excel("~/Desktop/SHD.Final.Weather.xlsx",na="NA")
VJI <- read_excel("~/Desktop/VJI.Final.Weather.xlsx",na="NA")

plot(SHD$Mort, type= 'l', col = 'dark grey',  xlab = "Date",ylab = "Daily Mortality",
     main = "SHD",      
     xaxt = "n")
axis(1, at = c(seq(1,5845, by = 365*5)),
     labels = c("2005", "2010","2015","2020"))
lines(rollmean(SHD$Mort,k=21), col = 'green')
abline(v=seq(0,5844,by = 365))

