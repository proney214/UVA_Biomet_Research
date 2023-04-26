#State Data Generation

library(readxl)

CHO <- read_excel("~/Desktop/CHO.Final.Weather.xlsx",na="NA")
CHO <- CHO[1:5844, ]
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

test <- rbind(CHO,EMV,EZF,IAD,LYH,OKV,ORF,PHF,RIC,ROA,SHD,VJI)

test <- select(test,-Station,-Date,-Year,-Month,-Day)
test %>% mutate_if(is.character,is.numeric)

CHO <- test[1:5844, ]
EMV <- test[5845:11688, ]
EZF <- test[11689:17532, ]
IAD <- test[17533:23376, ]
LYH <- test[23377:29220, ]
OKV <- test[29221:35064, ]
ORF <- test[35065:40908, ]
PHF <- test[40909:46752, ]
RIC <- test[46753:52596, ]
ROA <- test[52597:58440, ]
SHD <- test[58441:64284, ]
VJI <- test[64285:70128, ]

#for the mort portion below, change What you want to adjust the values to by 
# doing a command F to replace "$XXXX" with "$YYYY" and then run the rest of the code
# Make sure to adjust your 

CHO$White

CHOmort <- data.frame(CHO$White)
EMVmort <- data.frame(EMV$White)
EZFmort <- data.frame(EZF$White)
IADmort <- data.frame(IAD$White)
LYHmort <- data.frame(LYH$White)
OKVmort <- data.frame(OKV$White)
ORFmort <- data.frame(ORF$White)
PHFmort <- data.frame(PHF$White)
RICmort <- data.frame(RIC$White)
ROAmort <- data.frame(ROA$White)
SHDmort <- data.frame(SHD$White)
VJImort <- data.frame(VJI$White)

mort <- cbind(CHOmort,EMVmort,EZFmort,IADmort,LYHmort,OKVmort,ORFmort,PHFmort,RICmort,ROAmort,SHDmort,VJImort)
mortrowsum <- data.frame(rowSums(mort))

#Run Calculations for General Mortality

CHOtotal <- data.frame(mapply('*',CHO,CHOmort))
EMVtotal <- data.frame(mapply('*',EMV,EMVmort))
EZFtotal <- data.frame(mapply('*',EZF,EZFmort))
IADtotal <- data.frame(mapply('*',IAD,IADmort))
LYHtotal <- data.frame(mapply('*',LYH,LYHmort))
OKVtotal <- data.frame(mapply('*',OKV,OKVmort))
ORFtotal <- data.frame(mapply('*',ORF,ORFmort))
PHFtotal <- data.frame(mapply('*',PHF,PHFmort))
RICtotal <- data.frame(mapply('*',RIC,RICmort))
ROAtotal <- data.frame(mapply('*',ROA,ROAmort))
SHDtotal <- data.frame(mapply('*',SHD,SHDmort))
VJItotal <- data.frame(mapply('*',VJI,VJImort))

bigtotal <- CHOtotal+EMVtotal+EZFtotal+IADtotal+LYHtotal+OKVtotal+ORFtotal+PHFtotal+RICtotal+ROAtotal+SHDtotal+VJItotal

finaldf <- data.frame(mapply('/',bigtotal,mortrowsum))

write.csv(finaldf,"StateWeatherPrepAdjusted_White_Update_Total.csv",row.names = FALSE)



############# PLOTS for max min temperature

CHO <- read_excel("~/Desktop/CHO.Final.Weather.xlsx",na="NA")
CHO <- CHO[1:5844, ]
CHO %>% mutate_if(is.character,is.numeric)

EMV <- read_excel("~/Desktop/EMV.Final.Weather.xlsx",na="NA")
EMV %>% mutate_if(is.character,is.numeric)

EZF <- read_excel("~/Desktop/EZF.Final.Weather.xlsx",na="NA")
EZF %>% mutate_if(is.character,is.numeric)

IAD <- read_excel("~/Desktop/IAD.Final.Weather.xlsx",na="NA")
IAD %>% mutate_if(is.character,is.numeric)

LYH <- read_excel("~/Desktop/LYH.Final.Weather.xlsx",na="NA")
LYH %>% mutate_if(is.character,is.numeric)

OKV <- read_excel("~/Desktop/OKV.Final.Weather.xlsx",na="NA")
OKV %>% mutate_if(is.character,is.numeric)

ORF <- read_excel("~/Desktop/ORF.Final.Weather.xlsx",na="NA")
ORF %>% mutate_if(is.character,is.numeric)

PHF <- read_excel("~/Desktop/PHF.Final.Weather.xlsx",na="NA")
PHF %>% mutate_if(is.character,is.numeric)

RIC <- read_excel("~/Desktop/RIC.Final.Weather.xlsx",na="NA")
RIC %>% mutate_if(is.character,is.numeric)

ROA <- read_excel("~/Desktop/ROA.Final.Weather.xlsx",na="NA")
ROA %>% mutate_if(is.character,is.numeric)

SHD <- read_excel("~/Desktop/SHD.Final.Weather.xlsx",na="NA")
SHD %>% mutate_if(is.character,is.numeric)

VJI <- read_excel("~/Desktop/VJI.Final.Weather.xlsx",na="NA")
VJI %>% mutate_if(is.character,is.numeric)

test <- rbind(CHO,EMV,EZF,IAD,LYH,OKV,ORF,PHF,RIC,ROA,SHD,VJI)
max(test$`MaxT(C)`,na.rm = TRUE) #41.1 C
min(test$`MaxT(C)`,na.rm = TRUE) #-12.8 C
mean(test$`MaxT(C)`,na.rm = TRUE) #20.2 C

## Plots

df <- data.frame(time = VJI$Date,
                 Maximum = VJI$`MaxT(C)`,
                 Minimum = VJI$`MinT(C)`)
df <- melt(df ,  id.vars = 'time', variable.name = 'series')

# plot on same grid, each series colored differently --  Need k = 260
# good if the series have same scale
ggplot(df, aes(time,value)) + 
  geom_line(aes(colour = series)) +
  #geom_smooth(aes(colour = series), se = F, 
  #          method = "gam", formula = y ~ s(x, bs = "cs", fx = TRUE, k = 200))+
  scale_color_manual(values=c('Grey','Blue')) +
  xlab("Year") + ylab("Temperature (C)") +
  #ylim(0,25) +
  #scale_colour_grey(start = 0, end = .5) +
  theme(legend.position="right",
        legend.title=element_blank())+
  ggtitle("VJI Temperatures") 

plot(VJI$`MaxT(C)`, type= 'l', col = 'dark grey',  xlab = "Date",ylab = "Temperature (\u00B0C)",
     main = "VJI",      
     xaxt = "n",
     ylim = c(-10,40))
axis(1, at = c(seq(1,5845, by = 365*5)),
     labels = c("2005", "2010","2015","2020"))
lines(VJI$`MinT(C)`, type= 'l', col = 'light grey')
lines(rollmean(VJI$`MaxT(C)`,k=21), col = 'red')
lines(rollmean(VJI$`MinT(C)`,k=21), col = 'blue')
abline(v=seq(0,5844,by = 365))


#Fix Tick Marks
#Create Keys

plot(CHO$Mort, type= 'l', col = 'dark grey',  xlab = "Date",ylab = "Daily Mortality",
     main = "CHO",      
     xaxt = "n")
axis(1, at = c(seq(1,5845, by = 365*5)),
     labels = c("2005", "2010","2015","2020"))
lines(rollmean(CHO$Mort,k=21), col = 'green')
abline(v=seq(0,5844,by = 365))






