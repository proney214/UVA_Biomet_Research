## Max Min Plots

library(ggplot2)
library(readxl)
library(dplyr)
library(reshape2)
library(zoo)

CHO <- read_excel("~/Desktop/CHO.Working.Complete (1).xlsx", na = "NA")
#CHO <- select(CHO,1:91)
CHO %>% mutate_if(is.character,is.numeric)

EMV <- read_excel("~/Desktop/EMV.Working.Complete (3).xlsx", na = "NA")
#EMV <- select(EMV,1:91)
EMV %>% mutate_if(is.character,is.numeric)

EZF <- read_excel("~/Desktop/EZF.Working.Complete (1).xlsx", na = "NA")
#EZF <- select(EZF,1:91)
EZF %>% mutate_if(is.character,is.numeric)

IAD <- read_excel("~/Desktop/IAD.working.weatherprep.xlsx", na = "NA")
#IAD <- select(IAD,1:91)
IAD %>% mutate_if(is.character,is.numeric)

LYH <- read_excel("~/Desktop/LYH.working.weatherprep.xlsx", sheet = "Sheet1", na = "NA")
#LYH <- select(LYH,1:91)
LYH %>% mutate_if(is.character,is.numeric)

OKV <- read_excel("~/Desktop/OKV.working.weatherprep.xlsx", sheet = "Sheet1", na = "NA")
#OKV <- select(OKV,1:91)
OKV %>% mutate_if(is.character,is.numeric)

ORF <- read_excel("~/Desktop/ORF.working.weatherprep.xlsx", sheet = "Sheet1", na = "NA")
#ORF <- select(ORF,1:91)
ORF %>% mutate_if(is.character,is.numeric)

PHF <- read_excel("~/Desktop/PHF.Working.Complete.xlsx", na = "NA")
#PHF <- select(PHF,1:91)
PHF %>% mutate_if(is.character,is.numeric)

RIC <- read_excel("~/Desktop/RIC.working.weatherprep.xlsx", sheet = "Sheet1", na = "NA")
#RIC <- select(RIC,1:91)
RIC %>% mutate_if(is.character,is.numeric)

ROA <- read_excel("~/Desktop/ROA.Working.Complete.xlsx", na = "NA")
#ROA <- select(ROA,1:91)
ROA %>% mutate_if(is.character,is.numeric)

SHD <- read_excel("~/Desktop/SHD.Working.Complete.xlsx", sheet = "Sheet1", na = "NA")
#SHD <- select(SHD,1:91)
SHD %>% mutate_if(is.character,is.numeric)

VJI <- read_excel("~/Desktop/VJI.working.weatherprep.xlsx", na = "NA")
#VJI <- select(VJI,1:91)
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

plot(OKV$`MaxT(C)`, type= 'l', col = 'dark grey',  xlab = "Date",ylab = "Temperature (C)",
     main = "OKV",      
     xaxt = "n")
axis(1, at = c(seq(1,5845, by = 365*5)),
     labels = c("2005", "2010","2015","2020"))
lines(OKV$`MinT(C)`, type= 'l', col = 'light grey')
lines(rollmean(OKV$`MaxT(C)`,k=21), col = 'red')
lines(rollmean(OKV$`MinT(C)`,k=21), col = 'blue')
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



