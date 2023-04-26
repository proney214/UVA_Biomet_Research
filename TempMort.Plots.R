## Mort Plots

library(ggplot2)
library(readxl)
library(dplyr)
library(reshape2)

AdjustedMortMasterOrg <- read_excel("~/Desktop/AdjustedMortMaster.xlsx")
AdjustedMortMaster <- head(AdjustedMortMasterOrg, - 1) 

CHO <- read_excel("~/Desktop/CHO.Working.Complete (1).xlsx", na = "NA")
CHO <- select(CHO,1:112)
CHO %>% mutate_if(is.character,is.numeric)

EMV <- read_excel("~/Desktop/EMV.Working.Complete (3).xlsx", na = "NA")
EMV <- select(EMV,1:112)
EMV %>% mutate_if(is.character,is.numeric)

hist(EMV$MaxTF)
max(EMV$ATF1pm,na.rm = TRUE)

EZF <- read_excel("~/Desktop/EZF.Working.Complete (1).xlsx", na = "NA")
EZF <- select(EZF,1:112)
EZF %>% mutate_if(is.character,is.numeric)

IAD <- read_excel("~/Desktop/IAD.working.weatherprep.xlsx", na = "NA")
IAD <- select(IAD,1:112)
IAD %>% mutate_if(is.character,is.numeric)

LYH <- read_excel("~/Desktop/LYH.working.weatherprep.xlsx", sheet = "Sheet1", na = "NA")
LYH <- select(LYH,1:112)
LYH %>% mutate_if(is.character,is.numeric)

OKV <- read_excel("~/Desktop/OKV.working.weatherprep.xlsx", sheet = "Sheet1", na = "NA")
OKV <- select(OKV,1:112)
OKV %>% mutate_if(is.character,is.numeric)

ORF <- read_excel("~/Desktop/ORF.working.weatherprep.xlsx", sheet = "Sheet1", na = "NA")
ORF <- select(ORF,1:112)
ORF %>% mutate_if(is.character,is.numeric)

PHF <- read_excel("~/Desktop/PHF.Working.Complete.xlsx", na = "NA")
PHF <- select(PHF,1:112)
PHF %>% mutate_if(is.character,is.numeric)

RIC <- read_excel("~/Desktop/RIC.working.weatherprep.xlsx", sheet = "Sheet1", na = "NA")
RIC <- select(RIC,1:112)
RIC %>% mutate_if(is.character,is.numeric)

ROA <- read_excel("~/Desktop/ROA.Working.Complete.xlsx", na = "NA")
ROA <- select(ROA,1:112)
ROA %>% mutate_if(is.character,is.numeric)

SHD <- read_excel("~/Desktop/SHD.Working.Complete.xlsx", sheet = "Sheet1", na = "NA")
SHD <- select(SHD,1:112)
SHD %>% mutate_if(is.character,is.numeric)

VJI <- read_excel("~/Desktop/VJI.working.weatherprep.xlsx", na = "NA")
VJI <- select(VJI,1:112)
VJI %>% mutate_if(is.character,is.numeric)

test <- rbind(CHO,EMV,EZF,IAD,LYH,OKV,ORF,PHF,RIC,ROA,SHD,VJI)
max(test$`MaxT(C)`,na.rm = TRUE) #41.1 C
min(test$`MaxT(C)`,na.rm = TRUE) #-12.8 C
mean(test$`MaxT(C)`,na.rm = TRUE) #20.2 C

df <- data.frame(time = VJI$Date,
                 Real_Mort = VJI$mort,
                 Adjusted_Mort = AdjustedMortMaster$rowSums.newmortcompleteVJI...2.9...na.rm...TRUE.)
df <- melt(df ,  id.vars = 'time', variable.name = 'series')

# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplot(df, aes(time,value)) + 
  geom_line(aes(colour = series))+
  geom_smooth(aes(colour = series), se = F) +
  xlab("Year") + ylab("Mortality") +
  #ylim(0,25) +
  scale_colour_grey(start = 0.5, end = 0) +
  theme(legend.position="right",
        legend.title=element_blank())+
  ggtitle("VJI Mortalities") 

ggsave("MortPlots_VJI.png")
