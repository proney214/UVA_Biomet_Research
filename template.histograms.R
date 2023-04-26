library(readxl)
library(dplyr)

BLF_working_weatherprep <- read_excel("~/Desktop/BLF.working.weatherprep.xlsx",na="NA")

BLF_working_weatherprep %>% mutate_if(is.character, as.numeric)

BLF_working_weatherprep$`T(F)-7am` = as.numeric(BLF_working_weatherprep$`T(F)-7am`)
BLF_working_weatherprep$`AT(F)-1pm` = as.numeric(BLF_working_weatherprep$`AT(F)-1pm`)

plot = hist(BLF_working_weatherprep$`T(F)-7am`,breaks=120)
plot2 = hist(BLF_working_weatherprep$`AT(F)-1pm`,breaks=120)
plot3 = hist(BLF_working_weatherprep$`AT(C)-1pm`,breaks=240)


BLF_working_weatherprep <- read_excel("~/Desktop/BLF.working.weatherprep.xlsx")
View(BLF_working_weatherprep)

BLF_working_weatherprep$`T(F)-7am` = as.numeric(BLF_working_weatherprep$`T(F)-7am`)
plot = hist(BLF_working_weatherprep$`T(F)-7am`,breaks=120)


Book4 <- read_excel("~/Downloads/Book4.xlsx",na="NA")
Book4 %>% mutate_if(is.character, as.numeric)
plot5 = plot(x=Book4$OKV,y=Book4$BLF)
