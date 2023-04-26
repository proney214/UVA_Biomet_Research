*** Correlation Matrix***
  
library(readxl)
RIC_correlation_test <- read_excel("~/Desktop/RIC.correlation.test.xlsx")

RIC_correlation_test %>% mutate_if(is.character, as.numeric)
RIC_correlation_test$`SLP(hPa)-1am`=as.numeric(RIC_correlation_test$`SLP(hPa)-1am`)
RIC_correlation_test$`SLP(hPa)-7am`=as.numeric(RIC_correlation_test$`SLP(hPa)-7am`)
RIC_correlation_test$`SLP(hPa)-1pm`=as.numeric(RIC_correlation_test$`SLP(hPa)-1pm`)
RIC_correlation_test$`SLP(hPa)-7pm`=as.numeric(RIC_correlation_test$`SLP(hPa)-7pm`)


final<-cor(RIC_correlation_test,method = "pearson", use = "complete.obs")
final<-cor(test,method = "pearson", use = "complete.obs")

final<-as.data.frame(final)

plot(mort,type="l")

install.packages("writexl")
library(writexl)
write_xlsx(final,"~/Desktop/RICcormatrix.xlsx")

#General Plot template

#Base R plot functions to create line plots.
plot(x, y, type = "b", lty = 1, lwd = 1, col = "blue")
lines(x, y, lty = 2, lwd = 1)

geom_line(aes(x, y), data = NULL, linetype = "dashed", color = "black", size = 1): ggplot2 function to create line plots.
scale_linetype_manual(), scale_color_manual(), and scale_size_manual(): ggplot2 functions to set manually lines type, color and size.

