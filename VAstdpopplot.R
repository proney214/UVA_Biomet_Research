#Creat Plot for VA 2020 Standardized Population Grouping

V_A2020stdpopfinal <- data.frame(c(V_A2020stdpopraw [,1:2],V_A2020stdpoprev))

popplotog <- V_A2020stdpopfinal[3:10]

popplotlong <- pivot_longer(popplotog,1:8)
poptrunc <- trunc(popplotlong$value)

#create empty data frame with 10 rows and 10 columns to match Wx mort data
dfpopplot <- data.frame(matrix(ncol = 2, nrow = 8))

#provide column names
colnames(dfpopplot) <- c('name','value')

dfpopplot[,1] <- popplotlong[,1]
dfpopplot[,2] <- poptrunc

popxlabs <- c("0-9","10-19","20-29","30-39","40-49","50-64","65-74","75+")

ggplot(dfpopplot,aes(x = name, y = value)) +
geom_bar(position = 'dodge', stat='identity',fill="light blue") +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) +
  xlab("Age Group") + ylab("Standardardized Population") +
  scale_x_discrete(labels= popxlabs) +
  theme_gray()+
  ggtitle("VA 2020 Standardized Population Grouping")

