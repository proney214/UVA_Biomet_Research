#Run every column of dataset through the model
klist <- data.frame()
for (i in (3:7)) {
  print(i)
  f <- function(col,PHF) {
    form = as.formula(paste0("mort~s(Trend,k=16*3)+s(",col,",k=i)"))
    data.frame(col = col, gcv = gam(form, data = PHF, family="quasipoisson")$gcv)
  }
  
  #Compile outputs of model table into list of GCV values
  t <- do.call(rbind, lapply(c("MaxTF","MinTF","MaxTDepF","MinTDepF","DTRF","TF1am","TwF1am","TdF1am","RH1am","Speedkts1am","SLPhPa1am","ehPa1am","esubshPa1am","ATF1am","THIF1am","HxF1am","WCF1am","TF7am","TwF7am","TdF7am","RH7am","Speedkts7am","SLPhPa7am","ehPa7am","esubshPa7am","ATF7am","THIF7am","HxF7am","WCF7am","TF1pm","TwF1pm","TdF1pm","RH1pm","Speedkts1pm","SLPhPa1pm","ehPa1pm","esubshPa1pm","ATF1pm","THIF1pm","HxF1pm","WCF1pm","TF7pm","TwF7pm","TdF7pm","RH7pm","Speedkts7pm","SLPhPa7pm","ehPa7pm","esubshPa7pm","ATF7pm","THIF7pm","HxF7pm","WCF7pm"), f, PHF=PHF))
  
  #Find minimum GCV and variable associated with it
  UseThisVariable <- t[which.min(t$gcv),]
  klist <- append(klist,t[which.min(t$gcv),]) #This is the variable created which lists index, gcv, UseThisVariable
  View(UseThisVariable)
  #Add in plot for UseThisVariable
  print(klist)
}
print(klist)
list <- as.matrix(klist, ncol=2)
df <- as.table(list,ncol=2)
