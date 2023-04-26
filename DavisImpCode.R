#Imputation Code

for i = 1
for j = (1:nrow) {
  test_diff=difftime(CITY.working$CITY.working2005_2006.valid[j],Ref.time2005_2006$TIME[i],units="mins")
  if ((abs(test_diff))<=90
}