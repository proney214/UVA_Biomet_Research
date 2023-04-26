library(mvmeta)
library(mixmeta)

test <- berkey98

# RUN THE MODEL
model <- mixmeta(cbind(PD,AL) ~ 1, S=berkey98[5:7], data=berkey98)

# ONLY BLUP
blup(model)

# BLUP AND SE
blup(model, se=TRUE)

# SAME AS ABOVE, AGGREGATED BY OUTCOME, WITH PREDICTION INTERVALS
blup(model, se=TRUE, pi=TRUE, aggregate="outcome")

# WITH VCOV, FORCED TO A LIST
blup(model, se=TRUE, pi=TRUE, vcov=TRUE, aggregate="outcome")

# PREDICTING ONLY THE RANDOM-EFFECT RESIDUALS
blup(model, type="residual")
