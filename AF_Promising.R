par(mfrow=c(6,1))

plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VJI Mort Lag RR", ylim = c(0.70,1.30),lwd=3)


plot(pred1.MaxTC_MDC01,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VJI MDC01 Lag RR", ylim = c(0.70,1.30),lwd=3)


plot(pred1.MaxTC_MDC04,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VJI MDC04 Lag RR", ylim = c(0.70,1.30),lwd=3)


plot(pred1.MaxTC_MDC05,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VJI MDC05 Lag RR", ylim = c(0.70,1.30),lwd=3)


plot(pred1.MaxTC_MDC19,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VJI MDC19 Lag RR", ylim = c(0.70,1.30),lwd=3)


plot(pred1.MaxTC_NonBillICD,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VJI NonBillICD Lag RR", ylim = c(0.70,1.30),lwd=3)

# Load necessary packages
library(dlnm)
library(epitools)

# Fit a DLNM model
model <- glm(NonBillICD~cb1.MaxTC_NonBillICD+ns(Trend, 16*3)+as.factor(dow),family=quasipoisson(),lagframe_NonBillICD)

# Extract the coefficients from the model summary
coeffs_NonBillICD <- summary(model)$coefficients

# Calculate the attributable fraction
af <- AF(coeffs["cb1.MaxTC_NonBillICD", "Estimate"], coeffs["cb1.MaxTC_NonBillICD", "Std. Error"], alpha = 0.05)

coef_NonBillICD <- coeffs_NonBillICD[grep("MaxTC_NonBillICD",rownames(coeffs_NonBillICD)), "Estimate"]
Beta_Cummulative_NonBillICD <- sum(coef_NonBillICD)

af_NonBillICD <- 1 - (exp(-Beta_Cummulative_NonBillICD))

coeffs

# Print the attributable fraction
af
