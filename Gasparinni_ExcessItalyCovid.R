# GASPARINI OPEN CODE

# LOAD THE PACKAGES
library(curl) ; library(data.table)
library(dplyr) ; library(lubridate)
library(dlnm) ; library(mixmeta) ; library(tsModel)
library(splines) ; library(pbs) 
library(scales)
library(sp) ; library(tmap) ; library(GISTools) ; library(sf)
library(ggplot2) ; library(grid)
library(openxlsx)

# DEFINE START AND END DAY FOR POST-PERIOD AND COVID PERIOD
startdate <- dmy(01012005)
coviddate <- dmy(15022020)
enddate <- dmy(31122020)

# DEFINE WEEK PERIODS FOR FEB-APR
seqpost <- seq(startdate, enddate, 1)
cutdate <- unique(c(startdate-1,seqpost[seqpost %in% tapply(seqpost, week(seqpost), last)]))
labperiod1 <- sapply(seq(length(cutdate)-1), function(i) 
  paste(paste0(day(cutdate[i]+1), month(cutdate[i]+1,lab=T)),
        paste0(day(cutdate[i+1]), month(cutdate[i+1],lab=T)), sep="-"))
labperiod2 <- paste("Week", unique(week(seqpost)))

# DEFINE THE KNOTS FOR THE SPLINES FOR MODELLING EXCESS IN POST-PERIOD
nkpost <- 4

# DEFINE THE DF FOR THE CYCLIC SPLINE FOR SEASONALITY
dfseas <- 5

# DEFINE PARAMETERS OF CROSS-BASIS FOR TEMPERATURE
lagtmean <- 21
kpertmean <- c(10,75,90)
nklagtmean <- 3

# DEFINE LAG FOR FLU
lagflu <- 14

# MODEL FORMULA
mformula <- y ~ bpost + date + bseas + factor(wday(date)) + cbtmean

# NUMBER OF RESAMPLING ITERATIONS FOR EMPIRICAL CI
nsim <- 1000

# PREPARE THE AGGREGATED DATA FOR THE MODEL
################################################################################

# SELECT AGE GROUP, COLLAPSE DEATHS AT PROVINCE LEVEL
# NB: COMPUTE FOR munictype=1 ONLY (FULL PERIOD), AND TOTAL (UP TO 2019)
CHO_Final_Weather <- read_excel("~/Desktop/CHO.Final.Weather.xlsx")
EMV_Final_Weather <- read_excel("~/Desktop/EMV.Final.Weather.xlsx")
EZF_Final_Weather <- read_excel("~/Desktop/EZF.Final.Weather.xlsx")

datamodel <- datafull %>% 
  filter(agegrfull %in% agegrsel) %>%
  group_by(regcode, regname, provcode, provname, date) %>%
  summarize(y = sum(ifelse(munictype==1, .data[[ysel]], 0)),
            totdeath = sum(.data[[ysel]])) 

# EXTRAPOLATE TOTAL MORTALITY IN 2020 FOR MUNICIPALITIES WITHOUT SUCH INFO
# NB: BASED ON PROPORTION IN PREVIOUS YEARS IN MUNICIPALITIES WITH FULL DATA
datamodel <- datamodel %>%
  group_by(provcode) %>%
  mutate(ind = year(date)<2020, prop = sum(totdeath[ind])/sum(y[ind])) %>%
  ungroup() %>%
  mutate(totdeath = ifelse(is.na(totdeath), y*prop, totdeath)) %>%
  dplyr::select(-ind, -prop)

# COMPLETE THE SERIES (FILL MISSING DAYS WITH 0'S) BY MERGING
comb <- unique(datamodel[1:4])
expcomb <- cbind(comb[rep(seq(nrow(comb)), each=length(seqdate)),],
                 date=rep(seqdate, nrow(comb)))
datamodel <- merge(data.table(datamodel), expcomb, all.y=T, by=names(expcomb))
if(any(naind <- is.na(datamodel$y))) datamodel[naind,c("y","totdeath")] <- 0

# DEFINE POST-PERIOD SERIES, AND THE KNOTS FOR THE SPLINE
datamodel$tspost <- pmax(as.numeric(datamodel$date-startdate),0)

# MERGE WITH FLU DATA
# NB: AGE CONSIDERED AS > AND/OR <= 64 IF ANY GROUP BELONGING TO THESE RANGES
# NB: REGION 4 CODED AS 41-42 - NEED TO CHANGE FOR MERGING THEN TO CHANGE BACK
flu <- read.csv("data/flu_inc_2015-2020.csv", stringsAsFactors=F, colClasses=c(date="Date"))
fluind <- c(any(agegrsel<=13), any(agegrsel>13))
flu$flu <- if(all(fluind)) flu$inc else if(fluind[1]) flu$inc064 else flu$inc65
datamodel <- mutate(datamodel, regcode=replace(regcode,provcode==21,41),
                    regcode=replace(regcode,provcode==22,42))
datamodel <- merge(datamodel, flu[,c("regcode","date","flu")],
                   by=c("regcode","date"))
datamodel <- mutate(datamodel, regcode=replace(regcode,regcode%in%41:42,4))

# MERGE WITH TEMPERATURE DATA
temp <- read.csv("data/era5_mean2mt_2015-2020.csv", stringsAsFactors=F)
temp <- temp %>%
  rename(provcode=COD_PROV, tmean=X2m_tem_Kelvin) %>%
  mutate(date=as.Date(date), tmean=tmean-273.15)
datamodel <- merge(datamodel, temp, by=c("provcode","date"))

# RE-ORDER AND BACK TO DATA.FRAME
datamodel <- as.data.frame(arrange(datamodel, regcode, provcode, date))

datamodel <- c(CHO_Final_Weather$Mort, CHO_Final_Weather$ATF1pm, CHO_Final_Weather$Date)

# DEFINE PERIODS
seqperiod <- cut(unique(datamodel$tspost), cutdate-startdate, labels=labperiod2)

# SEQUENCES AND REPETITIONS
seqprov <- unique(datamodel$provcode)
seqreg <- unique(datamodel$regcode)
repprovreg <- with(datamodel, tapply(provcode, factor(regcode, levels=seqreg), 
                                     function(x) length(unique(x))))
seqregprov <- rep(seqreg, repprovreg)

# DEFINE AREAS (NORTH/CENTRAL/SOUTH/ISLAND)
areareg <- rep(c("North","Central","South","Islands"), c(8,4,6,2))
areaprov <- areareg[seqregprov]

# DEFINE LABELS
labprov <- sapply(strsplit(unique(datafull$provname), "/"), "[[", 1)
labreg <- sapply(strsplit(unique(datafull$regname), "/"), "[[", 1)

# DEFINE POST-PERIOD SERIES, AND THE KNOTS FOR THE SPLINE
datamodel$tspost <- pmax(as.numeric(datamodel$date-startdate),0)

# RE-ORDER AND BACK TO DATA.FRAME
datamodel <- as.data.frame(arrange(datamodel, regcode, provcode, date))

# DEFINE PERIODS
seqperiod <- cut(unique(datamodel$tspost), cutdate-startdate, labels=labperiod2)

# SEQUENCES AND REPETITIONS
seqprov <- unique(datamodel$provcode)
seqreg <- unique(datamodel$regcode)
repprovreg <- with(datamodel, tapply(provcode, factor(regcode, levels=seqreg), 
                                     function(x) length(unique(x))))
seqregprov <- rep(seqreg, repprovreg)

# DEFINE AREAS (NORTH/CENTRAL/SOUTH/ISLAND)
areareg <- rep(c("North","Central","South","Islands"), c(8,4,6,2))
areaprov <- areareg[seqregprov]

# DEFINE LABELS
labprov <- sapply(strsplit(unique(datafull$provname), "/"), "[[", 1)
labreg <- sapply(strsplit(unique(datafull$regname), "/"), "[[", 1)

# MODELLING: TWO-STAGE, PLUS COMPUTE THE EXCESS MORTALITY 
################################################################################

################################################################################
# FIRST STAGE

# LIST TO STORE COEF/VCOV AND CONVERGENCE INDICATOR
stage1list <- vector("list", length(seqprov))
names(stage1list) <- labprov

# LOOP ACROSS PROVINCES

for(i in seq(seqprov)) {
  
  # PRINT
  #cat(labprov[i],"")
  
  # EXTRACT THE DATA AND COMPLETE IT
  dd <- subset(datamodel, provcode==seqprov[i])
  
  # DEFINE BASIS FUNCTIONS FOR POST-PERIOD, SEASONALITY, TEMPERATURE, FLU
  # NB: USE onebasis TO SIMPLIFY PREDICTIONS AND PLOTTING
  kpost <- equalknots(dd$tspost, nkpost)
  bpost <- onebasis(dd$tspost, fun="bs", degree=2, knots=kpost)
  kseas <- equalknots(yday(dd$date), dfseas)
  bseas <- onebasis(yday(dd$date), fun="pbs", knots=kseas)
  cbtmean <- crossbasis(dd$tmean, lag=lagtmean,
                        argvar=list(fun="bs", degree=2, knots=quantile(dd$tmean, kpertmean/100)),
                        arglag=list(knots=logknots(lagtmean, nklagtmean)))
  flu013 <- runMean(dd$flu, 0:13)
  
  # RUN THE MODEL
  # NB: PRESERVE MISSING TO COMPUTE RESIDUALS LATER
  mod <- glm(mformula, data=dd, family=quasipoisson, na.action="na.exclude")
  
  # SAVE THE RESULTS: COEF/VCOV, RESIDUALS, OVERDISPERSION
  loglik <- sum(dpois(mod$y,mod$fitted.values,log=TRUE))
  disp <- sum(residuals(mod,type="pearson")^2, na.rm=T)/mod$df.res
  stage1list[[i]] <- list(coef=coef(mod), vcov=vcov(mod), dispersion=disp,
                          residuals=residuals(mod, type="deviance"))
}

################################################################################
# SECOND-STAGE META-ANALYSES

# MULTIVARIATE META-ANALYSIS OF COEFFICIENTS OF POST-PERIOD EXCESS
indpost <- grep("bpost", names(stage1list[[1]]$coef))
coefpost <- t(sapply(stage1list, function(x) x$coef[indpost]))
Scov <- lapply(stage1list, function(x) x$vcov[indpost,indpost])
metapost <- mixmeta(coefpost, Scov)
bluppost <- blup(metapost, vcov=T)

################################################################################
# COMPUTE EXCESS MORTALITY

# REDEFINE BASIS 
kpost <- equalknots(datamodel$tspost, nkpost)
bpost <- onebasis(unique(datamodel$tspost), fun="bs", degree=2, knots=kpost)

# DEFINE ARRAY TO STORE THE EXCESS DEATHS BY PROVINCE, PERIOD, RESAMPLING
excprovsim <- array(NA, dim=c(length(seqprov), length(labperiod1)+1, nsim+1),
                    dimnames=list(labprov, c("15Feb-15May",labperiod1),
                                  c("est",paste0("sim",seq(nsim)))))

# LOOP ACROSS PROVINCES
for(i in seq(seqprov)) {
  
  # PRINT
  #cat(labprov[i],"")
  
  # RETRIEVE COEF/VCOV AND TOTAL DEATHS
  coef <- bluppost[[i]]$blup
  vcov <- bluppost[[i]]$vcov
  death <- subset(datamodel, provcode==seqprov[i] & date>=startdate)$totdeath
  
  # COMPUTE ATTRIBUTABLE NUMBER (EXCESS), AND STORE THE SUM BY PERIOD
  an <- (1-exp(-bpost%*%coef))*death
  indcovid <- seqpost>=coviddate
  excprovsim[i,1,"est"] <- sum(an[indcovid])
  excprovsim[i,-1,"est"] <- tapply(an, seqperiod, sum)
  
  # SAMPLE COEF ASSUMING A MULTIVARIATE NORMAL DISTRIBUTION
  set.seed(13041975)
  coefsim <- mvrnorm(nsim, coef, vcov)
  
  # LOOP ACROSS ITERATIONS AND DO AS ABOVE WITH RESAMPLES COEF
  for(s in seq(nsim)) {
    an <- (1-exp(-bpost%*%coefsim[s,]))*death
    excprovsim[i,1,s+1] <- sum(an[indcovid])
    excprovsim[i,-1,s+1] <- tapply(an, seqperiod, sum)
  }
}

# COLLAPSE BY REGION AND THEN FULL COUNTRY
excregsim <- apply(excprovsim, 2:3, tapply, seqregprov, sum)
excitalysim <- apply(excregsim, 2:3, sum)


# COMPUTE EXCESS MORTALITY

# REDEFINE BASIS 
kpost <- equalknots(datamodel$tspost, nkpost)
bpost <- onebasis(unique(datamodel$tspost), fun="bs", degree=2, knots=kpost)

# DEFINE ARRAY TO STORE THE EXCESS DEATHS BY PROVINCE, PERIOD, RESAMPLING
excprovsim <- array(NA, dim=c(length(seqprov), length(labperiod1)+1, nsim+1),
                    dimnames=list(labprov, c("15Feb-15May",labperiod1),
                                  c("est",paste0("sim",seq(nsim)))))

# LOOP ACROSS PROVINCES
for(i in seq(seqprov)) {
  
  # PRINT
  #cat(labprov[i],"")
  
  # RETRIEVE COEF/VCOV AND TOTAL DEATHS
  coef <- bluppost[[i]]$blup
  vcov <- bluppost[[i]]$vcov
  death <- subset(datamodel, provcode==seqprov[i] & date>=startdate)$totdeath
  
  # COMPUTE ATTRIBUTABLE NUMBER (EXCESS), AND STORE THE SUM BY PERIOD
  an <- (1-exp(-bpost%*%coef))*death
  indcovid <- seqpost>=coviddate
  excprovsim[i,1,"est"] <- sum(an[indcovid])
  excprovsim[i,-1,"est"] <- tapply(an, seqperiod, sum)
  
  # SAMPLE COEF ASSUMING A MULTIVARIATE NORMAL DISTRIBUTION
  set.seed(13041975)
  coefsim <- mvrnorm(nsim, coef, vcov)
  
  # LOOP ACROSS ITERATIONS AND DO AS ABOVE WITH RESAMPLES COEF
  for(s in seq(nsim)) {
    an <- (1-exp(-bpost%*%coefsim[s,]))*death
    excprovsim[i,1,s+1] <- sum(an[indcovid])
    excprovsim[i,-1,s+1] <- tapply(an, seqperiod, sum)
  }
}

# COLLAPSE BY REGION AND THEN FULL COUNTRY
excregsim <- apply(excprovsim, 2:3, tapply, seqregprov, sum)
excitalysim <- apply(excregsim, 2:3, sum)

# SOME EXAMPLES OF RESULTS
################################################################################

# TOTAL NUMBER OF DEATHS IN ITALY AND A SELECTED PROVINCE IN 15Feb-15MAY 2020
(tot <- sum(subset(datamodel, date>=dmy("15022020"))$totdeath))
(totfl <- sum(subset(datamodel, date>=dmy("15022020") &
                       provname=="Firenze")$totdeath))

# EXCESS MORTALITY IN THE SAME AREAS AND PERIOD
excitalysim["15Feb-15May", "est"]
excprovsim["Firenze", "15Feb-15May", "est"]

# EMPIRICAL CONFIDENCE INTERVALS
quantile(excitalysim["15Feb-15May",-1], c(2.5,97.5)/100)

# IN A PERCENTAGE SCALE
excitalysim["15Feb-15May","est"] / (tot-excitalysim["15Feb-15May","est"]) *100
excprovsim["Firenze","15Feb-15May","est"] /
  (totfl-excprovsim["Firenze","15Feb-15May","est"]) * 100

# EXCESS MORTALITY BY WEEK (FIRST AND LAST ONLY PARTIAL)
excitalysim[-1,"est"]

# THE SAME FIGURES CAN BE COMPUTED BY SEX AND AGE SUB-GROUPS

################################################################################
# MAP

# READ THE SHAPEFILE OF THE PROVINCES SIMPLIFY (SOLVE SOME ISSUES), REORDER
spprov <- st_as_sf(readRDS("data/province weighted centroids.Rds"))
spprov <- st_simplify(spprov, dTolerance=1000)
spprov <- spprov[match(seqprov, spprov$COD_PROV),]

# BREAKS AND PALETTE
breaks <- c(-Inf, 0, 20, 50, 100, 150, 200, 300, 400, 500, Inf)
col <- colorRampPalette(c("yellow","red","purple3"))(10)

# EXTRACT ESTIMATES AND COMPUTE THE EXCESS IN PERCENTAGE
exc <- excprovsim[,1,1]
tot <- with(subset(datamodel, date>=coviddate),
            tapply(totdeath, factor(provcode, levels=unique(provcode)), sum))
spprov$excess <- exc/(tot-exc)*100

# MAP FOR BOTH SEXES AND ALL AGES
tm_shape(spprov) + 
  tm_polygons("excess", palette=col, breaks=breaks, midpoint=NA,
              title="Excess (%)") + 
  tm_layout(frame=F, title="Both sexes - All ages",
            title.position=c("center","bottom"))


