## Natural populations: inter-annual fitness variation and the effect of neighbors at different range locations
##
## Analysis for "Role of climate and competitors in limiting fitness across range edges of an annual plant" 
## by Stanton-Geddes, J., P. Tiffin and R. Shaw (2012) Ecology
##
## By John Stanton-Geddes, June 2011. All errors are my own. 
## Some exploratory analyses not recorded in the paper are included below.
##
## DATA:
##  Year - year data recorded
##  Site - site of observation: Conard Environmental Research Area (CRA), Grey Cloud Dunes Scientific and Natural Area (GCD), Green River State Wildlife Area (GRE), Konza Prairie (KZA), TYSm (Tyson Reseach Center, main site. small population and dropped after 2007), TYSa (Tyson Research Center, annex, added 2008), AFT (Afton State Park, added 2008).
##  Plant.ID - identifier for each experimental plant
##  Trt - experimental treatment, either removal (R) or surrounding vegetation or control (C) of no removal
##  NumCf - number of other C. fasciculata seedlings within 20cm radius of plant. not recorded in 2007
##  pForbs - percent cover of forbs in 20cm radius of plant. not recorded in 2007
##  pGrasses - percent cover of grasses in 20cm radius of plant. not recorded in 2007
##  pLegumes - percent cover of legumes in 20cm radius of plant. not recorded in 2007
##  pWoodys - percent cover of woody vegetation in 20cm radius of plant. not recorded in 2007
##  pBare - percent cover of bare ground in 20cm radius of plant. not recorded in 2007
##  weight.clipped - weight of clipped biomass in circle of 20cm surrounding target plant (Removal plants only). collected only in 2008
##  Initial.Height - height (cm) of plant at time treatment was initiated
##  Initial. LfNumb - leaf number of plant at time treatment was initiated. 2009 only
##  Survival - whether a plant survived to the end of the season or not (0 - no, 1 - yes). plants that could not be located were given NA
##  Stage - reproductive stage at time of end-of-season measurement. 0 - dead/not present, 1 - vegetative, 2 - flowering, 3 - flower and pods, 4 - pods, 5 - completely senesced
##  Final.Height - height (cm) of plant at time of end-of-season measurement
##  Fin.Branch - branch number of plant at time of end-of-season measurement
##  Fin.LeafNum - leaf number of plant at time of end-of-season measurement. plants had often senesced so this data often missing or unreliable
##  Fin.SeedPod - number of seedpods at end-of-season measurement. for plants with seedpods that dehisced, this includes pedicels


library(MASS)
library(gplots)
library(lattice)
library(bbmle)

# Read data file and modify
NRdata <- read.table("Cf_NR_data.csv", header=TRUE, sep=",", na.strings=".")
head(NRdata)
str(NRdata)

NRdata$Year <- as.factor(NRdata$Year)
NRdata$Plant.ID <- as.factor(NRdata$Plant.ID)

NRdata$Site <- factor(NRdata$Site, levels = c("TYSm", "TYSa", "KZA", "GRE", "CRA", "GCD", "AFT"), ordered = T)

summary(NRdata)

# Table data 

(TrtByYearTable = with(NRdata, ftable(tapply(Fin.SeedPod, list(Year,Site,Trt),mean, na.rm=T))))

(PercentIncrease = (TrtByYearTable[,2]-TrtByYearTable[,1])/TrtByYearTable[,1] * 100)

# Sample sizes
with(NRdata, table(list(Site, Trt, Year)))

# Drop TYSm because small sample, not representative of region and not used in future years
NRdata1 <- subset(NRdata, Site != "TYSm", )
NRdata1 <- drop.levels(NRdata1)
str(NRdata1)

NRdata1$Site <- factor(NRdata1$Site, levels = c("TYSa", "KZA", "GRE", "CRA", "GCD", "AFT"), ordered = T)

# Preliminary data examination

bwplot(Fin.SeedPod ~ Trt|Site, data = NRdata1)

hist(NRdata1$Fin.SeedPod) # not gaussian
hist(log(NRdata1$Fin.SeedPod)) # nearly gaussian

### Count data with categorical predictors: log-linear model with poisson errors
### What is the effect of site, year and treatment on seed pod production? 

NRglm1 <- glm(Fin.SeedPod ~ Year * Site * Trt, poisson, data = NRdata1)
summary(NRglm1)
# Serious overdispersion.  Fit with quasi-likelihood and use F tests in analysis of deviance
NRglm2 <- glm(Fin.SeedPod ~ Year * Site * Trt, quasipoisson, data = NRdata1)
summary(NRglm2)

# Drop three-way interaction
dropterm(NRglm2, test = "F")

NRglm3 <- update(NRglm2, . ~ . - Year:Site:Trt)
dropterm(NRglm3, test = "F")
anova(NRglm3, NRglm2, test = "F")
# Year:Site:Trt not significant

NRglm4 <- update(NRglm3, . ~ . - Site:Trt)
summary(NRglm4)
anova(NRglm4, NRglm3, test = "F")
# Site:trt not significant
dropterm(NRglm4, test = "F")

NRglm5 <- update(NRglm4, . ~ . - Year:Trt)
summary(NRglm5)
anova(NRglm5, NRglm4, test = "F")
# Year:Trt not significant
dropterm(NRglm5, test = "F")

NRglm6 <- update(NRglm5, . ~ . - Year:Site)
summary(NRglm6)
anova(NRglm6, NRglm5, test = "F")
# Year:Site highly significant
dropterm(NRglm6, test = "F")

# Test main effects of Year and Site and Trt against model without interactions
NRglm7 <- update(NRglm6, . ~ . - Trt)
summary(NRglm7)
anova(NRglm7, NRglm6, test = "F")

NRglm8 <- update(NRglm6, . ~ . - Year)
summary(NRglm8)
anova(NRglm8, NRglm6, test = "F")

NRglm9 <- update(NRglm6, . ~ . - Site)
summary(NRglm9)
anova(NRglm9, NRglm6, test = "F")


# Get predicted values for most likely model (NRglm5)

NRpred <- predict(NRglm5, se.fit = TRUE)
ePods <- NRpred$fit
sePods <- NRpred$se.fit

# 339 obs deleted due to missingness in glm.nb. Subset to data without these:
NRdata2 <- subset(NRdata1, select = c("Year", "Site", "Plant.ID", "Trt", "Fin.SeedPod"))
NRdata2 <- na.omit(NRdata2)
str(NRdata2)

# Combine predictors and estimated values
NRpreddata <- cbind(NRdata2, ePods, sePods)

str(NRpreddata)

################################################################################
## Plot for paper: 2 panels:
tiff(filename="CfE3_Fig3.tiff", width = 152, height = 150, units = "mm", compression = "lzw", bg = "white", res = 600)
par(mfrow = c(2,1), mar = c(2, 4, 1, 2) + 0.1)
# Plot differences of site by year
(YearxSite <- with(NRpreddata, tapply(ePods, list(Year, Site), mean)))
(se.YearxSite <- with(NRpreddata, tapply(sePods, list(Year, Site), mean)))
# no consistent pattern, effect of year varies by site 
poplabels <- c("MO", "KS", "IL", "IA", "MN1", "MN2")

b1 <- barplot(YearxSite, beside = TRUE, col = c("black","grey","white"), ylim = c(0,5), 
  ylab = "Seedpods", xaxt = "n", las = 2)
plotCI(b1, YearxSite, se.YearxSite, add = TRUE, pch = ".", gap = FALSE)
mtext(side = 1, poplabels, at = c(2.5, 6.75, 10.5, 14.75, 18.5, 23.5))
legend("topright", legend = levels(NRdata2$Year), fill = c("black","grey","white"), bty = "n")
text(1,0.15,"n.d")
text(21.2,0.15,"n.d")
mtext("(a)", side = 2, line = 2, at = 5, las = 2, cex = 1.5)
segments(0.1,0,25,0)

#  Plot differences by treatment at each site
(TrtxSite <- with(NRpreddata, tapply(ePods, list(Trt, Site), mean)))
(se.TrtxSite <- with(NRpreddata, tapply(sePods, list(Trt, Site), mean)))
# no consistent pattern, effect of year varies by site 
# Percent increase at each site:
(TrtxSite[2,]-TrtxSite[1,])/TrtxSite[1,]

b2 <- barplot(TrtxSite, beside = TRUE, col = c("black","white"), ylim = c(0,4), ylab = "Seedpods",
  xlab = "Population", sub = "South (interior) to North (edge)", xaxt = "n", las = 2)
plotCI(b2, TrtxSite, se.TrtxSite, add = TRUE, pch = ".", gap = FALSE)
legend("topright", legend = c("+ Neighbors", "- Neighbors"), fill = c("black","white"), bty = "n")
mtext(side = 1, poplabels, at = c(2, 5, 8, 11, 14, 17))
mtext("(b)", side = 2, line = 2, at = 4, las = 2, cex = 1.5) 
segments(0.1,0,25,0)

dev.off()

################################################################################
# Group populations by region and test effect of neighbor removal treatment

NRdata2$region <- ordered(rep("Interior", length(NRdata2$Site)), levels = c("Interior", "W.edge", "N.edge"))

for(i in 1:length(NRdata2$Site)) {
  NRdata2$region[i] <- ifelse(NRdata2$Site[i] == "AFT" | NRdata2$Site[i] == "GCD", "N.edge", 
    ifelse(NRdata2$Site[i] == "KZA", "W.edge", "Interior"))
  }
 
region.nb.1 <- glm.nb(Fin.SeedPod ~ region * Year * Trt, data = NRdata2) 
summary(region.nb.1)

region.nb.aic <- stepAIC(region.nb.1)
region.nb.aic$anova
# Region:trt interaction not significant...consistent with previous results
################################################################################



###-------------In unclipped plots, examine relationship between percent cover and fitness----
### Examine relationship between percent cover and weight of vegetation clipped in second year

per.cover_weight <- subset(NRdata1, weight.clipped > 0, )
per.cover_weight <- drop.levels(per.cover_weight)
str(per.cover_weight)

with(per.cover_weight, plot(pBare, weight.clipped))
# Negative relationship
with(per.cover_weight, cor.test(pBare, weight.clipped))
# cor = -0.85, r2 = 0.74.  confirms that percent cover associated with vegetation in plot

# Subset to control plants, exclude 2007 as percent cover data not collected
control_NRdata <- subset(NRdata1, Trt == "C" & Year != "2007", )
control_NRdata <- drop.levels(control_NRdata)
str(control_NRdata)

# Preliminary data examination
plot(control_NRdata$pBare, control_NRdata$Fin.SeedPod)
cor.test(control_NRdata$pBare, control_NRdata$Fin.SeedPod)
# no pattern
plot(control_NRdata$pBare, control_NRdata$Final.Height)
cor.test(control_NRdata$pBare, control_NRdata$Final.Height)
# no clear pattern.  negative correlation, though practically zero.


str(control_NRdata)

# Remove missing data
control_NRdata1 <- subset(control_NRdata, Fin.SeedPod != "NA", c("Year", "Site", "Trt", "pBare", "Fin.SeedPod"))

control_NRdata1 <- na.exclude(control_NRdata1)
str(control_NRdata1)

### GLM with percent cover as covariate
NPglm1 <- glm(Fin.SeedPod ~ Year * Site * pBare, poisson, data = control_NRdata1)
summary(NPglm1)
# still significant overdispersion. use negative binomial

NPnb1 <- glm.nb(Fin.SeedPod ~ Year * Site * pBare, data = control_NRdata1)
NPnb2 <- stepAIC(NPnb1)
summary(NPnb1)
NPnb2$anova
summary(NPnb2)
## No effect of including bare ground as a covariate

### Examine site by site

#TYSa
TYSa_control_NRdata <- subset(control_NRdata, Site == "TYSa", )
TYSa_control_NRdata <- drop.levels(TYSa_control_NRdata)
str(TYSa_control_NRdata)

with(TYSa_control_NRdata, plot(pBare, Fin.SeedPod))
cor.test(TYSa_control_NRdata$pBare, TYSa_control_NRdata$Fin.SeedPod)
# no pattern 

#KZA
KZA_control_NRdata <- subset(control_NRdata, Site == "KZA", )
KZA_control_NRdata <- drop.levels(KZA_control_NRdata)
str(KZA_control_NRdata)

cor.test(KZA_control_NRdata$pBare, KZA_control_NRdata$Fin.SeedPod)
with(KZA_control_NRdata, plot(pBare, Fin.SeedPod))
# more plots with >20% bare ground than CRA. fitness seems to be maximized at
# intermediate percent cover

#GRE
GRE_control_NRdata <- subset(control_NRdata, Site == "GRE", )
GRE_control_NRdata <- drop.levels(GRE_control_NRdata)
str(GRE_control_NRdata)

with(GRE_control_NRdata, plot(pBare, Fin.SeedPod))
# Wow...very clear pattern of increasing pod production with decreasing percent cover
cor.test(GRE_control_NRdata$pBare, GRE_control_NRdata$Fin.SeedPod)
# r = .45, r2 = .2. 

#CRA
CRA_control_NRdata <- subset(control_NRdata, Site == "CRA", )
CRA_control_NRdata <- drop.levels(CRA_control_NRdata)
str(CRA_control_NRdata)

with(CRA_control_NRdata, plot(pBare, Fin.SeedPod))
cor.test(CRA_control_NRdata$pBare, CRA_control_NRdata$Fin.SeedPod)
# few plants less than 10% empty...though only two plants with >20 seed pods 
# in these plots

#GCD
GCD_control_NRdata <- subset(control_NRdata, Site == "GCD", )
GCD_control_NRdata <- drop.levels(GCD_control_NRdata)
str(GCD_control_NRdata)

with(GCD_control_NRdata, plot(pBare, Fin.SeedPod))
# Not as strong, but also a pattern of increasing pods with decreasing percent cover
cor.test(GCD_control_NRdata$pBare, GCD_control_NRdata$Fin.SeedPod)
# correlation not significant and actully negative because of many patches that
# are mostly empty and have low production. subset to year with high pod production
# and check
GCD09_control_NRdata <- subset(GCD_control_NRdata, Year == "2009", )
GCD09_control_NRdata <- drop.levels(GCD09_control_NRdata)
str(GCD09_control_NRdata)

with(GCD09_control_NRdata, plot(pBare, Fin.SeedPod))
cor.test(GCD09_control_NRdata$pBare, GCD09_control_NRdata$Fin.SeedPod)
# correlation becomes positive, but still not significant

#AFT
AFT_control_NRdata <- subset(control_NRdata, Site == "AFT", )
AFT_control_NRdata <- drop.levels(AFT_control_NRdata)
str(AFT_control_NRdata)

with(AFT_control_NRdata, plot(pBare, Fin.SeedPod))
cor.test(AFT_control_NRdata$pBare, AFT_control_NRdata$Fin.SeedPod)
# only 45 points and well scattered 



##------------------Number seedlings--------------------------------------------
with(NRdata, tapply(NumCf, list(Site, Year), mean, na.rm = TRUE))

seedling_data <- subset(NRdata, Year != "2007", )

hist(seedling_data$NumCf)
seedling_glm1 <- glm(NumCf ~ Site * Year, poisson, data = seedling_data)
summary(seedling_glm1)
# way overdispersed. use negative binomial

seedling_nb1 <- glm.nb(NumCf ~ Site * Year, data = seedling_data)
summary(seedling_nb1)

seedling_nb2 <- glm.nb(NumCf ~ Site + Year, data = seedling_data)
summary(seedling_nb2)

seedling_nb3 <- glm.nb(NumCf ~ Site, data = seedling_data)
summary(seedling_nb3)

anova(seedling_nb3, seedling_nb2, seedling_nb1)
# All terms significant by model simplification



##-----------------Trait differences between sites------------------------------

with(control_NRdata, tapply(Final.Height, list(Site, Year), mean, na.rm = TRUE))

with(control_NRdata, tapply(Fin.Branch, list(Site, Year), mean, na.rm = TRUE))




##----------Examine correlation between weather and seed production------

weather.data <- read.csv("CfE1_weather_data.csv", na.string = ".")
str(weather.data)

plot(weather.data$P_may_sep, weather.data$avg_pods)
cor.test(weather.data$P_may_sep, weather.data$avg_pods)
# positive, but not significant
# one low point from TYS pop in dry hot year

plot(weather.data$P_may_sep[-11], weather.data$avg_pods[-11])
cor.test(weather.data$P_may_sep[-11], weather.data$avg_pods[-11])
# correlation becomes stronger but still not significant if this point is removed
# two low points remain...interestingly, both from GCD

# Subset to remove northern edge pops, GCD
interior.subset.weather <- subset(weather.data, site != "GCD" & site != "AFT", )
str(interior.subset.weather)

cor.test(interior.subset.weather$P_may_sep, interior.subset.weather$avg_pods)
# similar pattern, not significant

# compare with temperature
plot(weather.data$T_may_sep, weather.data$avg_pods)
cor.test(weather.data$T_may_sep, weather.data$avg_pods)

# compare with degree days
plot(weather.data$growing_degree_days_base50, weather.data$avg_pods)
cor.test(weather.data$growing_degree_days_base50, weather.data$avg_pods)

# are temperature and degre days correlated?
plot(weather.data$growing_degree_days_base50, weather.data$T_may_sep)
cor.test(weather.data$growing_degree_days_base50, weather.data$T_may_sep)
# Yes; r=0.96 p = 2.7e-8