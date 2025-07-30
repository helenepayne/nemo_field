## Population growth rate
##
## Analysis for "Role of climate and competitors in limiting fitness across range edges of an annual plant" 
## by Stanton-Geddes, J., P. Tiffin and R. Shaw (2012) Ecology
##
## By John Stanton-Geddes, June 2011. All errors are my own. 
## Some exploratory analyses not recorded in the paper are included below.
##
## NOTE: the aster analyses were largely based on and modified from code available in the many aster technical reports
## by Charlie Geyer (www.stat.umn.edu/geyer/aster/). However, any errors in the code are solely my responsibility.

## Examination of recruitment into experimental populations of Chamaecrista
## fasciculata established at 5 sites: interior (CERA), west edge (RNHA), north
## edge (SCRS), beyond west (CPBS) and beyond north (ACNW). At each site, I 
## established 5 blocks with either 20 (interior and edge site) or 10 (beyond 
## edge) experimental populations per block. Each experimental population 
## consisted of 4 seeds from the same population. The experimental populations 
## were planted in spring 2009 and survival and reproduction recorded, but the 
## plants, including seed pods, were left in the field. Analyis for 2009
## survival and reproduction is done separately (CfE3.r). In June 2010, I returned
## to each site and recorded the number of seedlings found within 60cm of the 
## center of each experimental population. 
## In this script, I analyze growth rate of each population at each site, and 
## how this is influenced by the competition treatment performed in 2009
## Note: the competition treatment was a "pulse" applied at the beginning of the
## 2009 season, and thus differences in competition were lessened in 2010, so
## any differences between treatments are carry-over effects from the effects
## of neighbor removal on plant fitness in 2009.

## Note: because of very low germination at SCRS, I replanted the first 5 blocks
## in June. However, very few of these seeds also germinated and survived. For
## this analysis, I am ignoring any plants that came up in the second planting
## (data on these in CfE3_data2010.xls) because in these 5 blocks, only 2 had 
## seedlings (Blocks 1 and 5) and in both of these blocks there were plants 
## from the first planting that likely were the source plant. 

################################################################################
# Input data and load libraries

library(MASS)
library(aster)
library(gdata)
library(lattice)
library(plotrix)

CfE3_recruit <- read.csv("CfE3_recruitment.csv")
################################################################################

################################################################################
# Check data is correct

str(CfE3_recruit)

# Change block to factor
CfE3_recruit$block <- as.factor(CfE3_recruit$block)

# Make site and pop ordered factors
# Order sites interior, west edge, north edge, beyond west edge, beyond north edge
CfE3_recruit$site <- ordered(CfE3_recruit$site, levels = c("CERA", "RNHA", "SCRS", 
  "CPBS", "ACNW"))
# Order pops from south to north in latitude  
CfE3_recruit$pop <- ordered(CfE3_recruit$pop, levels = c("TYS", "KZA", "EGRE", 
  "CRA", "GCD"))

levels(CfE3_recruit$pop)

levels(CfE3_recruit$site)
table(CfE3_recruit$trt, CfE3_recruit$site)
# Correct number of treatments at each site 
table(CfE3_recruit$pop, CfE3_recruit$site)
# Correct number of pops at each site

summary(CfE3_recruit)
################################################################################

################################################################################
## Examine overall distribution of seedlings
hist(CfE3_recruit$seedlings2010, breaks = 50)
# VAST majority of patches have 0, short tail

# How many seedling in patches that should be empty?
CfE3_control <- subset(CfE3_recruit, plants.w.pods == 0, )
sum(CfE3_control$seedlings2010)/sum(CfE3_recruit$seedlings2010)
# Overall 31% of seedlings found in patches that SHOULD have been empty

## Examine seedling distribution by site
## CERA
CERAsub <- subset(CfE3_recruit, site == "CERA", )
CERAsub <- drop.levels(CERAsub)
str(CERAsub)
hist(CERAsub$seedlings2010, breaks = 50)

CERAsub.control <- subset(CERAsub, plants.w.pods == 0, )
hist(CERAsub.control$seedlings2010, breaks = 20)
# These are patches which 'should' have 0 if all dispersal was within 50cm

# What percent of seeds dispersed into patches that should be empty?
sum(CERAsub.control$seedlings2010)/ sum(CERAsub$seedlings2010)
# 36%...well over the 20% expected to be missing according to Fenster. 
# Though, this includes only those that are KNOWN to have and not any seeds that
# dispersed from one patch into a patch with plants. Also, does not take into
# account the empty area between patches...so likely an underestimate

## RNHA
RNHAsub <- subset(CfE3_recruit, site == "RNHA", )
RNHAsub <- drop.levels(RNHAsub)
str(RNHAsub)
windows()
hist(RNHAsub$seedlings2010, breaks = 50)

sum(RNHAsub[which(RNHAsub$plants.w.pods == 0), "seedlings2010"]) / sum(RNHAsub$seedlings2010)
# 22%

## SCRS
SCRSsub <- subset(CfE3_recruit, site == "SCRS", )
SCRSsub <- drop.levels(SCRSsub)
str(SCRSsub)
hist(SCRSsub$seedlings2010, breaks = 50)

sum(SCRSsub[which(SCRSsub$plants.w.pods == 0), "seedlings2010"]) / sum(SCRSsub$seedlings2010)
# 25%

## CPBS
CfE3_recruit[which(CfE3_recruit$site == "CPBS" & CfE3_recruit$seedlings2010 > 0), ]
# Good...1 patch with a seedling had pods in the previous year 
  
################################################################################  
## CERA and RNHA search and block area

# patch search area, diameter of search area .6 m
(patch.search.area <- pi * 0.6^2)

# total block search area, 20 experimental patches plus 1 control
(CERA.block.search.area <- patch.search.area * 21)
# 23.75 m2

# total site search area, 10 blocks
(CERA.site.search.area <- CERA.block.search.area * 10)
# 237 m2

# total block area: for row including control patch, 1.2 m * 7.2 m rectangle
# for remaining three rows, 3.6 m * 6 m rectangle
(CERA.block.area <- 1.2*7.2+3.6*6)
# 30.24 m2

# total block area, 10 blocks
CERA.site.block.area <- CERA.block.area * 10
# 302.4 m2

# At CERA, the percent of the total area where seeds dispersed that was searched:
CERA.site.search.area/CERA.site.block.area    # 79%
# and the total area that was not searched = 
CERA.block.area * (1-(CERA.site.search.area/CERA.site.block.area))
# 6.5 m not searched, 23.8 m searched

### ACNW and CPBS search and block area ###
# patch search area the same 

# total block search area, 10 experimental patches plus 1 control
(beyond.block.search.area <- patch.search.area * 11)
# 12.44 m2

# total block area: for top row including control patch (5 patches), 1.2 m * 4.8 m rectangle
# for last 2 rows (3 patches each), 2.4 m * 3.6 m rectangle
(beyond.block.area <- 1.2*4.8 + 2.4*3.6)
# 14.4 m2

### SCRS different block size because of space constraints ###
# patch search area the same 
# total block search area same as CERA
# total block area: four rows 1.2m * 4.8m, row with control patch 1.2 * 6.6m
(SCRS.block.area <- 1.2*6.6 + 4.8*4.8)
# 30.96 m2


################################################################################
## Calculate population growth rate for each block at each site

# Calculate number seedlings in 2009 in each block for all sites
sum.seedlings2009.site.block <- matrix(ncol = length(levels(CfE3_recruit$site)), nrow = 
  length(levels(CfE3_recruit$block)))
dimnames(sum.seedlings2009.site.block) <- list(levels(CfE3_recruit$block), levels(CfE3_recruit$site))

for(j in levels(CfE3_recruit$site)) {
  jsite <- levels(CfE3_recruit$site)[j]
  sub_site <- subset(CfE3_recruit, site == j, )
   
  for(i in levels(sub_site$block)) {
    iblock <- i
    sub_block <- subset(sub_site, block == iblock, )
    sum.seedlings2009.site.block[i,j] <- sum(sub_block$num.germ)
    }
  }

sum.seedlings2009.site.block

# Calculate number seedlings in 2010 in each block for all sites
sum.seedlings2010.site.block <- matrix(ncol = length(levels(CfE3_recruit$site)), nrow = 
  length(levels(CfE3_recruit$block)))
dimnames(sum.seedlings2010.site.block) <- list(levels(CfE3_recruit$block), levels(CfE3_recruit$site))

for(j in levels(CfE3_recruit$site)) {
  jsite <- levels(CfE3_recruit$site)[j]
  sub_site <- subset(CfE3_recruit, site == j, )
   
  for(i in levels(sub_site$block)) {
    iblock <- i
    sub_block <- subset(sub_site, block == iblock, )
    sum.seedlings2010.site.block[i,j] <- sum(sub_block$seedlings2010)
    }
  }

sum.seedlings2010.site.block

# Calculate population growth rate = seedlings2010/seedlings2009
lambda.bysiteblock <- sum.seedlings2010.site.block/sum.seedlings2009.site.block
lambda.bysite <- colMeans(lambda.bysiteblock)
lambda.bysite

# Calculate standard deviation for each site

sd.lambda.bysite <- sd(lambda.bysiteblock)

# Calculate standard error
se.lambda.bysite <- sd.lambda.bysite/sqrt(length(lambda.bysiteblock[,1]))

# Calculate 95% confidence intervals

lambda.95CIlower <- t(lambda.bysite) - qnorm(0.975) * t(se.lambda.bysite)
lambda.95CIupper <- t(lambda.bysite) + qnorm(0.975) * t(se.lambda.bysite)
lambda.95CI <- matrix(cbind(lambda.95CIlower, lambda.95CIupper), ncol = 2)
dimnames(lambda.95CI) <- list(levels(CfE3_recruit$site), c("lowerCI", "upperCI"))
round(lambda.95CI, 2)


## Repeat above calculations, but include unobserved seedlings in the part of
## each block that was not searched, assuming seedling density constant 
# Calculate unobserved seedlings in each block

unobserved.seedlings2010.site.block <- matrix(ncol = length(levels(CfE3_recruit$site)), 
  nrow = length(levels(CfE3_recruit$block)))
dimnames(unobserved.seedlings2010.site.block) <- list(levels(CfE3_recruit$block), 
  levels(CfE3_recruit$site))

# corresponding matrices with area searched in each block, and total area in 
# each block, by site
block.search.area <- c(rep(CERA.block.search.area, 30), rep(beyond.block.search.area, 20))
block.search.area <- matrix(block.search.area, ncol = length(levels(CfE3_recruit$site)), nrow = 
  length(levels(CfE3_recruit$block)))
dimnames(block.search.area) <- list(levels(CfE3_recruit$block), levels(CfE3_recruit$site))

block.area <- c(rep(CERA.block.area, 20), rep(SCRS.block.area, 10), 
  rep(beyond.block.area, 20))
block.area <- matrix(block.area, ncol = length(levels(CfE3_recruit$site)), nrow = 
  length(levels(CfE3_recruit$block)))
dimnames(block.area) <- list(levels(CfE3_recruit$block), levels(CfE3_recruit$site))

# loop across sites and blocks and calculate unobserved seedling in unsearched area 
for(j in levels(CfE3_recruit$site)) {
  jsite <- levels(CfE3_recruit$site)[j]
  sub_site <- subset(CfE3_recruit, site == j, )
  for(i in levels(sub_site$block)) {
    iblock <- i
    sub_block <- subset(sub_site, block == iblock, )
    unobserved.seedlings2010.site.block[i,j] <- sum.seedlings2010.site.block[i,j] * 
      (1-(block.search.area[i,j]/block.area[i,j]))
    }
  }

# add unobserved to observed 

total.seedlings2010.siteblock <- unobserved.seedlings2010.site.block + 
  sum.seedlings2010.site.block
  
# Repeat population growth calculations with these values
lambda2.bysiteblock <- total.seedlings2010.siteblock/sum.seedlings2009.site.block
lambda2.bysite <- colMeans(lambda2.bysiteblock)
lambda2.bysite

# Calculate standard deviation for each site

sd.lambda2.bysite <- sd(lambda2.bysiteblock)

# Calculate standard error
se.lambda2.bysite <- sd.lambda2.bysite/sqrt(length(lambda2.bysiteblock[,1]))

# Calculate 95% confidence intervals

lambda2.95CIlower <- t(lambda2.bysite) - qnorm(0.975) * t(se.lambda2.bysite)
lambda2.95CIupper <- t(lambda2.bysite) + qnorm(0.975) * t(se.lambda2.bysite)
lambda2.95CI <- matrix(cbind(lambda2.95CIlower, lambda2.95CIupper), ncol = 2)
dimnames(lambda2.95CI) <- list(levels(CfE3_recruit$site), c("lowerCI", "upperCI"))
round(lambda2.95CI, 2)

################################################################################

## Examine by competition treatment
## NOTE: calculations do not take into account disperal from one patch to another
## since treatment was applied in 2009, but recruitment recorded in 2010
## Differences between treatments could simply be due to random dispersal 
## into different patches

(num.germ.bysitetrt <- tapply(CfE3_recruit$num.germ, list(CfE3_recruit$trt,
  CfE3_recruit$site), sum, na.rm = T))
(num.seedlings.bysitetrt <- tapply(CfE3_recruit$seedlings2010, list(CfE3_recruit$trt,
  CfE3_recruit$site), sum, na.rm = T))

(lambda.bycomp <- num.seedlings.bysitetrt/num.germ.bysitetrt) 

lambda.compx15 <- lambda.bycomp * 1.15
lambda.compx20 <- lambda.bycomp * 1.20
lambda.compx25 <- lambda.bycomp * 1.25

pop.growth.comp.table <- cbind(t(num.germ.bysitetrt), t(num.seedlings.bysitetrt), 
  t(lambda.bycomp))

colnames(pop.growth.comp.table) <- c("germ.2009.C", "germ.2009.N", "seedlings.2010.C",
  "seedlings.2010.N", "lambda.C", "lambda.N")

t(round(pop.growth.comp.table, 2))
################################################################################



################################################################################
## Aster analysis of population growth rate
################################################################################
# Plots with seedlings that had no observed parents cannot be included in the aster
# analysis (no predecessor), so remove them from analysis
# This makes these results an underestimate. 

seedlings_data <- subset(CfE3_recruit, pop != "NULL", )
seedlings_data <- drop.levels(seedlings_data)
str(seedlings_data)

# Some patches have values for seedlings, though no plants.  
# For now, change to zero
length(which(seedlings_data$seedlings2010 > 0))
# 124 patches with recruitment

falseseedlings <- which(seedlings_data$plants.w.pods == 0 & seedlings_data$seedlings2010 > 0)
length(falseseedlings)
# 64...about 50% of patches with seedlings had no plant with pods!

seedlings_data[falseseedlings, ]
# mostly at CERA, some at RNHA and few at SCRS

# Change all to zero

seedlings_data$seedlings <- seedlings_data$seedlings2010

seedlings_data[falseseedlings, "seedlings"] <- 0
str(seedlings_data)

seedlings_data[falseseedlings, ]

# Check any plants.w.pods greater than num.germ
which(seedlings_data$plants.w.pods > seedlings_data$num.germ)
# None...good.

# Check any num.germ greater than 4
which(seedlings_data$num.germ > 4)
# None...good.

#################################################################################
# Set up aster models 

# check for NAs
sum(is.na(seedlings_data))
# 0

# Reshape

vars <- c("num.germ", "plants.w.pods", "seedlings")

reseedlings_data <- reshape(seedlings_data, varying = list(vars), direction = "long",
  timevar = "varb", times = as.factor(vars), v.names = "resp")
  
# Check reshape
(nrow(reseedlings_data))
(nrow(seedlings_data) * length(vars))
# Both 2400


## Specify aster model
# Add root of 4 to dataframe

reseedlings_data <- data.frame(reseedlings_data, root = 4)
head(reseedlings_data)

# Specify order of nodes
pred <- c(0,1,2)

# Specify conditional family distributions
# Use bernoulli for "num.germ" and "plants.w.pods", which is binomial 
# with sample size from previous stage
# Use poisson for seedlings

fam.default()

fam <- c(1,1,2)

sapply(fam.default(), as.character)[fam]

# Count individuals and nodes
nind <- length(unique(reseedlings_data$id))
nind # 800
nnode <- length(levels(reseedlings_data$varb))
nnode # 3

# Show graphical model
foo <- c("root", vars)
pvars <- foo[pred + 1]
bar <- cbind(pvars, vars)
colnames(bar) <- c("pred", "succ")
bar

# Add level for seedlings
seedlings <- grep("seedlings", as.character(reseedlings_data$varb))
seedlings <- is.element(seq(along = reseedlings_data$varb), seedlings)
reseedlings_data <- data.frame(reseedlings_data, seedlings = as.integer(seedlings))

# Fit model

aster1 <- aster(resp ~ varb, pred, fam, varb, id, root, data = reseedlings_data)
summary(aster1, show.graph = TRUE)
# Fits!

## Examine effect of site and treatment
## no population effect because it was not significant for fitness through pods
## in previous season, so unlikely to be important for dispersal, and because of
## error in dispersal estimate between patches, I'm suspicious of results 
## including this term 

# Fit full model specified at fitness level seedlings
aster2 <- aster(resp ~ varb + seedlings:site/block + seedlings:trt + seedlings:pop +
  seedlings:site:trt + seedlings:site:pop + seedlings:pop:trt, pred, fam, varb, id, 
  root, data = reseedlings_data)
summary(aster2, show.graph = TRUE, info.tol = 1e-15)
# NOTE high info.tol

# Test effect of block
aster3 <- aster(resp ~ varb + seedlings:site + seedlings:trt + seedlings:pop +
  seedlings:site:trt + seedlings:site:pop + seedlings:pop:trt, pred, fam, varb, id, 
  root, data = reseedlings_data)
summary(aster3, show.graph = TRUE, info.tol = 1e-14)

anova(aster3, aster2)
# Significant, dev = 194.  45 df = (10-1) blocks * (5-1) sites

# Test pop:trt
aster4 <- aster(resp ~ varb + seedlings:site/block + seedlings:trt + seedlings:pop +
  seedlings:site:trt + seedlings:site:pop, pred, fam, varb, id, root, data = reseedlings_data)
summary(aster4, show.graph = TRUE, info.tol = 1e-15)  
  
anova(aster4, aster2)
# Significant, dev = 19.  P = 0.0007. 4 df = (5-1)pop * (2-1) trt

# compare to model without block because of high info.tol
aster4a <- aster(resp ~ varb + seedlings:site + seedlings:trt + seedlings:pop +
  seedlings:site:trt + seedlings:site:pop, pred, fam, varb, id, root, data = reseedlings_data)
summary(aster4a, show.graph = TRUE, info.tol = 1e-14)  
  
anova(aster4a, aster3)
# Same result

# Test site:pop
aster5 <- aster(resp ~ varb + seedlings:site/block + seedlings:trt + seedlings:pop +
  seedlings:site:trt + seedlings:pop:trt, pred, fam, varb, id, root, data = reseedlings_data)
summary(aster5, show.graph = TRUE, info.tol = 1e-15)  
  
anova(aster5, aster2)
# Not significant. 16 df = (5-1)site * (5-1)pop 

# Test site:trt
aster6 <- aster(resp ~ varb + seedlings:site/block + seedlings:trt + seedlings:pop +
  seedlings:pop:trt, pred, fam, varb, id, root, data = reseedlings_data)
summary(aster6, show.graph = TRUE, info.tol = 1e-15)  
  
anova(aster6, aster5)
# Significant. 4 df = (5-1)site * (2-1)trt
 

# Fit model with main effects only
aster7 <- aster(resp ~ varb + seedlings:site/block + seedlings:trt + seedlings:pop, 
  pred, fam, varb, id, root, data = reseedlings_data)
summary(aster7, show.graph = TRUE, info.tol = 1e-15)

# Test effect of block
aster8 <- aster(resp ~ varb + seedlings:site + seedlings:trt + seedlings:pop, 
  pred, fam, varb, id, root, data = reseedlings_data)
summary(aster8, show.graph = TRUE, info.tol = 1e-12)

anova(aster8, aster7)
# block highly significant

# Test effect of pop
aster9 <- aster(resp ~ varb + seedlings:site/block + seedlings:trt, 
  pred, fam, varb, id, root, data = reseedlings_data)
summary(aster9, show.graph = TRUE, info.tol = 1e-15)

anova(aster9, aster7)
# pop signficant

# Test effect of trt
aster10 <- aster(resp ~ varb + seedlings:site/block + seedlings:pop, 
  pred, fam, varb, id, root, data = reseedlings_data)
summary(aster10, show.graph = TRUE, info.tol = 1e-15)

anova(aster10, aster7)
# trt not signficant, but interaction is so keep in model

# Test effect of site (against model without block)
aster11 <- aster(resp ~ varb + seedlings:pop + seedlings:trt, pred, fam, varb, id, root, data = reseedlings_data)
summary(aster11, show.graph = TRUE, info.tol = 1e-15)

anova(aster11, aster8)
# significant


## Final model is aster5. Fit without block for convenience of prediction
aster.final <- aster(resp ~ varb + seedlings:site + seedlings:trt + seedlings:pop +
  seedlings:site:trt + seedlings:pop:trt, pred, fam, varb, id, root, data = reseedlings_data)
summary(aster.final, show.graph = TRUE, info.tol = 1e-15) 

################################################################################
### Prediction
################################################################################

## Create new data to predict with
seedlings_newdata <- data.frame(site = rep(levels(seedlings_data$site), each = 5, times = 2))
seedlings_newdata$site <- ordered(seedlings_newdata$site, levels = c("CERA", "RNHA", "SCRS", "CPBS", "ACNW"))
seedlings_newdata$pop <- ordered(rep(c("TYS", "KZA", "EGRE", "CRA", "GCD"), 10), levels = 
  levels(seedlings_data$pop))
seedlings_newdata$trt <- factor(rep(c("competition", "removal"), each = 25), levels =
  levels(seedlings_data$trt))
  
for(v in vars) seedlings_newdata[[v]] <- 1

seedlings_newdata$root <- 4

# Reshape
re_seedlings_newdata <- reshape(seedlings_newdata, varying = list(vars), direction = "long",
  timevar = "varb", times = as.factor(vars), v.names = "resp")
  
# Add indicators for fitness level, seedlings
seedlings <- grep("seedlings", as.character(re_seedlings_newdata$varb))
seedlings <- is.element(seq(along = re_seedlings_newdata$varb), seedlings)
re_seedlings_newdata <- data.frame(re_seedlings_newdata, seedlings = as.integer(seedlings))
re_seedlings_newdata  


## Predict using newdata
seedlings.pout <- predict(aster.final, varvar = varb, idvar = id, root = root, 
  newdata = re_seedlings_newdata, se.fit = TRUE, info.tol = 1e-15)
  
seedlings.fit <- matrix(seedlings.pout$fit, ncol = 3)
colnames(seedlings.fit) <- c("germ09", "plants.w.pods", "seedlings10")
seedlings.sefit <- matrix(seedlings.pout$se.fit, ncol = 3)
colnames(seedlings.sefit) <- c("se.germ09", "se.plants.w.pods", "se.seedlings10")

# preliminary plot
seedlings.fit.table <- cbind(seedlings_newdata[,1:3], round(seedlings.fit, 2))
seedlings.fit.table

with(seedlings.fit.table, barchart(seedlings10 ~ pop|site))
with(seedlings.fit.table, barchart(seedlings10 ~ trt|site))

# Pattern of population differences is uninteresting...looks largely driven 
# by seed provisioning as pops tend to perform the same across sites
# For clarity of presentation, use predictions from model with site and trt only

aster.final2 <- aster(resp ~ varb + seedlings:site + seedlings:trt + seedlings:site:trt,
  pred, fam, varb, id, root, data = reseedlings_data)
summary(aster.final2, show.graph = TRUE, info.tol = 1e-14) 

## Create new data to predict with
seedlings_newdata2 <- data.frame(site = rep(levels(seedlings_data$site), times = 2))
seedlings_newdata2$site <- ordered(seedlings_newdata2$site, levels = c("CERA", "RNHA", "SCRS", "CPBS", "ACNW"))
seedlings_newdata2$trt <- factor(rep(c("competition", "removal"), each = 5), levels =
  levels(seedlings_data$trt))
  
for(v in vars) seedlings_newdata2[[v]] <- 1

seedlings_newdata2$root <- 4

# Reshape
re_seedlings_newdata2 <- reshape(seedlings_newdata2, varying = list(vars), direction = "long",
  timevar = "varb", times = as.factor(vars), v.names = "resp")
  
# Add indicators for fitness level, seedlings
seedlings <- grep("seedlings", as.character(re_seedlings_newdata2$varb))
seedlings <- is.element(seq(along = re_seedlings_newdata2$varb), seedlings)
re_seedlings_newdata2 <- data.frame(re_seedlings_newdata2, seedlings = as.integer(seedlings))
re_seedlings_newdata2  

## Predict using newdata
seedlings.pout2 <- predict(aster.final2, varvar = varb, idvar = id, root = root, 
  newdata = re_seedlings_newdata2, se.fit = TRUE, info.tol = 1e-14)
  
seedlings.fit2 <- matrix(seedlings.pout2$fit, ncol = 3)
colnames(seedlings.fit2) <- c("germ09", "plants.w.pods", "seedlings10")
seedlings.sefit2 <- matrix(seedlings.pout2$se.fit, ncol = 3)
colnames(seedlings.sefit2) <- c("se.germ09", "se.plants.w.pods", "se.seedlings10")

# preliminary plot
seedlings.fit2.table <- cbind(seedlings_newdata2[,1:2], round(seedlings.fit2, 2))
seedlings.fit2.table

with(seedlings.fit2.table, barchart(seedlings10 ~ trt|site))



################################################################################
## Transform aster results to calculate population growth rate

# Divide num seedlings / num germ 
aster.pop.growth <- seedlings.fit2.table[,5]/seedlings.fit2.table[,3]
round(aster.pop.growth, 2)

# Calculate confidence intervals for population growth rate using delta method 

  ## Test for first row of prediction only##
  j1 <- seedlings.fit2[1,3] 
  j2 <- 0
  j3 <- 1 / seedlings.fit2[1,1]

  jacobian <- as.vector(c(j1, j2, j3))

  # Extract the component gradient from output of predict function
  gradient <- seedlings.pout2$gradient
  dim(gradient)

  # Component fisher in the output of the aster function
  dim(aster.final2$fisher)

  # Solve for inverse fisher information
  sun <- gradient %*% solve(aster.final2$fisher) %*% t(gradient)
  dim(sun)

  # Find 3 x 3 submatrix of sun that corresponds to three elements of Jacobian
  
  moon <- seq(1, length(seedlings.fit2))
  moon <- matrix(moon, ncol = 3)
  moon 
  # elements packed by column
  
  star <- moon[1, 1:3]
  star
  
  sun.sub.i <- sun[star, ]
  sun.sub.i <- sun.sub.i[ ,star]
  sun.sub.i
  # 3x3 submatrix
  
  mu.sub.i <- as.vector(seedlings.fit2)[star]
  mu.sub.i

  lambda.sub.i <- mu.sub.i[3] / mu.sub.i[1]
  lambda.sub.i  # ~ 0, corresponds to pop growth estimate
  
  jacobian.sub.i <- c(mu.sub.i[3], 0, 1/mu.sub.i[1])
  
  # compute standard error for first row
  se.fit.i <- as.numeric(sqrt(rbind(jacobian) %*% sun.sub.i %*% cbind(jacobian)))
  # also zero
  ## Finish test for first row ##

## Calculate standard error for all rows
se.fit <- double(length(aster.pop.growth))
for(i in 1:length(aster.pop.growth)) {
   star <- moon[i, 1:3]
   sun.sub.i <- sun[star, ]
   sun.sub.i <- sun.sub.i[ ,star]
   mu.sub.i <- as.vector(seedlings.fit2)[star]
   jacobian.sub.i <- c(mu.sub.i[3], 0, 1/mu.sub.i[1])
   se.fit[i] <- as.numeric(sqrt(rbind(jacobian.sub.i) %*% sun.sub.i %*% cbind(jacobian)))
   }
   
# Table population growth estimates and standard errors
popgrowth.table <- cbind(seedlings.fit2.table[,1:2], round(aster.pop.growth, 2), 
  round(se.fit,4))
colnames(popgrowth.table)[3:4] <- c("lambda", "se.lambda")
popgrowth.table

# Calculate 95% confidence intervals
popgrowth.95CI <- cbind(aster.pop.growth - qnorm(0.975) * se.fit, 
  aster.pop.growth + qnorm(0.975) * se.fit)
popgrowth.95CI <- cbind(seedlings.fit2.table[,1:2], round(popgrowth.95CI, 3))
colnames(popgrowth.table)[3:4] <- c("lower.95CI", "upper.95CI")
popgrowth.95CI
