## Experimental populations: fitness and biotic interactions within and beyond the range
##
## Analysis for "Role of climate and competitors in limiting fitness across range edges of an annual plant" 
## by Stanton-Geddes, J., P. Tiffin and R. Shaw (2012) Ecology
##
## By John Stanton-Geddes, June 2011. All errors are my own. 
## Some exploratory analyses not recorded in the paper are included below.
##
## NOTE: the aster analyses were largely based on and modified from code available in the many aster technical reports
## by Charlie Geyer (www.stat.umn.edu/geyer/aster/). However, any errors in the code are solely my responsibility.

## Seeds were planted into 5 sites at different geographic range locations: the range interior (CERA), western edge (RNHA),
## northern edge (SCRS), beyond western edge (CPBS), beyond northern edge (ACNW). Because of poor germination,
## likely due to drought conditions in spring 2009, the first 5 blocks of the SCRS site were replanted in June
## and thus are included as a sixth site, SCRSb.
## Five plant seed source populations were used: Missouri (TYS), Kansas (KZA), Illinois (GRE), Iowa (CRA) and
## Minnesota (GCD).  Eighty seeds at the interior and edge sites, and forty seeds at they beyond
## edge sites were planted into 10 blocks at each site.  Within each block, 4 seeds from the same
## population were planted into experimental patches, with seeds located 20cm from each other
## in each patch, and a meter between patches.  The patches were randomly assigned to either
## natural competition (competition) or reduced competition by neighbor removal (removal).
## Early-season survival, reproductive stage, plant height and leaf number at the middle of the season, 
## and reproductive stage, height, and seed pod number were recorded.  
## In early summer 2010, I returned to each site and recorded the number of seedlings recruiting into the 1 meter 
## area surrounding each experimental patch.

## DATA ARE:
## esurv, early-season survival: 0 - dead/not present, 1 - alive
## july.stage, life-history stage in July: 
##      0 - dead/not present, 1 - vegetative, 2 - flowering, 3 - flower and pods, 4 - pods, 5 - completely senesced
## july.height, height in cm in July
## july.leaf, leaf number in July
## herbivory.category, percent category of leaf herbivory in July.
##      1: 0%, 2: 1-25%, 3: 26-75%, 4: 76-100%
##      For plants with <20 leaves, this was determined either by counting the 
##      number of leaves with herbivory, dividing by total leaves and placing in 
##      For plants with >20 leaves, the % category was visually estimated.
## disease.category, percent category for disease in July. values same as herbivory
## fall.stage: life-history stage at time of fall measurements, same as july.stage
## branch.number, branch number at end of season
## pods, number of pods (counting pedicels where pods had broken off)
## browsed.any, 0: plant not browsed, 1: plant browsed at some point in season
##      (integrates across observation of browsing in July or at end-of-season)
## 
##
## The specific questions to analyze are:
## 1) How do sites influence plant fitness?
## 2) How does the competition treatment influence plant fitness?  Does this vary with site?
## 3) How do populations differ in fitness within and between sites?
## 4) Does herbivory vary between sites and populations? Does it influence fitness?
## 5) Does disease vary between sites and populations? Does it influence fitness?
##



library(MASS)
library(gdata)
library(aster)
library(lattice)
library(bbmle)
library(plotrix)

# Preferences
options(show.signif.stars = FALSE)

################################################################################
# Data input and validation
################################################################################

CfE3data <- read.csv("CfE3_fitness_herbivory.csv", na.string = "?")

# Examine data

str(CfE3data)
levels(CfE3data$site)
table(CfE3data$trt, CfE3data$site)

# Change to factor
CfE3data$block <- as.factor(CfE3data$block)
CfE3data$patch <- as.factor(CfE3data$patch)
CfE3data$p.pos <- as.factor(CfE3data$p.pos)
CfE3data$fam <- as.factor(CfE3data$fam)
# Change to site and pop to ordered factor
# Order sites Beyond.W, W.Edge, Interior, N.Edge, Beyond.N
CfE3data$site <- ordered(CfE3data$site, levels = c("CPBS","RNHA", "CERA", "SCRS", "SCRSb",
   "ACNW"))
# Order pops from south to north in latitude  
CfE3data$pop <- ordered(CfE3data$pop, levels = c("TYS", "KZA", "GRE", "CRA", "GCD"))

# Change herbivory and disease.category to factors
CfE3data$herbivory.category <- as.factor(CfE3data$herbivory.category)
CfE3data$disease.category <- as.factor(CfE3data$disease.category)

### Create new variables

# Fall survival (yes/no) from fall stage.
CfE3data$fsurv <- ifelse(CfE3data$fall.stage > 0, 1, 0)

# Create column for "anypods"
CfE3data$anypods <- ifelse(CfE3data$seed.pods > 0, 1, 0)
# CfE3data[1000:1100,]
 
summary(CfE3data)
# 1 NA for p.pos
which(is.na(CfE3data$p.pos))
CfE3data[1829,]
# Mistake in planting - lost seed #1 in put second seed in position #2. Remove this entry from dataframe
CfE3data_clean <- CfE3data[-1829,]

# Check for missing data
which(is.na(CfE3data_clean$esurv))
CfE3data_clean[which(is.na(CfE3data_clean$esurv)), 1:12]
# These are all true missing data; seeds dropped while planting
# Remove from data frame
CfE3data_clean <- CfE3data_clean[-which(is.na(CfE3data_clean$esurv) != is.na(CfE3data_clean$id)),]
str(CfE3data_clean)

### Check for missing/incorrect data at later life-history stages

## Are there any plants with esurv == 0 but are alive later in the season?
check_fall.stage <- which(CfE3data_clean$esurv == 0 & CfE3data_clean$fall.stage > 0)
CfE3data_clean[check_fall.stage, 1:15]

# These are cases where a plant germinated after I first checked it.  Change esurv to 1
# The fact that they germinated late is captured in differences in later life-history stages

CfE3data_clean[which(CfE3data_clean$esurv == 0 & CfE3data_clean$fall.stage > 0), "esurv"] <- 1
CfE3data_clean[check_fall.stage, 1:15]

## Are there any plants with esurv == 0 but july.stage > 0?
check_july.stage <- which(CfE3data_clean$esurv == 0 & CfE3data_clean$july.stage > 0)
CfE3data_clean[check_july.stage, 1:15]

# Seeds that germinated after first check, but died before end of season. Change esurv to 1
CfE3data_clean[which(CfE3data_clean$esurv == 0 & CfE3data_clean$july.stage > 0), "esurv"] <- 1
CfE3data_clean[check_july.stage, 1:15]

## Are there any plants with july.stage == 0 but fall.stage > 0?
check_midseason.stage <- which(CfE3data_clean$july.stage == 0 & CfE3data_clean$fall.stage > 0)
CfE3data_clean[check_midseason.stage, 1:15]
# surprisingly, there are 12, though 9 are from SCRS which had very dry spring conditions...
# possibly causing seeds to wait to germinate. Also note that fall.stage == 1 for
# 9 of 12 so those that did germinate generally did not flower by end of season, and
# none produced pods

# Change july.stage to 1
CfE3data_clean[which(CfE3data_clean$july.stage == 0 & CfE3data_clean$fall.stage > 0), "july.stage"] <- 1
CfE3data_clean[check_midseason.stage, 1:15]
# Also need to change mid-season measurements to "NA"
CfE3data_clean[check_midseason.stage, "july.height"] <- NA
CfE3data_clean[check_midseason.stage, "july.leaf"] <- NA
CfE3data_clean[check_midseason.stage, "herbivory.category"] <- NA
CfE3data_clean[check_midseason.stage, "disease.category"] <- NA
CfE3data_clean[check_midseason.stage, 1:15]

# Are there any plants with fall.stage == 0 but have pods?
check_pods <- which(CfE3data_clean$fall.stage == 0 & CfE3data_clean$seed.pods > 0)
CfE3data_clean[check_pods,]
# None...good.

# Check distribution of pods
range(CfE3data_clean$seed.pods)
hist(CfE3data_clean$seed.pods)
# lots of zeros...
CfE3pods.data <- subset(CfE3data_clean, seed.pods > 0, )
str(CfE3pods.data)
# Only 196 plants out of 3600 planted produced pods

hist(CfE3pods.data$seed.pods)
# Probably should fit with a negative binomial distribution...looks over-dispersed for poisson
hist(rnbinom(200, 1, .05))

################################################################################
## Examine life-history stages by site 
################################################################################

## Examine esurv
(esurv.by.site.trt <- with(CfE3data_clean, tapply(esurv, list(trt, site), mean, 
  na.rm = TRUE)))
(esurv.by.pop.site <- with(CfE3data_clean, tapply(esurv, list(pop, site), mean, 
  na.rm = TRUE)))

## Examine fsurv
# overall
(overall.fsurv.by.site.trt <- with(CfE3data_clean, tapply(fsurv, list(trt, site), mean, 
  na.rm = TRUE)))  
# given esurv == 1
CfE3_esurv <- subset(CfE3data_clean, esurv == 1, )
str(CfE3_esurv)
(fsurv.by.site.trt <- with(CfE3_esurv, tapply(fsurv, list(trt, site), mean, 
  na.rm = TRUE))) 

## Examine anypods overall
(overall.anypods.by.site.trt <- with(CfE3data_clean, tapply(anypods, 
  list(trt, site), mean, na.rm = TRUE)))  
# Examine anypods given fsurv == 1   
CfE3_fsurv <- subset(CfE3data_clean, fsurv == 1, )
str(CfE3_fsurv)
(anypods.by.site.trt <- with(CfE3_fsurv, tapply(anypods, list(trt, site), mean, 
  na.rm = TRUE))) 

## Examine pods overall
# by site and pop 
(pods.by.site.pop <- with(CfE3data_clean, tapply(seed.pods, 
  list(site, pop), mean, na.rm = TRUE)))
# by site and trt
(pods.by.site.trt <- with(CfE3data_clean, tapply(seed.pods, 
  list(trt, site), mean, na.rm = TRUE)))
# examine pods produced given survival by site and treatment
(pods.by.site.pop.survived <- with(CfE3pods.data, tapply(seed.pods, list(pop, site),
  mean, na.rm = TRUE)))
(num.by.site.pop.survived <- table(CfE3pods.data$site,CfE3pods.data$pop))

(pods.by.site.trt.survived <- with(CfE3pods.data, tapply(seed.pods, list(trt, site),
  mean, na.rm = TRUE)))
# Calculate standard errors for this
# Number
(num.by.site.trt.survived <- table(CfE3pods.data$site,CfE3pods.data$trt))
# Standard deviation
(sd.by.site.trt.survived <- with(CfE3pods.data, tapply(seed.pods, list(site, trt), sd, na.rm = TRUE)))
# Calculate standard error
(se.by.site.trt.survived <- sd.by.site.trt.survived/sqrt(num.by.site.trt.survived))


################################################################################

# Examine correlation between height and seed pod production for plants that produced pods
plot(CfE3pods.data$july.height, CfE3pods.data$seed.pods)
# One plant has 0 recorded height, but has pods
CfE3pods.data[which(CfE3pods.data$july.height == 0 & CfE3pods.data$seed.pods > 0),]

missingheight <- which(CfE3pods.data$july.height == 0 & CfE3pods.data$seed.pods > 0)

# Missing height data.  Change from zero to NA
CfE3pods.data[missingheight, "july.height"] <- NA

CfE3pods.data[missingheight, ]

cor.test(CfE3pods.data$july.height, CfE3pods.data$seed.pods)
# r = 0.494
# r2 = 0.24
# not very high...

# Change missing height data to NA in full data

missingheight2 <- which(CfE3data_clean$july.height == 0 & CfE3data_clean$seed.pods > 0)
CfE3data_clean[missingheight2, "july.height"] <- NA

# Any fall.stage <2 that have pods?
missingfallstage <- which(CfE3data_clean$fall.stage < 2 & CfE3data_clean$seed.pods > 0)
CfE3data_clean[missingfallstage, ]
# Mistake in data entry, change to "3" for flowers and fruits
CfE3data_clean[missingfallstage, "fall.stage"] <- 3



################################################################################
## Set up aster models
################################################################################

# Remove SCRSb because it contributes minimal data points and complicates models
# Results are consistent if it is included (see ??????.r)
CfE3_aster <- subset(CfE3data_clean, site != "SCRSb", )
CfE3_aster <- drop.levels(CfE3_aster)
str(CfE3_aster)

CfE3_aster$site <- ordered(CfE3_aster$site, levels = c("CPBS", "RNHA", "CERA", "SCRS", "ACNW"))
CfE3_aster$pop <- ordered(CfE3_aster$pop, levels = c("TYS", "KZA", "GRE", "CRA", "GCD"))

# drop id
CfE3_aster <- subset(CfE3_aster, ,-id)
str(CfE3_aster)


# Check for NAs
sum(is.na(CfE3_aster)) # 0 NA

for(i in 1:length(CfE3_aster)) {
  nalist <- which(is.na(CfE3_aster[,i]))
  print(nalist)
  }

# Missing data for height, leaf #, herbivory and disease
# Drop these predictors for analysis of fitness
CfE3_aster2 <- subset(CfE3_aster, , select = -c(july.height, july.leaf, herbivory.category, disease.category))
str(CfE3_aster2)

# reshape

# Stages: early-season survival, survival to end-of-season, any pods produced, number pods
# any pods (bernoulli) and number seed.pods (zero-truncated negative binomial) split into two stages to facilitate modeling
vars <- c("esurv", "fsurv", "anypods", "seed.pods")

reCfE3 <- reshape(CfE3_aster2, varying = list(vars), direction = "long", timevar = "varb",
	times = as.factor(vars), v.names = "resp")
# Check reshape done correctly
(nrow(reCfE3))
(nrow(CfE3_aster)*length(vars))
# both 12708

# Specifiying aster model
# Add root to dataframe
reCfE3 <- data.frame(reCfE3, root = 1)
names(reCfE3)

# Specify order of nodes
pred <- c(0,1,2,3)

# Specify conditional family distributions
## Find size parameter for negative binomial distribution for seed.pods
# True distribution
hist(CfE3pods.data$seed.pods)

# Use fitdistr to estimate size parameters for negative binomial distribution of seed pods
(pods.param <- fitdistr(CfE3pods.data$seed.pods, "negative binomial")) # size = 0.94, mu = 9.07
windows()
hist(rnbinom(200, size = 0.94, mu = 9.1))
# alternate parameterization: prob = size/(size + mu)
.94/(9.1+.94)
windows()
hist(rnbinom(200, size = 0.94, prob = 0.09))

alpha.pods <- round(pods.param$estimate[1],2)

famlist <- list(fam.bernoulli(), fam.truncated.negative.binomial(size = alpha.pods,
  truncation = 0))

fam <- c(1,1,1,2)

sapply(famlist, as.character)[fam]

# Count individuals and nodes
nind <- length(unique(reCfE3$id))
nind # 3177
nnode <- length(levels(reCfE3$varb))
nnode # 4

# Show graphical model
foo <- c("root", vars)
pvars <- foo[pred + 1]
bar <- cbind(pvars, vars)
colnames(bar) <- c("pred", "succ")
bar

# Add levels of graphical model
esurv <- grep("esurv", as.character(reCfE3$varb))
esurv <- is.element(seq(along = reCfE3$varb), esurv)
reCfE3 <- data.frame(reCfE3, esurv = as.integer(esurv))

fsurv <- grep("fsurv", as.character(reCfE3$varb))
fsurv <- is.element(seq(along = reCfE3$varb), fsurv)
reCfE3 <- data.frame(reCfE3, fsurv = as.integer(fsurv))

anypods <- grep("anypods", as.character(reCfE3$varb))
anypods <- is.element(seq(along = reCfE3$varb), anypods)
reCfE3 <- data.frame(reCfE3, anypods = as.integer(anypods))

pods <- grep("seed.pods", as.character(reCfE3$varb))
pods <- is.element(seq(along = reCfE3$varb), pods)
reCfE3 <- data.frame(reCfE3, pods = as.integer(pods))

names(reCfE3)

################################################################################
# Fit aster models
#
################################################################################

# Fit null model to test

aster.null <- aster(resp ~ varb, pred, fam, varb, id, root, famlist = famlist,
  data = reCfE3)
summary(aster.null, show.graph = TRUE)

check.data <- reCfE3[reCfE3$id == 1, ]
check.data$resp <- 1
check.data

check.pout.null <- predict(aster.null, varvar = varb, idvar = id, root = root,
  newdata = check.data, model.type = "conditional")
check.pout.null

# Good!  Accurately estimates esurv, fsurv, anypods and pods averaged across all sites.

# Fit full model including blocks nested within site
aster1 <- aster(resp ~ varb + pods:site/block + pods:trt + pods:pop + 
  pods:site:trt + pods:site:pop + pods:trt:pop, pred, fam, varb, id, root, 
  famlist = famlist, data = reCfE3)
summary(aster1, show.graph = TRUE, info.tol = 1e-13)

################## Check goodness of fit #######################################
# Pods given anypods
xi.hat <- predict(aster1, model.type = "cond", parm.type = "mean")
xi.hat <- matrix(xi.hat, nrow = nrow(aster1$x), ncol = ncol(aster1$x))
theta.hat <- predict(aster1, model.type = "cond", parm.type = "canon")
theta.hat <- matrix(theta.hat, nrow = nrow(aster1$x), ncol = ncol(aster1$x))

woof <- CfE3_aster2$seed.pods[CfE3_aster2$anypods == 1]
length(woof) # 189
sum(woof == 0) # 189 plants produced at least one pod
range(woof)   # 1 to 98
hist(woof)

nwoof <- length(woof)
woof.theta <- theta.hat[CfE3_aster2$anypods == 1, 4]
woof.xi <- xi.hat[CfE3_aster2$anypods == 1, 4]
wgrad <- double(nwoof)
winfo <- double(nwoof)

for (i in 1:nwoof) {
   wgrad[i] <- famfun(famlist[[2]], deriv = 1, woof.theta[i])
   winfo[i] <- famfun(famlist[[2]], deriv = 2, woof.theta[i])
   }
all.equal(woof.xi, wgrad)

pearson <- (woof - woof.xi)/sqrt(winfo)

# Plot Pearson residuals against expected pod count given survival
windows()
plot(pearson ~ woof.xi)
# Ok...about 20 outliers above 2. positive skew okay because cannot have negative pod values so values biased upwards
################################################################################
## Test block
aster2 <- aster(resp ~ varb + pods:site + pods:trt + pods:pop + pods:site:trt + 
  pods:site:pop + pods:trt:pop, pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster2, show.graph = TRUE, info.tol = 1e-14)

anova(aster2, aster1)
# Significant effect of blocks, P < 0.0001.  45 df = 5 sites * (10-1)blocks

################################################################################
## Test interactions against full model

## Test trt:pop      
aster3 <- aster(resp ~ varb + pods:site/block + pods:trt + pods:pop + pods:site:trt + 
  pods:site:pop, pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster3, show.graph = TRUE, info.tol = 1e-15)

anova(aster3, aster1)
# Not significant, P = 0.18.  4 df = (5-1) pop * (2-1) trt

# repeat without including block
aster3a <- aster(resp ~ varb + pods:site + pods:trt + pods:pop + pods:site:trt + 
  pods:site:pop, pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster3a, show.graph = TRUE, info.tol = 1e-14)
anova(aster3a, aster2)
# also not significant, P = 0.22

## Test site:pop
aster4 <- aster(resp ~ varb + pods:site/block + pods:trt + pods:pop + pods:site:trt,
  pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster4, show.graph = TRUE, info.tol = 1e-15)

anova(aster4, aster3)
# Significant, P = 0.013.  16 df = (5-1)site * (5-1) pop
# without block
aster4a <- aster(resp ~ varb + pods:site + pods:trt + pods:pop + pods:site:trt,
  pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster4a, show.graph = TRUE, info.tol = 1e-13)

anova(aster4a, aster3a)
# also significant, P = 0.003

## Test site:trt
aster5 <- aster(resp ~ varb + pods:site/block + pods:trt + pods:pop + pods:site:pop,
  pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster5, show.graph = TRUE, info.tol = 1e-15)

anova(aster5, aster3)
# Highly significant, P < 0.0001.  4 df = (5-1) site * (2-1) trt

# without block
aster5a <- aster(resp ~ varb + pods:site + pods:trt + pods:pop + pods:site:pop,
  pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster5a, show.graph = TRUE, info.tol = 1e-14)

anova(aster5a, aster3a)
# also significant


################################################################################
# Note, because few individuals from each population survived to seed production:
table(CfE3pods.data$pop, CfE3pods.data$site)
# So, include pop as a covariate for remaining tests but acknowledge that tests
# of pop factor are unlikely to be significant
# test(see CfE3_29March.r)
################################################################################

# Test main effects without interactions in model
aster6 <- aster(resp ~ varb + pods:site + pods:trt + pods:pop,
  pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster6, show.graph = TRUE)

aster7 <- aster(resp ~ varb + pods:site + pods:pop,
  pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster7, show.graph = TRUE)

anova(aster7, aster6)
# Not significant, P = 0.58

aster8 <- aster(resp ~ varb + pods:site, pred, fam, varb, id, root, 
  famlist = famlist, data = reCfE3)
summary(aster8, show.graph = TRUE)

anova(aster8, aster7)
# Not significant, P = 0.43

aster9 <- aster(resp ~ varb, pred, fam, varb, id, root, 
  famlist = famlist, data = reCfE3)
summary(aster9, show.graph = TRUE)

anova(aster9, aster8)
# Significant, P < 0.0001

### NOTE: results are consistent if the effect of each predictor is not specified
### at pods, but entered individually. If this is done, info.tol does not have 
### to be raised


################################################################################
## To test the effect of each predictor at each level of varb, above and beyond
## the overall effect of that predictor on fitness (pods:predictor), build up 
## models by adding terms to base model (~ varb + pods:site + pods:pop + pods:trt)
## Forward step-wise model selection as opposed to deletion to avoid selecting
## an overparameterized model. 
## NOTE block not included in these models as including it (nested in site) 
## causes one level of site to be aliased
## HOWEVER when site/block is included all results that can be tested) are consistent
################################################################################

# Check full model
aster_varb <- aster(resp ~ varb + esurv:site + fsurv:site + anypods:site + pods:site +
  esurv:trt + fsurv:trt + anypods:trt + pods:trt + esurv:pop + fsurv:pop +
  anypods:pop + pods:pop, pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster_varb, show.graph = TRUE)

# check that this is the same as varb:site ...
aster_varba <- aster(resp ~ varb + varb:site + varb:trt + varb:pop,
  pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster_varba, show.graph = TRUE)

anova(aster_varba, aster_varb)
# Good, same model.

## Test main effects
aster_null <- aster(resp ~ varb, pred, fam, varb, id, root, famlist = famlist, 
  data = reCfE3)
summary(aster_null, show.graph = TRUE)

aster_site <- aster(resp ~ varb + pods:site, pred, fam, varb, id, root, famlist = famlist, 
  data = reCfE3)
summary(aster_site, show.graph = TRUE)

anova(aster_null, aster_site)
# Significant

aster_pop <- aster(resp ~ varb + pods:site + pods:pop, pred, fam, varb, id, root, 
  famlist = famlist, data = reCfE3)
summary(aster_pop, show.graph = TRUE)

anova(aster_site, aster_pop)
# Not significant, P = 0.43

aster_trt <- aster(resp ~ varb + pods:site + pods:pop + pods:trt, pred, fam, 
  varb, id, root, famlist = famlist, data = reCfE3)
summary(aster_trt, show.graph = TRUE)

anova(aster_pop, aster_trt)
# Not significant! P = 0.58

### Though main effects of pop and trt not significant, retain in model because
### of significant interactions

#Base model
aster_base <- aster(resp ~ varb + pods:site + pods:pop + pods:trt, 
  pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster_base, show.graph = TRUE)

### Test effect of SITE at each stage ###
# Test effect of SITE on esurv
aster_site.1 <- aster(resp ~ varb + pods:site + pods:pop + pods:trt + esurv:site, 
  pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster_site.1, show.graph = TRUE)

anova(aster_base, aster_site.1)
# Significant, P < 2e-16. 4 df = (5-1)pops * 1varb

# Test effect of SITE on fsurv
aster_site.2 <- aster(resp ~ varb + pods:site + pods:pop + pods:trt + esurv:site +
  fsurv:site, pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster_site.2, show.graph = TRUE)

anova(aster_site.1, aster_site.2)
# Significante, P < 2e-16

# Test effect of SITE on anypods
aster_site.3 <- aster(resp ~ varb + pods:site + pods:pop + pods:trt + esurv:site +
  fsurv:site + anypods:site, pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster_site.3, show.graph = TRUE)

anova(aster_site.2, aster_site.3)
# Significant, P < 0.0001

# Test effect of SITE on pods
aster_site.4 <- aster(resp ~ varb + pods:pop + pods:trt + esurv:site +
  fsurv:site + anypods:site, pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster_site.4, show.graph = TRUE)

anova(aster_site.4, aster_site.3)
# Significant. 4 df. P < 0.0001

### Test effect of POP at each stage ###

# Test effect of POP on esurv
aster_pop.1 <- aster(resp ~ varb + pods:site + pods:pop + pods:trt + 
  esurv:site + fsurv:site + anypods:site + esurv:pop, pred, fam, varb, id, root, 
  famlist = famlist, data = reCfE3)
summary(aster_pop.1, show.graph = TRUE)

anova(aster_site.3, aster_pop.1)
# Significant, P = 0.001.  4 df = (5-1)pop * 1varb

# Test effect of POP on fsurv
aster_pop.2 <- aster(resp ~ varb + pods:site + pods:pop + pods:trt + 
  esurv:site + fsurv:site + anypods:site + esurv:pop + fsurv:pop, pred, fam, 
  varb, id, root, famlist = famlist, data = reCfE3)
summary(aster_pop.2, show.graph = TRUE)

anova(aster_pop.1, aster_pop.2)
# Not significant, P = .24.  4 df

# Test effect of POP on anypods
aster_pop.3 <- aster(resp ~ varb + pods:site + pods:pop + pods:trt + 
  esurv:site + fsurv:site + anypods:site + esurv:pop + anypods:pop, 
  pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster_pop.3, show.graph = TRUE)

anova(aster_pop.1, aster_pop.3)
# Not significant, P = 0.11.  4 df

# Test effect of POP on seed.pods when esurv:pop included in model
aster_pop.4 <- aster(resp ~ varb + pods:site + pods:trt + 
  esurv:site + fsurv:site + anypods:site + esurv:pop, 
  pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster_pop.4, show.graph = TRUE)

anova(aster_pop.4, aster_pop.1)
# Not significant, P = 0.34.  4 df

### Test effect of TRT at each stage ###
# Test effect of TRT on esurv
aster_trt.1 <- aster(resp ~ varb + pods:site + pods:pop + pods:trt + 
  esurv:site + fsurv:site + anypods:site + esurv:pop + esurv:trt, pred, fam, 
  varb, id, root, famlist = famlist, data = reCfE3)
summary(aster_trt.1, show.graph = TRUE)

anova(aster_pop.1, aster_trt.1)
# Significant, P < 0.0001  1 df = (2-1)trt * 1 varb

# Test effect of TRT on fsurv
aster_trt.2 <- aster(resp ~ varb + pods:site + pods:pop + pods:trt + 
  esurv:site + fsurv:site + anypods:site + esurv:pop + esurv:trt + fsurv:trt, 
  pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster_trt.2, show.graph = TRUE)

anova(aster_trt.1, aster_trt.2)
# Not significant, P = 0.37

# Test effect of TRT on anypods
aster_trt.3 <- aster(resp ~ varb + pods:site + pods:pop + pods:trt + 
  esurv:site + fsurv:site + anypods:site + esurv:pop + esurv:trt + fsurv:trt +
  anypods:trt, pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster_trt.3, show.graph = TRUE)

anova(aster_trt.2, aster_trt.3)
# Significant, P = 0.002 df


################################################################################
## Test interactions step-wise forward for consistency with model building 

aster_int1 <- aster(resp ~ varb + pods:site + pods:trt + pods:pop,
  pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster_int1, show.graph = TRUE)

# Test site:pop
aster_int2 <- aster(resp ~ varb + pods:site + pods:trt + pods:pop +
  pods:site:pop, pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster_int2, show.graph = TRUE, info.tol = 1e-14)

anova(aster_int1, aster_int2)
# Significant, P = 0.0009. 16 df = (5-1)site * (5-1)pops

# Test site:trt
aster_int3 <- aster(resp ~ varb + pods:site + pods:trt + pods:pop +
  pods:site:pop + pods:site:trt, pred, fam, varb, id, root, famlist = famlist, data = reCfE3)
summary(aster_int3, show.graph = TRUE, info.tol = 1e-14)

anova(aster_int2, aster_int3)
# Significant, P < 0.0001. 4 df = (5-1)sites * (2-1) trt

# Test trt:pop
aster_int4 <- aster(resp ~ varb + pods:site + pods:trt + pods:pop +
  pods:site:pop + pods:site:trt + pods:pop:trt, pred, fam, varb, id, root, 
  famlist = famlist, data = reCfE3)
summary(aster_int4, show.graph = TRUE, info.tol = 1e-14)

anova(aster_int3, aster_int4) 
# Not significant, P = 0.22

## Results of stepwise forward consistent with stepwise deletion model selection

################################################################################
## Predicted values 
asterblock.pr <- predict(aster3, se.fit = TRUE, info.tol = 1e-14)
asterblock.fit <- asterblock.pr$fit
asterblock.sefit <- asterblock.pr$se.fit
asterblock.pr.data <- cbind(reCfE3, asterblock.fit, asterblock.sefit)

esurv.asterblock.fit <- subset(asterblock.pr.data, varb == "esurv")
fsurv.asterblock.fit <- subset(asterblock.pr.data, varb == "fsurv")
anypods.asterblock.fit <- subset(asterblock.pr.data, varb == "anypods")
pods.asterblock.fit <- subset(asterblock.pr.data, varb == "seed.pods")

esurv.asterblock <- with(esurv.asterblock.fit, tapply(asterblock.fit, 
  list(trt, site), mean))
round(esurv.asterblock, 2)
round(esurv.by.site.trt, 2)

fsurv.asterblock <- with(fsurv.asterblock.fit, tapply(asterblock.fit, list(trt, site), mean))
round(fsurv.asterblock, 2)
round(overall.fsurv.by.site.trt, 2)

anypods.asterblock <- with(anypods.asterblock.fit, tapply(asterblock.fit, list(trt, site), mean))
round(anypods.asterblock, 2)
round(overall.anypods.by.site.trt, 2)

pods.asterblock <- with(pods.asterblock.fit, tapply(asterblock.fit, list(trt, site), mean))
round(pods.asterblock, 2)
round(pods.by.site.trt, 2)

# Note that because esurv is not explicitly included in model, site:trt differences
# are not captured by the predicted values for this stage in the model, though
# overall fitness is accurately estimated


## Predict using model including all significant predictors at
## levels of varb
# Fit new model including these terms
aster.final <- aster(resp ~ varb + varb:site + esurv:trt + anypods:trt + pods:trt +
  esurv:pop + pods:site:pop + pods:site:trt, pred, fam, varb, id, root, 
  famlist = famlist, data = reCfE3)
summary(aster.final, show.graph = TRUE, info.tol = 1e-14)

## Predict
asterfinal.pr2 <- predict(aster.final, se.fit = TRUE, model.type = "unconditional", 
  info.tol = 1e-14)
  
asterfinal.fit <- asterfinal.pr2$fit
asterfinal.sefit <- asterfinal.pr2$se.fit
asterfinal.pr.data <- cbind(reCfE3, asterfinal.fit, asterfinal.sefit)

# separate predicted values for each level of varb
esurv.asterfinal.fit <- subset(asterfinal.pr.data, varb == "esurv")
fsurv.asterfinal.fit <- subset(asterfinal.pr.data, varb == "fsurv")
anypods.asterfinal.fit <- subset(asterfinal.pr.data, varb == "anypods")
pods.asterfinal.fit <- subset(asterfinal.pr.data, varb == "seed.pods")

# mean values by site and trt for each level of varb
esurv.asterfinal <- with(esurv.asterfinal.fit, tapply(asterfinal.fit, 
  list(trt, site), mean))
round(esurv.asterfinal, 2)
round(esurv.by.site.trt, 2)

fsurv.asterfinal <- with(fsurv.asterfinal.fit, tapply(asterfinal.fit, list(trt, site), mean))
round(fsurv.asterfinal, 2)
round(overall.fsurv.by.site.trt, 2)

anypods.asterfinal <- with(anypods.asterfinal.fit, tapply(asterfinal.fit, list(trt, site), mean))
round(anypods.asterfinal, 2)
round(overall.anypods.by.site.trt, 2)

pods.asterfinal <- with(pods.asterfinal.fit, tapply(asterfinal.fit, list(trt, site), mean))
round(pods.asterfinal, 2)
round(pods.by.site.trt, 2)

# standard errors for predicted mean values
se.esurv.asterfinal <- with(esurv.asterfinal.fit, tapply(asterfinal.sefit, 
  list(trt, site), mean))
se.fsurv.asterfinal <- with(fsurv.asterfinal.fit, tapply(asterfinal.sefit, 
  list(trt, site), mean))
se.anypods.asterfinal <- with(anypods.asterfinal.fit, tapply(asterfinal.sefit, 
  list(trt, site), mean))
se.pods.asterfinal <- with(pods.asterfinal.fit, tapply(asterfinal.sefit, 
  list(trt, site), mean))

## Plot
par(xpd = T)
plot1 <- barplot(pods.asterfinal, beside = T, legend = T, ylim = c(0,2), 
  ylab = "Lifetime pod production")
plotCI(plot1, pods.asterfinal, se.pods.asterfinal, add = T, pch = ".", gap = F)
mtext("West Edge", 1, line = 3, adj = 0.1)
arrows(7,-.2,2,-.2)
mtext("North Edge", 1, line = 3, adj = 0.9)
arrows(9,-.2,14,-.2)


### Get conditional predicted values
cond.asterfinal.pr2 <- predict(aster.final, se.fit = TRUE, 
  model.type = "conditional", info.tol = 1e-14)
  
cond.asterfinal.fit <- cond.asterfinal.pr2$fit
cond.asterfinal.sefit <- cond.asterfinal.pr2$se.fit
cond.asterfinal.pr.data <- cbind(reCfE3, cond.asterfinal.fit, cond.asterfinal.sefit)

# separate predicted values for each level of varb
cond.esurv.asterfinal.fit <- subset(cond.asterfinal.pr.data, varb == "esurv")
cond.fsurv.asterfinal.fit <- subset(cond.asterfinal.pr.data, varb == "fsurv")
cond.anypods.asterfinal.fit <- subset(cond.asterfinal.pr.data, varb == "anypods")
cond.pods.asterfinal.fit <- subset(cond.asterfinal.pr.data, varb == "seed.pods")

# Conditional predictions for esurv. These should be same as unconditional predictions
# as they are first node in graph
cond.esurv.asterfinal <- with(cond.esurv.asterfinal.fit, tapply(cond.asterfinal.fit, 
  list(trt, site), mean))
round(cond.esurv.asterfinal, 2)
# compare to unconditional predicted values
round(esurv.asterfinal, 2)
# same

# Conditional predictions for fsurv. 
## NOTE: for remaining stages, need to look at only predicted individuals with
## values greater than 0, as those with 0 values are not in the sample size for
## the conditional estimate given they were 0 in a previous stage
# look at only individuals with fsurv > 0
cond.pr.fsurv <- subset(cond.fsurv.asterfinal.fit, cond.fsurv.asterfinal.fit$cond.asterfinal.fit > 0, )
str(cond.pr.fsurv)
# 874 individuals 
cond.pr.fsurv.table <- with(cond.pr.fsurv, tapply(cond.asterfinal.fit, list(trt, site), mean))
round(cond.pr.fsurv.table, 2)
# compare to observed data
round(fsurv.by.site.trt, 2)
# very similar

# Conditional predictions for "anypods"
# look at only individuals with anypods > 0
cond.pr.anypods <- subset(cond.anypods.asterfinal.fit, cond.anypods.asterfinal.fit$cond.asterfinal.fit > 0, )
str(cond.pr.anypods)
# 502 individuals
cond.anypods.asterfinal <- with(cond.pr.anypods, tapply(cond.asterfinal.fit, list(trt, site), mean))
round(cond.anypods.asterfinal, 2)
# compare to observed data
round(anypods.by.site.trt, 2)
# very similar

# Conditional predictions for "pods"
# look at only individuals with pods >0
cond.pr.pods <- subset(cond.pods.asterfinal.fit, cond.pods.asterfinal.fit$cond.asterfinal.fit > 0, )
str(cond.pr.pods)
# Good: 189 observations is equal to the number of plants with pods in observed data
cond.pr.pods.table <- with(cond.pr.pods, tapply(cond.asterfinal.fit, list(trt,site), mean))
round(cond.pr.pods.table, 2)
# compare to observed data
round(pods.by.site.trt.survived, 2)
# Great! This is what I want. 

# Standard errors
cond.pr.esurv.se <- with(cond.esurv.asterfinal.fit, tapply(cond.asterfinal.sefit, list(trt, site), mean))
round(cond.pr.esurv.se, 2)

cond.pr.fsurv.se <- with(cond.pr.fsurv, tapply(cond.asterfinal.sefit, list(trt, site), mean))
round(cond.pr.fsurv.se, 2)

cond.pr.anypods.se <- with(cond.pr.anypods, tapply(cond.asterfinal.sefit, list(trt, site), mean))
round(cond.pr.anypods.se, 2)

cond.pr.pods.setable <- with(cond.pr.pods, tapply(cond.asterfinal.sefit, list(trt, site), mean))
round(cond.pr.pods.setable, 2)


### Figure 4
# Five panels for each life history stage: early-season survival, fall survival, whether a plant produce any pods or not, seed pods given survival, and lifetime pod production
tiff(filename="CfE3_Fig4.tiff", width = 152, height = 200, units = "mm", compression = "lzw", bg = "white", res = 600)
par(mar = c(1,4,1,2), oma = c(2,0,0,0), xpd = TRUE)
plotarea <- layout(matrix(c(1,2,3,4,5), 5, 1, byrow = TRUE))

xnames <- c("Interior", "W.Edge", "N.Edge", "Beyond.W", "Beyond.N")

p1_fitness <- barplot(pods.asterfinal, beside = T, ylim = c(0,2), 
  ylab = "Lifetime pods", xaxt = "n", las = 1, cex.lab = 1.3, col = c("black","white"))
plotCI(p1_fitness, pods.asterfinal, se.pods.asterfinal, add = T, pch = ".", gap = F)
legend("topleft", legend = c("Neighbors present", "Neighbors removed"), cex = 1.3,
  fill = c("black","white"), bty = "n")
segments(0.25,0,15,0)

p2_cond.pods <- barplot(cond.pr.pods.table, beside = T, ylim = c(0,25), 
  ylab = "Seed pods", xaxt = "n", cex.lab = 1.3, las = 1, col =c("black","white"))
plotCI(p2_cond.pods, cond.pr.pods.table, cond.pr.pods.setable, add = T, pch = ".", gap = F)
segments(0.25,0,15,0)

p3_cond.anypods <- barplot(cond.anypods.asterfinal, beside = T, ylim = c(0,1), 
  ylab = "Any pods", xaxt = "n", cex.lab = 1.3, las = 1, col=c("black","white"))
plotCI(p3_cond.anypods, cond.anypods.asterfinal, cond.pr.anypods.se, add = T, pch = ".", gap = F)
segments(0.25,0,15,0)

p3_cond.fsurv <- barplot(cond.pr.fsurv.table, beside = T, ylim = c(0,1), las = 1, 
  ylab = "Fall survival", cex.lab = 1.3, xaxt = "n", cex.names = 1.3, col=c("black","white"))
plotCI(p3_cond.fsurv, cond.pr.fsurv.table, cond.pr.fsurv.se, add = T, pch = ".", gap = F)
segments(0.25,0,15,0)

p5_esurv <- barplot(esurv.asterfinal, beside = T, ylim = c(0,0.8), las = 1, 
  ylab = "Early-season survival", xaxt = "n", cex.lab = 1.3, cex.names = 1.3, col=c("black","white"))
plotCI(p5_esurv, esurv.asterfinal, se.esurv.asterfinal, add = T, pch = ".", gap = F)
mtext(1, line = 1, text = xnames, at = c(2, 5.25, 8, 11, 14))
segments(0.25,0,15,0)

dev.off()


################################################################################
## BIOTIC INTERACTIONS ##
################################################################################

################################################################################
## Preliminary examination of differences in browsing between regions and pops

## Examine browsing on ALL data (0/1 = not browsed/browsed)
(browsed.table.bysitepop <- tapply(CfE3_aster$browsed, list(CfE3_aster$site, 
  CfE3_aster$pop), mean, na.rm = T))
(browsed.table.bysite <- tapply(CfE3_aster$browsed, CfE3_aster$site, 
  mean, na.rm = T))
(browsed.table.bypop <- tapply(CfE3_aster$browsed, CfE3_aster$pop, 
  mean, na.rm = T)) 

# Examine browsing on only plants that germinated 
(mean.browsed.bysitepop <- tapply(CfE3_esurv$browsed, list(CfE3_esurv$site, 
  CfE3_esurv$pop), mean, na.rm = T))
(mean.browsed.bysite <- tapply(CfE3_esurv$browsed, CfE3_esurv$site, 
  mean, na.rm = T))
(mean.browsed.bypop <- tapply(CfE3_esurv$browsed, CfE3_esurv$pop, 
  mean, na.rm = T)) 
# Browsing on 71% of plants in interior! 53% at west edge, but 0% beyond west, and 
# 20% north and beyond north. 
  


################################################################################
## Analysis of browsing. Logistic regression with browsed (0/1) as response
## and predictors site, population, treatment and site:treatment
################################################################################

# Use data for plants that survived to early-season, and thus had the opportunity 
# to be browsed
str(CfE3_esurv)
# un-cleaned data...create new dataframe

CfE3_esurv2 <- subset(CfE3_aster2, esurv == 1, )
str(CfE3_esurv2)
sum(is.na(CfE3_esurv2))

browse1 <- glm(browsed ~ site + pop + trt + site:trt, binomial, data = CfE3_esurv2)
summary(browse1)

browse2 <- glm(browsed ~ site + pop + trt + site:trt, binomial(link = "cloglog"), data = CfE3_esurv2)
summary(browse2)
# No improvement using complementary log-log link function. residual deviance = 941.7 compared to 942 with logit link

browse3 <- glm(browsed ~ site + pop + trt, binomial, data = CfE3_esurv2)
summary(browse3)

anova(browse3, browse2, test = "Chi")
# P = .92, not significant

browse4 <- glm(browsed ~ pop + trt, binomial, data = CfE3_esurv2)
summary(browse4)

anova(browse4, browse3, test = "Chi")
# P < 0.0001

browse5 <- glm(browsed ~ site + trt, binomial, data = CfE3_esurv2)
summary(browse5)

anova(browse5, browse3, test = "Chi")
# P = 0.03

browse6 <- glm(browsed ~ site + pop, binomial, data = CfE3_esurv2)
summary(browse6)

anova(browse6, browse3, test = "Chi")
# P < 0.0001


## Predict and plot

browsenames <- lapply(CfE3_esurv2[ ,c("site", "pop", "trt")], levels)
browse.pr <- predict(browse3, expand.grid(browsenames), type = "response",se.fit = TRUE)
(browse.pr.table <- cbind(expand.grid(browsenames), round(browse.pr$fit, 2), 
  round(browse.pr$se.fit,2)))
colnames(browse.pr.table)[4:5] <- c("browse.pr", "se.browse.pr")
browse.pr.table

(browse.pr.bysitetrt <- with(browse.pr.table, tapply(browse.pr, list(site, trt), 
  mean)))
(se.browse.pr.bysitetrt <- with(browse.pr.table, tapply(se.browse.pr, 
  list(site, trt), mean)))

sites <- c("Beyond.W", "W.Edge", "Interior", "N.Edge", "Beyond.N")
b <- barplot(t(browse.pr.bysitetrt), beside = T, legend = T, ylab = "fraction browsed", 
  ylim = c(0,1), las = 1, cex.lab = 1.3, cex.axis = 1.1, names.arg = sites)
plotCI(b, t(browse.pr.bysitetrt), t(se.browse.pr.bysitetrt), add = TRUE, 
  pch = ".", gap = FALSE)
  
# Look at differences between populations
(browse.pr.table.bysite <- browse.pr.table[order(browse.pr.table$site), ])


################################################################################
## Analysis of whether or not browsing, herbivory and disease influence seed production 
## at each site. See below for further analyses of herbivory and disease
################################################################################

# Create new dataframe for aster including herbivory and disease
CfE3_aster3 <- CfE3_aster
str(CfE3_aster3)

which(is.na(CfE3_aster3$july.leaf))
a <- which(is.na(CfE3_aster3$herbivory.category))
which(is.na(CfE3_aster3$disease.category))

# Change to "1" = 0% herbivory
CfE3_aster3[which(is.na(CfE3_aster3$herbivory.category)), "herbivory.category"] <- 1
CfE3_aster3[which(is.na(CfE3_aster3$disease.category)), "disease.category"] <- 1

# Remove remaining NA
CfE3_aster3 <- na.omit(CfE3_aster3) 

## Format for aster 
reCfE3v3 <- reshape(CfE3_aster3, varying = list(vars), direction = "long", timevar = "varb",
	times = as.factor(vars), v.names = "resp")
# Check reshape done correctly
(nrow(reCfE3v3))
(nrow(CfE3_aster3)*length(vars))
# both 12324

# Specifiying aster model
# Add root to dataframe
reCfE3v3 <- data.frame(reCfE3v3, root = 1)
names(reCfE3v3)

# Count individuals and nodes
nind <- length(unique(reCfE3v3$id))
nind # 3081
nnode <- length(levels(reCfE3v3$varb))
nnode # 4

# Show graphical model
foo <- c("root", vars)
pvars <- foo[pred + 1]
bar <- cbind(pvars, vars)
colnames(bar) <- c("pred", "succ")
bar

# Add level for "pods". Only concerned with effect of herbivory and disease on total fitness
pods <- grep("seed.pods", as.character(reCfE3v3$varb))
pods <- is.element(seq(along = reCfE3v3$varb), pods)
reCfE3v3 <- data.frame(reCfE3v3, pods = as.integer(pods))

names(reCfE3v3)


# Analysis site by site

# CERA
CERA.asterdata <- subset(reCfE3v3, site == "CERA", )
str(CERA.asterdata)

    CERA.aster1 <- aster(resp ~ varb + block + pods:trt + pods:pop + pods:herbivory.category +
    pods:disease.category + pods:browsed, pred, fam, varb, id, root, famlist = famlist, 
    data = CERA.asterdata)
    summary(CERA.aster1)
    CERA.aster2 <- aster(resp ~ varb + block + pods:trt + pods:pop + pods:herbivory.category +
    pods:disease.category, pred, fam, varb, id, root, famlist = famlist, 
    data = CERA.asterdata)
    anova(CERA.aster2, CERA.aster1)
    # Significant, P < 0.0001 
    
    CERA.aster3 <- aster(resp ~ varb + block + pods:trt + pods:pop + pods:herbivory.category +
    pods:browsed, pred, fam, varb, id, root, famlist = famlist, data = CERA.asterdata)
    anova(CERA.aster3, CERA.aster1)
    # Significant, P < 0.0001
    
    CERA.aster4 <- aster(resp ~ varb + block + pods:trt + pods:pop + pods:disease.category +
    pods:browsed, pred, fam, varb, id, root, famlist = famlist, data = CERA.asterdata)
    anova(CERA.aster4, CERA.aster1)
    # Not signficant, P = 0.27
    summary(CERA.aster4)
    
    # Significant effects of browsing and disease, but not foliear herbivory
  
# RNHA
RNHA.asterdata <- subset(reCfE3v3, site == "RNHA", )
str(RNHA.asterdata)

    RNHA.aster1 <- aster(resp ~ varb + block + pods:trt + pods:pop + pods:herbivory.category +
    pods:disease.category + pods:browsed, pred, fam, varb, id, root, famlist = famlist, 
    data = RNHA.asterdata)
    summary(RNHA.aster1)
    RNHA.aster2 <- aster(resp ~ varb + block + pods:trt + pods:pop + pods:herbivory.category +
    pods:disease.category, pred, fam, varb, id, root, famlist = famlist, 
    data = RNHA.asterdata)
    anova(RNHA.aster2, RNHA.aster1)
    # Not significant, P = 0.99
    
    RNHA.aster3 <- aster(resp ~ varb + block + pods:trt + pods:pop + pods:herbivory.category, 
    pred, fam, varb, id, root, famlist = famlist, data = RNHA.asterdata)
    anova(RNHA.aster3, RNHA.aster2)
    # Not significant, P = 0.47
    
    RNHA.aster4 <- aster(resp ~ varb + block + pods:trt + pods:pop, pred, fam, varb, id, 
    root, famlist = famlist, data = RNHA.asterdata)
    anova(RNHA.aster4, RNHA.aster3)
    # Signficant, P < 0.0001
    summary(RNHA.aster3)
    
    
# SCRS
SCRS.asterdata <- subset(reCfE3v3, site == "SCRS", )
str(SCRS.asterdata)

    SCRS.aster1 <- aster(resp ~ varb + block + pods:trt + pods:pop + pods:herbivory.category +
    pods:disease.category + pods:browsed, pred, fam, varb, id, root, famlist = famlist, 
    data = SCRS.asterdata)
    summary(SCRS.aster1, info.tol = 1e-15)
    # NOTE high info.tol
    SCRS.aster2 <- aster(resp ~ varb + block + pods:trt + pods:pop + pods:herbivory.category +
    pods:disease.category, pred, fam, varb, id, root, famlist = famlist, 
    data = SCRS.asterdata)
    anova(SCRS.aster2, SCRS.aster1)
    # Not significant, P = 0.59
    
    SCRS.aster3 <- aster(resp ~ varb + block + pods:trt + pods:pop + pods:herbivory.category, 
    pred, fam, varb, id, root, famlist = famlist, data = SCRS.asterdata)
    anova(SCRS.aster3, SCRS.aster2)
    # Significant, P < 0.0001
    
    SCRS.aster4 <- aster(resp ~ varb + block + pods:trt + pods:pop + pods:disease.category, 
    pred, fam, varb, id, root, famlist = famlist, data = SCRS.asterdata)
    anova(SCRS.aster4, SCRS.aster2)
    # Not signficant, P = 0.18
    summary(SCRS.aster4, info.tol = 1e-15)
    
# CPBS
CPBS.asterdata <- subset(reCfE3v3, site == "CPBS", )
str(CPBS.asterdata)
# NOTE. no plants browsed so cannot test this factor

    CPBS.aster1 <- aster(resp ~ varb + block + pods:trt + pods:pop + pods:herbivory.category +
    pods:disease.category, pred, fam, varb, id, root, famlist = famlist, 
    data = CPBS.asterdata)
    summary(CPBS.aster1, info.tol = 1e-12)
    CPBS.aster2 <- aster(resp ~ varb + block + pods:trt + pods:pop + pods:disease.category, 
    pred, fam, varb, id, root, famlist = famlist, data = CPBS.asterdata)
    anova(CPBS.aster2, CPBS.aster1)
    # Not significant, P = 0.12
    
    CPBS.aster3 <- aster(resp ~ varb + block + pods:trt + pods:pop, pred, fam, varb, id, 
    root, famlist = famlist, data = CPBS.asterdata)
    anova(CPBS.aster3, CPBS.aster2)
    # Significant, P < 0.0001
    summary(CPBS.aster2, info.tol = 1e-12)
    
# ACNW
ACNW.asterdata <- subset(reCfE3v3, site == "ACNW", )
str(ACNW.asterdata)

    ACNW.aster1 <- aster(resp ~ varb + block + pods:trt + pods:pop + pods:herbivory.category +
    pods:disease.category + pods:browsed, pred, fam, varb, id, root, famlist = famlist, 
    data = ACNW.asterdata)
    summary(ACNW.aster1, info.tol = 1e-12)
    ACNW.aster2 <- aster(resp ~ varb + block + pods:trt + pods:pop + pods:herbivory.category +
    pods:disease.category, pred, fam, varb, id, root, famlist = famlist, 
    data = ACNW.asterdata)
    anova(ACNW.aster2, ACNW.aster1)
    # Not significant, P = 0.99
    
    ACNW.aster3 <- aster(resp ~ varb + block + pods:trt + pods:pop + pods:herbivory.category, 
    pred, fam, varb, id, root, famlist = famlist, data = ACNW.asterdata)
    anova(ACNW.aster3, ACNW.aster2)
    # Not significant, P = 0.26
    
    ACNW.aster4 <- aster(resp ~ varb + block + pods:trt + pods:pop, 
    pred, fam, varb, id, root, famlist = famlist, data = ACNW.asterdata)
    anova(ACNW.aster4, ACNW.aster3)
    # Not signficant, P = 0.93
    summary(ACNW.aster4, info.tol =1e-12)


#
################################################################################
## Analyses of herbivory and disease between populations and sites
## NOTE - results below not included in Ecology paper, but included in PhD thesis
################################################################################

## Preliminary examination of data

# Examine only plants that survived early-season (esurv == 1)
str(CfE3_fsurv)
# 874 individuals
CfE3_esurv$site <- ordered(CfE3_esurv$site, levels = c("CPBS", "RNHA", "CERA", "SCRS", "ACNW"))

# Subset to only plants that survived to July, when data was recorded for herbivory and disease
CfE3_july <- subset(CfE3_aster, july.stage > 0, )
str(CfE3_july)
# 748 
CfE3_july$site <- ordered(CfE3_july$site, levels = c("CPBS", "RNHA", "CERA", "SCRS", "ACNW"))

## Examine herbivory (categories 1-5)
(mean.herbivory.bysite <- tapply(as.numeric(CfE3_july$herbivory.category), CfE3_july$site, 
  mean, na.rm = T))
(mean.herbivory.bypop <- tapply(as.numeric(CfE3_july$herbivory.category), CfE3_july$pop, 
  mean, na.rm = T))  
# herbivory varies 2x by site, but little by pop.
# site order: west edge > beyond west > north edge > interior > beyond north
plot(CfE3_july$herbivory.category ~ CfE3_july$site)
# shows greater herbivory as you go west (drier)

# Examine differences between pops within sites
(mean.herbivory.bysitepop <- tapply(as.numeric(CfE3_july$herbivory.category), list(CfE3_july$pop, 
  CfE3_july$site), mean, na.rm = T)) 
normalized.herb.bysitepop <- mean.herbivory.bysitepop 

for(i in 1:ncol(normalized.herb.bysitepop)) { 
  print(mean(normalized.herb.bysitepop[,i]))
  for(j in 1:length(normalized.herb.bysitepop[,1])) {
    normalized.herb.bysitepop[j,i] <- (mean.herbivory.bysitepop[j,i] - mean(mean.herbivory.bysitepop[ ,i]))/
    sd(mean.herbivory.bysitepop[ ,i])
    }
  }
normalized.herb.bysitepop 
# No pop has consistently higher or lower herbivory across sites
# At interior site, edge pops (GCD and KZA) have lower than average herbivory
# GRE and TYS pop have greater than average, CRA just about average
# but overall, no clear pop differentiation 

## Examine disease (categories 1-5)
(mean.disease.bysite <- tapply(as.numeric(CfE3_july$disease.category), CfE3_july$site, 
  mean, na.rm = T))
(mean.disease.bypop <- tapply(as.numeric(CfE3_july$disease.category), CfE3_july$pop, 
  mean, na.rm = T)) 
plot(CfE3_july$disease.category ~ CfE3_july$site)  
# similar to herbivory, large differences between sites but not pops within sites
# site order: beyond north > north edge > beyond west > interior > west edge
# Interesting; opposite of herbivory, disease is less in western sites and greater north
# makes sense if arid conditions less conducive to fungal growth? 

(mean.disease.bysitepop <- tapply(as.numeric(CfE3_july$disease.category), list(CfE3_july$pop, 
  CfE3_july$site), mean, na.rm = T))
normalized.disease.bysitepop <- mean.disease.bysitepop 

for(i in 1:ncol(normalized.disease.bysitepop)) { 
  for(j in 1:length(normalized.disease.bysitepop[,1])) {
    normalized.disease.bysitepop[j,i] <- (mean.disease.bysitepop[j,i] - mean(mean.disease.bysitepop[ ,i]))/
    sd(mean.disease.bysitepop[ ,i])
    }
  }
normalized.disease.bysitepop 
# TYS always has less disease than average, CRA and GCD have more than average at 3 of 5 sites
 
### Check effect of herbivory on pod production
with(CfE3_july, xyplot(seed.pods ~ herbivory.category | site))
with(CfE3_july, tapply(seed.pods, list(herbivory.category, site), mean, na.rm = T))

# Disease
with(CfE3_july, xyplot(seed.pods ~ disease.category | site))
with(CfE3_july, tapply(seed.pods, list(disease.category, site), mean, na.rm = T))
  
# Are these correlated?
cor.test(as.numeric(CfE3_july$herbivory.category), as.numeric(CfE3_july$disease.category))
# marginally significant weak positive correlation...so NO


#################################################################################
# Analysis of herbivory category. Use POLR - proportional odds logistic regression
# to fit logistic regression to ordered factor response
##################################################################################

herbivory.polr1 <- polr(herbivory.category ~ site + pop + trt + pop:trt + site:trt + 
  site:pop, data = CfE3_july)
summary(herbivory.polr1)

herbivory.polr2 <- update(herbivory.polr1,~. - site:pop)
anova(herbivory.polr2, herbivory.polr1)
# P = 0.74, drop

herbivory.polr3 <- update(herbivory.polr2,~. - pop:trt)
anova(herbivory.polr3, herbivory.polr2)
# P = 0.09, drop

herbivory.polr4 <- update(herbivory.polr3,~. - site:trt)
anova(herbivory.polr4, herbivory.polr3)
# P = 0.003, retain

herbivory.polr5 <- update(herbivory.polr3,~. - pop)
anova(herbivory.polr5, herbivory.polr3)
# P = 0.07, drop

# Final model
summary(herbivory.polr5)

# Get predicted values

herbnames <- lapply(CfE3_july[ , c("site","trt")], levels)
herbivory.pr <- predict(herbivory.polr5, expand.grid(herbnames), type = "probs")
(herb.pr.bysitetrt <- cbind(expand.grid(herbnames), round(herbivory.pr, 2)))

herb.pr.comp <- subset(herb.pr.bysitetrt, trt == "competition", )
herb.pr.rem <- subset(herb.pr.bysitetrt, trt == "removal", )

par(xpd = T, mar = c(5.1, 4.1, 4.1, 7.1))
barplot(t(as.matrix(herb.pr.comp[1:5,3:6])), names.arg = sites, 
  yaxt = "n", cex.lab = 1.3)
mtext(side = 2, line = 2.5, "% plants in each", cex = 1.3)
mtext(side = 2, line = 1.5, "herbivory category (NR)", cex = 1.3)
legend("topright", c("0%", "1-25%", "26-75%", "76-100%"), fill = gray.colors(5))

windows()
par(xpd = T, mar = c(5.1, 4.1, 4.1, 7.1))
barplot(t(as.matrix(herb.pr.rem[1:5,3:6])), names.arg = sites, 
  yaxt = "n", cex.lab = 1.3)
mtext(side = 2, line = 2.5, "% plants in each", cex = 1.3)
mtext(side = 2, line = 1.5, "herbivory category (NP)", cex = 1.3)
legend("topright", c("0%", "1-25%", "26-75%", "76-100%"), fill = gray.colors(5))

## Overall pattern the same, so make one plot for each site (not showing treatment effect)

herbivory.polr.plot <- polr(herbivory.category ~ site, data = CfE3_july)
summary(herbivory.polr.plot)

sitenames <- herbnames[1]
herb.pr2 <- predict(herbivory.polr.plot, expand.grid(sitenames), type = "probs")
(herb.pr.bysite <- cbind(expand.grid(sitenames), round(herb.pr2, 2)))

windows()
par(xpd = T, mar = c(5.1, 4.1, 4.1, 7.1))
barplot(t(as.matrix(herb.pr.bysite[1:5,2:5])), names.arg = sites, 
  yaxt = "n", cex.lab = 1.3)
mtext(side = 2, line = 2.5, "% plants in each", cex = 1.3)
mtext(side = 2, line = 1.5, "herbivory category", cex = 1.3)
legend("topright", c("0%", "1-25%", "26-75%", "76-100%"), fill = gray.colors(5))



################################################################################
### Analysis of disease category. Use POLR - proportional odds logistic regression
# to fit logistic regression to ordered factor response
#################################################################################

disease.polr1 <- polr(disease.category ~ site + pop + trt + site:pop + site:trt +
  pop:trt, data = CfE3_july)
summary(disease.polr1)

disease.polr2 <- update(disease.polr1,~. - site:pop)
anova(disease.polr2, disease.polr1)
# P = 0.79, drop

disease.polr3 <- update(disease.polr2,~. - pop:trt)
anova(disease.polr3, disease.polr2)
# P = 0.23, drop

disease.polr4 <- update(disease.polr3,~. - site:trt)
anova(disease.polr4, disease.polr3)
# P = 0.0497, retain

disease.polr5 <- update(disease.polr3,~. - pop)
anova(disease.polr5, disease.polr3)
# P = 0.21, drop

# Test main effects of site and treatment
disease.polr6 <- update(disease.polr4,~. - pop)
disease.polr7 <- update(disease.polr6,~. - site)

anova(disease.polr7, disease.polr6)
# site highly significant


# Final model
summary(disease.polr5)

# Get predicted values

disease.pr <- predict(disease.polr5, expand.grid(herbnames), type = "probs")
(disease.pr.bysite <- cbind(expand.grid(herbnames), round(disease.pr, 2)))

# as with herbivory, little effect of treatment so plot without treatment
disease.polr.plot <- polr(disease.category ~ site, data = CfE3_july)
summary(disease.polr.plot)

disease.pr2 <- predict(disease.polr.plot, expand.grid(sitenames), type = "probs")
(disease.pr.bysite <- cbind(expand.grid(sitenames), round(disease.pr2, 2)))

par(xpd = T, mar = c(5.1, 4.1, 4.1, 7.1))
barplot(t(as.matrix(disease.pr.bysite[1:5,2:5])), names.arg = sites, 
  yaxt = "n", cex.lab = 1.3)
mtext(side = 2, line = 2.5, "% plants in each", cex = 1.3)
mtext(side = 2, line = 1.5, "disease category", cex = 1.3)
legend("topright", c("0%", "1-25%", "26-75%", "76-100%"), fill = gray.colors(5))
