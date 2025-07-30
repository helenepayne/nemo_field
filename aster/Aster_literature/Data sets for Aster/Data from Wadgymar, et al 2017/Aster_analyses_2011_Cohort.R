GV11<-read.delim(".../Aster_analyses_2011_Cohort.txt", header=T)



##Season should be a factor
G11$season<-as.factor(G11$season)


##Check variables
sapply(GV11,class)
str(GV11)


##Load libraries
library(aster)
library(MASS)
library(tidyr)
library(lme4)


##standardize trait values
GV11$sSLA<-scale(GV11$SLA,center=TRUE, scale=TRUE)

GV11$sdeltaC<-scale(GV11$deltaC13,center=TRUE, scale=TRUE)

GV11$sheight<-scale(GV11$height,center=TRUE, scale=TRUE)

GV11$sFDsnow<-scale(GV11$FDsnow,center=TRUE, scale=TRUE)

GV11$elev<-scale(GV11$elevation,center=TRUE, scale=TRUE)

##Create scaled quadratic elevation variable
GV11$elev2<-GV11$elev^2



###Add some columns to our data set for the aster analyses

##A vector of length nrow that specifies the type of node each row corersponds to - flr, fecund, survived, or begin
layer <- gsub("[0-9]", "", as.character(GV11 $varb))
unique(layer)


##Creates a vector called fit where all of the fecund node types get a 1 and everything else gets a 0
fit <- as.numeric(layer == "fecund")

 ##Creates a vector called surv where all of the begin node types get a 1 and everything else gets a 0
surv<-as.numeric(layer == "begin")

##Creates a vector called flower where all of the flr node types get a 1 and everything else gets a 0
flower<-as.numeric(layer == "flr")

##Add these new columns to dataset
GV11<-data.frame(GV11, layer, fit, surv, flower)

##A few checks
with(GV11, class(layer)) #make sure it's a factor
with(GV11, class(fit)) #Make sure it is numeric
with(GV11, class(surv)) #Should be numeric
with(GV11, class(flower)) #Should be numeric



















#########################################################################################
## Full model 
### This code runs the aster models that we need for  linear cumulative selection ## 
 #########################################################################################
 
 # Use fitdistr to estimate size parameters for negative binomial distribution of fecundity (silique length). I've tried other distributions (Poisson, normal, separate negative binoomial for each season), but this seems to fit the best.

##Subsets for rows preaining to fecundity data
GV11_fecund<-GV11[grep("^fecund", GV11$varb),]

##Calculates starting values for Maximum Likelihood fitting of negative binomial, rounds to 2 digits ## size represents the ovedispersion parameter (the shape parameter) and mu is the mean ## This next step generates warning messages that I don't think are problematic ##
fit.param <- fitdistr(GV11_fecund$resp, "negative binomial")
alpha.fit <- round(fit.param $estimate[1],2)
##Histogram of response variable (silique length)
#hist(GV11_fecund$resp)



######Structure of aster model
##Variables that correspond to nodes of the graph
vars <-c("flr12", "fecund12", "survived12","begin13","flr13","fecund13","survived13","begin14","flr14", "fecund14","survived14")

##Vector for graphical strucure listed in vars - initial nodes is 0
pred <- c(0,1,0,3,4,5,4,7,8,9,8)

##Check structure
all(pred<seq(along=pred)) #Must be true
foo <- rbind(vars, c("initial", vars)[pred + 1])
rownames(foo) <- c("successor", "predecessor")
t(foo)



##The two distributions used in our aster model corresponding to order of vars
famlist <- list(fam.bernoulli(), fam.negative.binomial(size = alpha.fit))

fam <- c(1,2,1,1,1,2,1,1,1,2,1)

##Check
cbind(vars, fam)






## This is the linear model - We are specifically interested in fit:sSLA, fit:sdeltaC, fit:sFDsnow, an fit:sheight. If you are interested in why this is the way that models are formed, you can  google "aster No naked predictors." Essentailly "fit" represents overall fitness (fecundity, conditioned on survival) for each year, so we need to estimate selection by including that term in the model. Using something like "fit" is a relatively new recommendation from the aster creators, so older publications will not have done this.
 
##Linear model
linear<- aster(resp~varb+fit:(sSLA+sdeltaC+sFDsnow+sheight), pred, fam, famlist=famlist, varb, id, root, data = GV11)

summary(linear)

##Alternative models with elevation as covariate
linearE<- aster(resp~varb+fit:(elev+sSLA+sdeltaC+sFDsnow+sheight), pred, fam, famlist=famlist, varb, id, root, data = GV11)

linearE2<- aster(resp~varb+fit:(elev+elev2+sSLA+sdeltaC+sFDsnow+sheight), pred, fam, famlist=famlist, varb, id, root, data = GV11)

##We will go with the model that includes elevation
anova(linear, linearE, linearE2)



##Checking for significance of each linear term (for neg bin model)
linearE_SLA<- aster(resp ~varb+fit: (elev+sdeltaC + sFDsnow + sheight), pred, fam, famlist=famlist,varb, id, root, data = GV11)

linearE_deltaC<- aster(resp ~varb+fit: (elev+sSLA + sFDsnow + sheight), pred, fam, famlist=famlist,varb, id, root, data = GV11)

linearE_FDsnow<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sheight), pred, fam, famlist=famlist,varb, id, root, data = GV11)

linearE_height<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow ), pred, fam, famlist=famlist,varb, id, root, data = GV11)

anova(linearE_SLA,linearE)
anova(linearE_deltaC,linearE)
anova(linearE_FDsnow,linearE) #Sig
anova(linearE_height,linearE) #Sig



 
 
 
 
 
######## To extract selection gradients #########
 
###linear model##
#first, we extract the predicted fitness values from our model "linear" and arrange them nicely
pout<-matrix(predict(linearE), nrow=nrow(linearE$x), ncol=ncol(linearE$x))
colnames(pout)<-colnames(linearE$x)
 
##Now we use those predicted values to calculate predicted overall fitness for each year. We need a data frame with geno, trait values, and predicted values 

##Add predicted data to our observed data without repeating rows 
GV11_lifetime<-data.frame(GV11[1:19,], pout)

##Add columns for the fitness each genotype achieved in each year - (num flowered/number at start of season) * length fruit produced
GV11_lifetime$fit12<-(GV11_lifetime$flr12/GV11_lifetime$root)*GV11_lifetime$fecund12

GV11_lifetime$fit13<-(GV11_lifetime$flr13/GV11_lifetime$survived12)*GV11_lifetime$fecund13

GV11_lifetime$fit14<-(GV11_lifetime$flr14/GV11_lifetime$survived13)*GV11_lifetime$fecund14

##delete columns - all we need are geno and fit12:fit14
GV11_lifetime <-GV11_lifetime[,-c(1:2,4:31)] 

## Converte from wide to long format, labeling fit12:fit14 as Expected_fitness ##
GV11_lifetime <- gather(GV11_lifetime, season, Expected_fitness, fit12:fit14)

#Relabel season to reflect just the year (numeric)
levels(GV11_lifetime $season)[levels(GV11_lifetime $season)=="fit12"] <- "2012"

levels(GV11_lifetime $season)[levels(GV11_lifetime $season)=="fit13"] <- "2013"

levels(GV11_lifetime $season)[levels(GV11_lifetime $season)=="fit14"] <- "2014"

##We need to add in trait data 
#Subset the rows for the three years of fecundity data (ensures that we don't repeat rows with the same data) and keep only trait and elev data
GV11_fecund<-GV11[grep("fecund", GV11$varb),]

##Remove columns that aren't needed - keep geno, season, sandardized traits, fitness, and elev
GV11_fecund <-GV11_fecund[,-c(1, 8:11, 13,20:23)] 

##Merge data sets - Fitness is from observed fecundity data and fitness is predicted from aster 
predicted_fit <- merge(GV11_lifetime, GV11_fecund, by=c("season","geno"))

###Calculate relative fitness based on aster predicted fitness##
 predicted_fit$w_expected<- predicted_fit$Expected_fitness/mean(predicted_fit$Expected_fitness)
 
 
###Calculate observed relative fitness
predicted_fit$w_obs<-predicted_fit$Fitness/mean(predicted_fit$Fitness)


 
 
##Run the OLS model, from which we can extract selection gradients - the selection gradient should be correct, but the SE will not be. 
wmout<-lmer(w_expected ~elev+sSLA+sdeltaC+sFDsnow+sheight+season+(1|geno),data= predicted_fit) 

summary(wmout)
















###quadratic model for overall fitness###
### This code runs the aster models that we need for  nonlinear cumulative selection ## 

###Quadratic model
quadratic <- aster(resp ~varb+fit:(sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ) , pred, fam,famlist=famlist, varb, id, root, data = GV11)

summary(quadratic)

##Alternative models with elevation as covariate
quadraticE <- aster(resp ~varb+fit:(elev+sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ) , pred, fam,famlist=famlist, varb, id, root, data = GV11)

quadraticE2 <- aster(resp ~varb+fit:(elev+elev2+sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ) , pred, fam,famlist=famlist, varb, id, root, data = GV11)

#Adding elevation does not improve model fit
anova(quadratic, quadraticE, quadraticE2)
 
 
##Checking for significance of quadratic terms
q_SLA <- aster(resp ~varb+fit:(elev+sSLA +sdeltaC+ sFDsnow+sheight+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ) , pred, fam,famlist=famlist, varb, id, root, data = GV11)
 
q_deltaC <- aster(resp ~varb+fit:(elev+sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+I(sFDsnow^2) + I(sheight^2) ) , pred, fam,famlist=famlist, varb, id, root, data = GV11)

q_FDsnow <- aster(resp ~varb+fit:(elev+sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+ I(sdeltaC^2) + I(sheight^2) ) , pred, fam,famlist=famlist, varb, id, root, data = GV11)

q_height <- aster(resp ~varb+fit:(elev+sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) ) , pred, fam,famlist=famlist, varb, id, root, data = GV11)

anova(q_SLA, quadraticE) #Sig
anova(q_deltaC, quadraticE)
anova(q_FDsnow, quadraticE)
anova(q_height, quadraticE)




######## To extract selection gradients #########

#first, we extract the predicted fitness values from our model "quadratic" and arrange them nicely
pout2<-matrix(predict(quadraticE), nrow=nrow(quadraticE$x), ncol=ncol(quadraticE $x))
colnames(pout2)<-colnames(quadraticE$x)

##Now we use those predicted values to calculate predicted overall fitness for each year. We need a data frame with geno, trait values, and predicted values, and calculate fitness (fecundity * (flowering success)/#plants)

##Add predicted data to our observed daa
GV11_lifetimeQ<-data.frame(GV11[1:19,], pout2)

##Add columns for the fitness each genotype achieved in each year - (num flowered/number at start of season) * length fruit produced
GV11_lifetimeQ $fit12<-(GV11_lifetimeQ $flr12/GV11_lifetimeQ $root)* GV11_lifetimeQ $fecund12

GV11_lifetimeQ $fit13<-(GV11_lifetimeQ $flr13/GV11_lifetimeQ $survived12)* GV11_lifetimeQ $fecund13

GV11_lifetimeQ $fit14<-(GV11_lifetimeQ $flr14/GV11_lifetimeQ $survived13)* GV11_lifetimeQ $fecund14

#delete columns  - all we need are geno and fit12:fit14
GV11_lifetimeQ <-GV11_lifetimeQ[,-c(1:2,4:33)] 

##Convert from wide to long format, labeling fit12:fit14 as Expected_fitness ##
GV11_lifetimeQ <- gather(GV11_lifetimeQ, season, Expected_fitness, fit12:fit14)

##Relabel season to reflect just the year (numeric)
levels(GV11_lifetimeQ$season)[levels(GV11_lifetimeQ $season)=="fit12"] <- "2012"

levels(GV11_lifetimeQ$season)[levels(GV11_lifetimeQ $season)=="fit13"] <- "2013"

levels(GV11_lifetimeQ$season)[levels(GV11_lifetimeQ $season)=="fit14"] <- "2014"


##We need to add in trait data 
#Subset the rows for the three years of fecundity data (ensures that we don't repeat rows with the same data) and keep only trait and elev data
GV11_fecund<-GV11[grep("fecund", GV11$varb),]

##Remove columns that aren't needed - keep geno, season, sandardized traits, fitness, and elev
GV11_fecund<-GV11_fecund[,-c(1, 8:11, 13,20:23)] 

##Merge data sets - Fitness is from observed fecundity data and fitness is predicted from aster
predicted_fitQ <- merge(GV11_lifetimeQ, GV11_fecund, by=c("season","geno"))

#Calculate relative fitness based on aster predicted fitness
predicted_fitQ$w_expected<- predicted_fitQ$Expected_fitness/mean(predicted_fitQ $Expected_fitness)

#Calculate observed relative fitness 
 predicted_fitQ$w_obs<-predicted_fitQ$Fitness/mean(predicted_fitQ$Fitness)


##Run OLS model, from which we can exract selection gradients (but the SE will not be correct)
wmoutQ<-lmer(w_expected ~elev+sSLA+sdeltaC+sFDsnow+sheight+ I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2)+season+(1|geno),data= predicted_fitQ) 

summary(wmoutQ)























  
 
 
 
 
 
 

#########################################################################################
 #####This second aster model is to estimate linear selection via fecundity only (no predecessors) 
 ### I've tried Poisson and normal distributions. Negative binomial seems to be the best. 
 #########################################################################################

##Subset rows pertaining to fecundity data
GV11_fecund<-GV11[grep("^fecund", GV11$varb),]

##Variables that correspond to the nodes of the graph
varsF <-c("fecund12","fecund13", "fecund14")

##Vector for graphical structure listed in vars (simple in this case)
predF <- c(0,0,0)
##Check structure
all(predF<seq(along=predF)) #Must be true
foo <- rbind(varsF, c("initial", varsF)[predF + 1])
rownames(foo) <- c("successor", "predecessor")
t(foo)

##Calculate starting values for ML fitting of negative binomial and round to 2 digits (ignore warnings)
fit.param <- fitdistr(GV11_fecund$resp, "negative binomial")
alpha.fit <- round(fit.param $estimate[1],2)

##Set up distributions for aster model 
famlistF <- list(fam.negative.binomial(size = alpha.fit))
famF <- c(1,1,1)
##Check
cbind(varsF, famF)

##A vector of length nrow that specifices the type of node each row corresponds to (all the same in this case)
layer <- gsub("[0-9]", "", as.character(GV11_fecund $varb))
unique(layer)







##Aster model
linear_fecund<- aster(resp ~varb+fit: (sSLA+sdeltaC + sFDsnow + sheight), predF, famF, famlist=famlistF, varb, id, root, data = GV11_fecund)

summary(linear_fecund)

##Alternative models with elevation as covariate
linear_fecundE<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight), predF, famF, famlist=famlistF, varb, id, root, data = GV11_fecund)

linear_fecundE2<- aster(resp ~varb+fit: (elev+elev2+sSLA+sdeltaC + sFDsnow + sheight), predF, famF, famlist=famlistF, varb, id, root, data = GV11_fecund)

#Adding elev and elev2 does improve model fit
anova(linear_fecund, linear_fecundE, linear_fecundE2)


##Checking for significance of each linear term (with elev and elev2)
l_sla_fecund<- aster(resp ~varb+fit: (elev+elev2+sdeltaC + sFDsnow + sheight), predF, famF, famlist=famlistF,varb, id, root, data = GV11_fecund)

l_deltaC_fecund<- aster(resp~varb+fit: (elev+elev2+sSLA + sFDsnow + sheight), predF, famF, famlist=famlistF,varb, id, root, data = GV11_fecund)

l_fd_fecund<- aster(resp ~varb+fit: (elev+elev2+sSLA+sdeltaC + sheight), predF, famF, famlist=famlistF,varb, id, root, data = GV11_fecund)

l_height_fecund<- aster(resp ~varb+fit: (elev+elev2+sSLA+sdeltaC + sFDsnow ), predF, famF, famlist=famlistF,varb, id, root, data = GV11_fecund)

anova(l_sla_fecund, linear_fecundE2)
anova(l_deltaC_fecund, linear_fecundE2)
anova(l_fd_fecund, linear_fecundE2) #Sig
anova(l_height_fecund, linear_fecundE2) #Sig



######################################
####To extract betas for linear models ####

#first, we extract the predicted fitness values from our model "linear_fecundE2" and arrange them nicely
pout_fecund <-matrix(predict(linear_fecundE2), nrow=nrow(linear_fecundE2$x), ncol=ncol(linear_fecundE2$x))
colnames(pout_fecund)<-colnames(linear_fecundE2$x)

#Create a vector of fecundity values from the three years
dim(pout_fecund) <- c(dim(pout_fecund)[1]*dim(pout_fecund)[2], 1)

##Add predicted data
GV11_fecundity<-as.data.frame(cbind(GV11_fecund, pout_fecund))

##Delete columns that are not needed
GV11_fecundity <-GV11_fecundity[,-c(8:9,20:23)]
colnames(GV11_fecundity)[10] <- "observed_fecundity"
colnames(GV11_fecundity)[18] <- "expected_fecundity"

#Calculate relative fitness (expected relative fitness)
GV11_fecundity$w_expected<- GV11_fecundity $expected_fecundity/mean(GV11_fecundity$expected_fecundity)
 
#Season needs to be turned into a factor
GV11_fecundity$season <- factor(GV11_fecundity $season)

#Observed relative fitness
GV11_fecundity$w_obs<-GV11_fecundity$observed_fecundity/mean(GV11_fecundity$observed_fecundity)




####The OLS model with elev and elev2
wmoutfE2<-lmer(w_expected ~elev+elev2+sSLA+sdeltaC+sFDsnow+sheight+season+(1|geno),data= GV11_fecundity) 

summary(wmoutfE2)


 
 
 
 
 
 
 
 
 

#Nonlinear selection via fecundity 
quadratic_fecund <- aster(resp ~varb+fit:(sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ) , predF, famF, famlist=famlistF,varb, id, root, data = GV11_fecund)

summary(quadratic_fecund)

##Alternative models with elevation as covariate
quadratic_fecundE <- aster(resp ~varb+fit:(elev+sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ) , predF, famF, famlist=famlistF,varb, id, root, data = GV11_fecund)

quadratic_fecundE2 <- aster(resp ~varb+fit:(elev+elev2+sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ) , predF, famF, famlist=famlistF,varb, id, root, data = GV11_fecund)

##Adding elev and elev2 improves model fit
anova(quadratic_fecund, quadratic_fecundE, quadratic_fecundE2)



##Check significance of each trait
q_SLAf <- aster(resp ~varb+fit:(elev+elev2+sSLA +sdeltaC+ sFDsnow+sheight+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ) , predF, famF, famlist=famlistF,varb, id, root, data = GV11_fecund)

q_deltaCf <- aster(resp ~varb+fit:(elev+elev2+sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+I(sFDsnow^2) + I(sheight^2) ) , predF, famF, famlist=famlistF,varb, id, root, data = GV11_fecund)

q_FDsnowf <- aster(resp ~varb+fit:(elev+elev2+sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+ I(sdeltaC^2) + I(sheight^2) ) ,predF, famF, famlist=famlistF,varb, id, root, data = GV11_fecund)

q_heightf <- aster(resp ~varb+fit:(elev+elev2+sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) ) , predF, famF, famlist=famlistF,varb, id, root, data = GV11_fecund)

anova(q_SLAf, quadratic_fecundE2) #Sig
anova(q_deltaCf, quadratic_fecundE2) #Sig
anova(q_FDsnowf, quadratic_fecundE2) #Sig
anova(q_heightf, quadratic_fecundE2) 
 


####To extract betas for quadratic models ####

#first, we extract the predicted fitness values from our model "quadratic_fecund" and arrange them nicely
pout2_fecund <-matrix(predict(quadratic_fecundE2), nrow=nrow(quadratic_fecundE2$x), ncol=ncol(quadratic_fecundE2$x))
colnames(pout2_fecund)<-colnames(quadratic_fecundE2$x)

#Create a vector of fecundity values from the three years
dim(pout2_fecund) <- c(dim(pout2_fecund)[1]*dim(pout2_fecund)[2], 1)


##Add predicted data
GV11_fecundityQ<-as.data.frame(cbind(GV11_fecund, pout2_fecund))

##Delete columns that are not needed
GV11_fecundityQ <-GV11_fecundityQ[,-c(8:11, 13, 20:23)] 
colnames(GV11_fecundityQ)[8] <- "observed_fecundity"
colnames(GV11_fecundityQ)[15] <- "expected_fecundity"

#Calculate expected relative fitness
GV11_fecundityQ$w_expected<- GV11_fecundityQ $expected_fecundity/mean(GV11_fecundityQ $expected_fecundity)

##Calculate observed realtive fitness
GV11_fecundityQ$w_obs<-GV11_fecundityQ$observed_fecundity/mean(GV11_fecundityQ$observed_fecundity)


##Season needs to be turned into a factor
GV11_fecundityQ$season <- factor(GV11_fecundityQ$season)


##The model
wmout_fqE2<-lmer(w_expected ~elev+elev2+sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2)+season+(1|geno), data=GV11_fecundityQ)

summary(wmout_fqE2)





 











































#########################################################################################
 #####This third aster model is for estimating linear selection via viability (e.g.flowering success) and does not include fecundity #####
 #########################################################################################
 
##Subsets for rows preaining to flowering success
GV11_fecund<-GV11[-grep("^fecund", GV11$varb),]
GV11_flr<-GV11_fecund[-grep("^survived", GV11_fecund$varb),]

##Season should be numeric
GV11_flr$season<-as.factor(GV11_flr$season)


######Structure of aster model
##Variables tha correspond to nodes of the graph
varsV <-c("flr12","begin13","flr13","begin14","flr14")
##Vector for graphical strucure listed in vars - initial nodes is 0
predV <- c(0,0,2,2,4)
##Check structure
all(predV<seq(along=predV)) #Should be true

##The distribution used in our aster model corresponding to order of vars
famlistV <- list(fam.bernoulli())
famV <- c(1,1,1,1,1)

 
 
 
 
##The model
linear_flr<- aster(resp ~varb+ flower: (sSLA+sdeltaC + sFDsnow + sheight), predV, famV, famlist=famlistV,varb, id, root, data = GV11_flr)

summary(linear_flr)

##Alternative models with elevation as covariate
linear_flrE<- aster(resp ~varb+ flower: (elev+sSLA+sdeltaC + sFDsnow + sheight), predV, famV, famlist=famlistV,varb, id, root, data = GV11_flr)

linear_flrE2<- aster(resp ~varb+ flower: (elev+elev2+sSLA+sdeltaC + sFDsnow + sheight), predV, famV, famlist=famlistV,varb, id, root, data = GV11_flr)

#Adding elev and elev2 improves model fit
anova(linear_flr, linear_flrE, linear_flrE2)



##Checking for significance of each linear term 
l_sla_flr<- aster(resp ~varb+ flower: (elev+elev2+sdeltaC + sFDsnow + sheight), predV, famV, famlist=famlistV,varb, id, root, data = GV11_flr)

l_deltaC_flr<- aster(resp ~varb+ flower: (elev+elev2+sSLA + sFDsnow + sheight), predV, famV, famlist=famlistV,varb, id, root, data = GV11_flr)

l_fd_flr<- aster(resp ~varb+ flower: (elev+elev2+sSLA+sdeltaC + sheight), predV, famV, famlist=famlistV,varb, id, root, data = GV11_flr)

l_height_flr<- aster(resp ~varb+ flower: (elev+elev2+sSLA+sdeltaC + sFDsnow ), predV, famV, famlist=famlistV,varb, id, root, data = GV11_flr)

anova(l_sla_flr, linear_flrE2) 
anova(l_deltaC_flr, linear_flrE2) #Sig
anova(l_fd_flr, linear_flrE2)
anova(l_height_flr, linear_flrE2) #Sig



######################################
####To extract betas for linear models ####

#first, we extract the predicted fitness values from our model "linear_fecund" and arrange them nicely
pout_flr <-round(matrix(predict(linear_flrE2), nrow=nrow(linear_flrE2$x), ncol=ncol(linear_flrE2$x)))
colnames(pout_flr)<-colnames(linear_flrE2$x)


##Now I need to calculate flowering success (flr/begin)
started<-c(GV11$root[1:19], pout_flr[,2], pout_flr[,4])

flowered<-c(pout_flr[,1], pout_flr[,3], pout_flr[,5])

veg12<-GV11$root[1:19]-pout_flr[,1]
veg13<-pout_flr[,2]-pout_flr[,3]
veg14<-pout_flr[,4]-pout_flr[,5]

notflowered<-c(veg12, veg13, veg14)

prop<-flowered/(flowered+notflowered)

#Checks - should be the same
cbind(flowered+notflowered, started)

cbind(prop, c(pout_flr[,1]/GV11$root[1:19], pout_flr[,3]/pout_flr[,2], pout_flr[,5]/pout_flr[,4]))

##Add data
GV11_flr2<-GV11[grep("flr", GV11 $varb),]
GV11_flr2<-data.frame(GV11_flr2, started, flowered, notflowered, prop)

##Make sure season is numeric
GV11_flr2$season<-as.factor(GV11_flr2$season)

#Calculate expected relative fitness
GV11_flr2$w_expected<-GV11_flr2$prop/mean(GV11_flr2$prop) 

##Calculate observed relative fitness
GV11_flr2$w_obs<-GV11_flr2$flower_success/mean(GV11_flr2$flower_success)


####The model
w_logisticE2<-glmer(cbind(flowered ,notflowered)~elev+elev2+sSLA +sdeltaC+ sFDsnow+sheight+season+(1|geno),family=binomial, data=GV11_flr2)

summary(w_logisticE2)









 
 
 
#########################
##Nonlinear selection via viability
quadratic_flr<- aster(resp ~ varb+flower:(sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ) , predV, famV, famlist=famlistV, varb, id, root, data = GV11_flr)
 

##Alternative models with elevation as covariate
quadratic_flrE <- aster(resp ~ varb+flower:(elev+sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ) , predV, famV, famlist=famlistV, varb, id, root, data = GV11_flr)
 
quadratic_flrE2 <- aster(resp ~ varb+flower:(elev+elev2+sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ) , predV, famV, famlist=famlistV, varb, id, root, data = GV11_flr)
 
 ##Adding elev and elev2 improves model fit
anova(quadratic_flr, quadratic_flrE, quadratic_flrE2)


##Checking for significance of each non-linear term
q_SLAflr <- aster(resp ~varb+flower:(elev+elev2+sSLA +sdeltaC+ sFDsnow+sheight+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ) ,predV, famV, famlist=famlistV, varb, id, root, data = GV11_flr)

q_deltaCflr <- aster(resp ~varb+ flower:(elev+elev2+sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+I(sFDsnow^2) + I(sheight^2) ) , predV, famV, famlist=famlistV, varb, id, root, data = GV11_flr)

q_FDsnowflr <- aster(resp ~varb+ flower:(elev+elev2+sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+ I(sdeltaC^2) + I(sheight^2) ) ,predV, famV, famlist=famlistV, varb, id, root, data = GV11_flr)

q_heightflr <- aster(resp ~varb+ flower:(elev+elev2+sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) ) , predV, famV, famlist=famlistV, varb, id, root, data = GV11_flr)

anova(q_SLAflr, quadratic_flrE2) #Sig.
anova(q_deltaCflr, quadratic_flrE2) #Sig.
anova(q_FDsnowflr, quadratic_flrE2) #Sig.
anova(q_heightflr, quadratic_flrE2) #Sig.
 
 
 
 
####To extract betas for quadratic models ####
#first, we extract the predicted fitness values and arrange them nicely
pout_flrQ <-round(matrix(predict(quadratic_flrE2), nrow=nrow(quadratic_flrE2$x), ncol=ncol(quadratic_flrE2$x)))
colnames(pout_flrQ)<-colnames(quadratic_flrE2 $x)

##Now I need to calculate flowering success (flr/begin)
##Round first, then calculate
started<-c(GV11$root[1:19], pout_flrQ[,2], pout_flrQ[,4])

flowered<-c(pout_flrQ[,1], pout_flrQ[,3], pout_flrQ[,5])

veg12<-GV11$root[1:19]-pout_flrQ[,1]
veg13<-pout_flrQ[,2]-pout_flrQ[,3]
veg14<-pout_flrQ[,4]-pout_flrQ[,5]

notflowered<-c(veg12, veg13, veg14)

prop<-flowered/(flowered+notflowered)

#Checks - should be the same
cbind(flowered+notflowered, started)

cbind(prop, c(pout_flrQ[,1]/GV11$root[1:19], pout_flrQ[,3]/pout_flrQ[,2], pout_flrQ[,5]/pout_flrQ[,4]))

##Add data
GV11_flrQ<-GV11[grep("flr", GV11 $varb),]
GV11_flrQ<-data.frame(GV11_flrQ, started, flowered, notflowered, prop)

#Calculate expected relative fitness
GV11_flrQ$w_expected<-GV11_flrQ$prop/mean(GV11_flrQ$prop) 

##Calculate observed relative fitness
GV11_flrQ$w_obs<-GV11_flrQ$flower_success/mean(GV11_flrQ$flower_success)

##make sure sason is numeric
GV11_flrQ$season<-as.factor(GV11_flrQ$season)
 
 
 
#####The model
w_logisticE2Q<-glmer(cbind(flowered,notflowered)~elev+elev2+sSLA +sdeltaC+ sFDsnow+sheight+ I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2)+ season+(1|geno),family=binomial, data=GV11_flrQ)

summary(w_logisticE2Q)
