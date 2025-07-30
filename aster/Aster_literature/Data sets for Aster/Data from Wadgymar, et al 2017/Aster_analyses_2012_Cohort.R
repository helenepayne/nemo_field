COHORT12<-read.delim(".../Aster_analyses_2012_Cohort.txt", header=T)

##Note that Gothic is the low-elevation garden and Sco is the high-elevation garden


##Season should be a factor
COHORT12$season<-as.factor(COHORT12$season)


##Check variables
sapply(COHORT12,class)
str(COHORT12)


##Load libraris
library(aster)
library(MASS)
library(tidyr)
library(lme4)



#standardize trait values
COHORT12$sSLA<-scale(COHORT12$SLA,center=TRUE, scale=TRUE)

COHORT12$sdeltaC<-scale(COHORT12$deltaC,center=TRUE, scale=TRUE)

COHORT12$sheight<-scale(COHORT12$height,center=TRUE, scale=TRUE)

COHORT12$sFDsnow<-scale(COHORT12$FDsnow,center=TRUE, scale=TRUE)

COHORT12$elev<-scale(COHORT12$elevation,center=TRUE, scale=TRUE)





##A vector of length nrow that specifies the type of node each row corersponds to - flr, fecund, survived, or begin
layer <- gsub("[0-9]", "", as.character(COHORT12 $varb))
unique(layer)

##Creates a vector called fit where all of the fecund node types get a 1 and everything else gets a 0
fit <- as.numeric(layer == "fecund")

##Add new vectors to data set
COHORT12 <- data.frame(COHORT12, layer, fit)
with(COHORT12, class(fit)) ##Should be numeric
with(COHORT12, class(layer)) ##Should be a factor

##Subset by garden
GV<-subset(COHORT12, garden=="Gothic ")
Sco<-subset(COHORT12, garden=="Sco")

##Subsets for rows preaining to fecundity data
COHORT12_fecund<-COHORT12[grep("^fecund", COHORT12$varb),]

#hist(COHORT12_fecund$resp)

##Subset by season - fecund data only for graphing
s2013<-subset(COHORT12_fecund, season=="2013")
s2014<-subset(COHORT12_fecund, season=="2014")
#hist(s2013$resp)
#hist(s2014$resp)










#########################################################################################
 ## Full model  ## 
 ### This code runs the aster models that we need for  linear cumulative selection ## 
#########################################################################################



##The model with the best fit is normal for both years. 

######Structure of aster model
##Variables tha correspond to nodes of the graph
##Normal distribution
vars <-c("flr13", "fecund13", "survived13", "begin14","flr14", "fecund14", "survived14")

##Vector for graphical strucure listed in vars - initial nodes is 0
pred <- c(0,1,0,3,4,5,4)
##Check structure
all(pred<seq(along=pred))
foo <- rbind(vars, c("initial", vars)[pred + 1])
rownames(foo) <- c("successor", "predecessor")
t(foo)

##The two distributions used in our aster model corresponding to order of vars
famlist <- list(fam.bernoulli(), fam.normal.location(1))
fam <- c(1,2,1,1,1,2,1)
##Check
cbind(vars, fam)










#Best model has linear effects of source elevation as a covariate
linear<- aster(resp ~varb*garden+fit: (garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)

summary(linear)

##Alternative models with elev
linearE<- aster(resp ~varb*garden+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)

linearE2<- aster(resp ~varb*garden+fit: (garden*elev+garden*I(elev^2)+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)

anova(linear,linearE, linearE2)


##Checking for significance of each linear term (remove trait*garden and trait)
linear_SLA<- aster(resp ~varb*garden+fit: (garden*elev+garden*sdeltaC + garden*sFDsnow + garden*sheight), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)

linear_deltaC<- aster(resp ~varb*garden+fit: (garden*elev+garden*sSLA + garden*sFDsnow + garden*sheight), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)

linear_FDsnow<- aster(resp ~varb*garden+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sheight), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)

linear_height<- aster(resp ~varb*garden+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)

anova(linear_SLA,linearE) #Sig
anova(linear_deltaC,linearE)
anova(linear_FDsnow,linearE) #Sig
anova(linear_height,linearE) #Sig



##Checking for significance of each linear term*garden interaction (remove trait*garden but leave trait)
linear_SLA2<- aster(resp ~varb*garden+fit: (sSLA+garden*elev+garden*sdeltaC + garden*sFDsnow + garden*sheight), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)

linear_deltaC2<- aster(resp ~varb*garden+fit: (sdeltaC+garden*elev+garden*sSLA + garden*sFDsnow + garden*sheight), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)

linear_FDsnow2<- aster(resp ~varb*garden+fit: (sFDsnow+garden*elev+garden*sSLA+garden*sdeltaC + garden*sheight), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)

linear_height2<- aster(resp ~varb*garden+fit: (sheight+garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)


anova(linear_SLA2,linearE) 
anova(linear_deltaC2,linearE)
anova(linear_FDsnow2,linearE) 
anova(linear_height2,linearE) #Sig






##Analyses of separate gardens to test for the significance of trais in each garden when the interaction between trait*garden is significant
linearE_GV<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight), pred, fam, famlist=famlist,varb, id, root, data = GV)

linearE_Sco<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight), pred, fam, famlist=famlist,varb, id, root, data = Sco)

##Significance of traits from separate models 
linearE_GVheight<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow), pred, fam, famlist=famlist,varb, id, root, data = GV)

linearE_Scoheight<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow), pred, fam, famlist=famlist,varb, id, root, data = Sco)

anova(linearE_GVheight, linearE_GV)
anova(linearE_Scoheight, linearE_Sco) #Sig










 ######## To extract selection gradients #########
###linear model, WITH source elevation.##
#first, we extract the predicted fitness values from our model "linearE" and arrange nicely
pout<-matrix(predict(linearE), nrow=nrow(linearE$x), ncol=ncol(linearE$x))
colnames(pout)<-colnames(linearE$x)
 
 ##Now we use those predicted values to calculate predicted overall fitness for each year. We wanted to create a data frame with "geno" and trait values along with these predicted values, calculate fitness (fecundity * (flowering success)/#plants)
 
##Add predicted data to our observed data without repeating rows
C12_lifetime<-data.frame(COHORT12[1:46,], pout)

##Add columns for the fitness each genotype achieved in each year - (num flowered/number at start of season) * length fruit produced
C12_lifetime$fit13<-(C12_lifetime$flr13/C12_lifetime$root)* C12_lifetime$fecund13

C12_lifetime$fit14<-(C12_lifetime$flr14/C12_lifetime$survived13)* C12_lifetime$fecund14


##delete columns- all we need are geno, garden, and fit13:fit14
C12_lifetime<-C12_lifetime[,-c(1,4:29)] 

## Convert from wide to long format ##
C12_lifetime<- gather(C12_lifetime, season, Expected_fitness, fit13:fit14)

##Relabel season to reflect just the year (numeric)
levels(C12_lifetime$season)[levels(C12_lifetime$season)=="fit13"] <- "2013"
levels(C12_lifetime$season)[levels(C12_lifetime$season)=="fit14"] <- "2014"


##We need to add in trait data 
#Subset the rows for the two years of fecundity data (ensures that we don't repeat rows with the same data) and keep only trait and elev data
COHORT12sub<-COHORT12[grep("fecund", COHORT12 $varb),]
##Remove columns that aren't needed
COHORT12sub <-COHORT12sub[,-c(9:13,21:22)] 

##Merge data sets
predicted_fit<- merge(C12_lifetime, COHORT12sub, by=c("garden","season","geno"))


###Calculate relative fitness based on aster predicted fitness##
predicted_fit$w_expected<- predicted_fit$Expected_fitness/mean(predicted_fit$Expected_fitness)
  
#Calculate observed relative fitness 
predicted_fit$w_obs<-predicted_fit$Fit/mean(predicted_fit$Fit)


 
##Run the OLS model, from which we can extract selection gradients - the selection gradient should be correct, but the SE will not be. 
 
wmout<-lmer(w_expected ~sSLA*garden+sdeltaC*garden+sFDsnow*garden+sheight*garden+garden*elev+garden*season+(1|geno),data= predicted_fit) 

summary(wmout)

















###quadratic model for overall fitness###
### This code runs the aster models that we need for  nonlinear cumulative selection ## 

###Quadratic model

#Best model has linear effect of source elevation as a covariate
quadratic<- aster(resp ~varb*garden+fit: (garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sSLA^2)+ garden*I(sdeltaC^2) +garden*I(sFDsnow^2) + garden*I(sheight^2) ), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)



##Alternative models with elev and elev^2 as covariates
quadraticE<- aster(resp ~varb*garden+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sSLA^2)+ garden*I(sdeltaC^2) +garden*I(sFDsnow^2) + garden*I(sheight^2) ), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)


quadraticE2<- aster(resp ~varb*garden+fit: (garden*elev+garden*I(elev^2)+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sSLA^2)+ garden*I(sdeltaC^2) +garden*I(sFDsnow^2) + garden*I(sheight^2) ), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)

#QuadraticE has lowest deviance value
anova(quadratic, quadraticE, quadraticE2)


##Checking significance of each term (drop interaction and trait)
quadraticE_SLA<- aster(resp ~varb*garden+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+ garden*I(sdeltaC^2) +garden*I(sFDsnow^2) + garden*I(sheight^2) ), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)

quadraticE_deltaC<- aster(resp ~varb*garden+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sSLA^2)+ garden*I(sFDsnow^2) + garden*I(sheight^2) ), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)

quadraticE_FDsnow<- aster(resp ~varb*garden+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sSLA^2)+ garden*I(sdeltaC^2) + garden*I(sheight^2) ), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)

quadraticE_height<- aster(resp ~varb*garden+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sSLA^2)+ garden*I(sdeltaC^2) +garden*I(sFDsnow^2)), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)


anova(quadraticE_SLA, quadraticE) #Sig
anova(quadraticE_deltaC, quadraticE) #Sig
anova(quadraticE_FDsnow, quadraticE) #sig
anova(quadraticE_height, quadraticE) #Sig





##Checking significance of each interaction (drop interaction and keep trait)
quadraticE_SLA2<- aster(resp ~varb*garden+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+ garden*I(sdeltaC^2) +garden*I(sFDsnow^2) + garden*I(sheight^2) + I(sSLA^2)), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)

quadraticE_deltaC2<- aster(resp ~varb*garden+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sSLA^2)+ garden*I(sFDsnow^2) + garden*I(sheight^2) +I(sdeltaC^2)), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)

quadraticE_FDsnow2<- aster(resp ~varb*garden+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sSLA^2)+ garden*I(sdeltaC^2) + garden*I(sheight^2) +I(sFDsnow^2)), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)

quadraticE_height2<- aster(resp ~varb*garden+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sSLA^2)+ garden*I(sdeltaC^2) +garden*I(sFDsnow^2)+I(sheight^2)), pred, fam, famlist=famlist,varb, id, root, data = COHORT12)


anova(quadraticE_SLA2, quadraticE) #Sig
anova(quadraticE_deltaC2, quadraticE) #Sig
anova(quadraticE_FDsnow2, quadraticE) #Sig
anova(quadraticE_height2, quadraticE) #Sig



##Separate models for each garden to test for significance of each trait in each garden when interaction when trait*garden is significant
quadraticE_GV<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight+I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ), pred, fam, famlist=famlist,varb, id, root, data = GV)

quadraticE_Sco<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight+I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ), pred, fam, famlist=famlist,varb, id, root, data = Sco)


####Deviance values in analyses of separate gardens for traits involved in interacions with garden in the overall models
quadraticE_GVSLA<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ), pred, fam, famlist=famlist,varb, id, root, data = GV)

quadraticE_GVdeltaC<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight+ I(sSLA^2) +I(sFDsnow^2) + I(sheight^2) ), pred, fam, famlist=famlist,varb, id, root, data = GV)

quadraticE_GVFDsnow<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight+ I(sdeltaC^2) +I(sSLA^2) + I(sheight^2) ), pred, fam, famlist=famlist,varb, id, root, data = GV)

quadraticE_GVheight<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight+ I(sdeltaC^2) +I(sFDsnow^2) + I(sSLA^2) ), pred, fam, famlist=famlist,varb, id, root, data = GV)

anova(quadraticE_GVSLA, quadraticE_GV)
anova(quadraticE_GVdeltaC, quadraticE_GV)
anova(quadraticE_GVFDsnow, quadraticE_GV)
anova(quadraticE_GVheight, quadraticE_GV)


quadraticE_ScoSLA<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ), pred, fam, famlist=famlist,varb, id, root, data = Sco)

quadraticE_ScodeltaC<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight+ I(sSLA^2) +I(sFDsnow^2) + I(sheight^2) ), pred, fam, famlist=famlist,varb, id, root, data = Sco)

quadraticE_ScoFDsnow<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight+ I(sdeltaC^2) +I(sSLA^2) + I(sheight^2) ), pred, fam, famlist=famlist,varb, id, root, data = Sco)

quadraticE_Scoheight<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight+ I(sdeltaC^2) +I(sFDsnow^2) + I(sSLA^2) ), pred, fam, famlist=famlist,varb, id, root, data = Sco)


anova(quadraticE_ScoSLA, quadraticE_Sco) #Sig
anova(quadraticE_ScodeltaC, quadraticE_Sco) #Sig
anova(quadraticE_ScoFDsnow, quadraticE_Sco) #Sig
anova(quadraticE_Scoheight, quadraticE_Sco) #Sig









######## To extract selection gradients #########
 
#first, we extract the predicted fitness values from our model "quadraticE" and arrange them nicely
poutQ<-matrix(predict(quadraticE, root=root), nrow=nrow(quadraticE $x), ncol=ncol(quadraticE $x))
colnames(poutQ)<-colnames(quadraticE $x)

##Now we use those predicted values to calculate predicted overall fitness for each year. We need a data frame with geno, trait values, and predicted values 

##Add predicted data to our observed data without repeating rows 
C12_lifetimeQ<-data.frame(COHORT12[1:46,], poutQ)

##Add columns for the fitness each genotype achieved in each year - (num flowered/number at start of season) * length fruit produced
C12_lifetimeQ$fit13<-(C12_lifetimeQ$flr13/C12_lifetimeQ$root)* C12_lifetimeQ$fecund13

C12_lifetimeQ$fit14<-(C12_lifetimeQ$flr14/C12_lifetimeQ$survived13)* C12_lifetimeQ$fecund14

##delete columns that are not needed from the dataframe
C12_lifetimeQ<-C12_lifetimeQ[,-c(1,4:29)] 

## Convert from wide to long format ##
C12_lifetimeQ<- gather(C12_lifetimeQ, season, Expected_fitness, fit13:fit14)

##Relabel season to reflect just the year (numeric)
levels(C12_lifetimeQ$season)[levels(C12_lifetimeQ $season)=="fit13"] <- "2013"
levels(C12_lifetimeQ$season)[levels(C12_lifetimeQ $season)=="fit14"] <- "2014"


##We need to add in trait data 
#Subset the rows for the three years of fecundity data (ensures that we don't repeat rows with the same data) and keep only trait and elev data
COHORT12sub<-COHORT12[grep("fecund", COHORT12 $varb),]

##Remove columns that aren't needed - keep geno, season, sandardized traits, fitness, and elev
COHORT12sub <-COHORT12sub[,-c(9:14,21:22)] 


predicted_fitQ <- merge(C12_lifetimeQ, COHORT12sub, by=c("garden","season","geno"))

###Calculate relative fitness based on aster predicted fitness##
predicted_fitQ$w_expected<- predicted_fitQ$Expected_fitness/mean(predicted_fitQ$Expected_fitness)
  
#Calculate observed relative fitness 
predicted_fitQ$w_obs<-predicted_fitQ$Fit/mean(predicted_fitQ$Fit)



 
##Run the OLS model, from which we can extract selection gradients
wmoutQ<-lmer(w_expected ~sSLA*garden+garden*I(sSLA^2)+sdeltaC*garden+garden*I(sdeltaC^2)+sFDsnow*garden+garden*I(sFDsnow^2)+sheight*garden+garden*I(sheight^2)+garden*elev+garden*season+(1|geno),data= predicted_fitQ) 

summary(wmoutQ)
fixef(wmoutQ)*2  #For quadratic gradients

























 
 
 
 
 
 
 
 
 #########################################################################################
 #####This second aster model is to estimate linear selection via fecundity only (no predecessors) 
 #########################################################################################

##Variables that correspond to the nodes of the graph
varsF <-c("fecund13",  "fecund14")

##Vector for graphical structure listed in vars (simple in this case)
predF <- c(0,0)
##Chck structure
all(predF<seq(along=predF)) #Must be true
foo <- rbind(varsF, c("initial", varsF)[predF + 1])
rownames(foo) <- c("successor", "predecessor")
t(foo)

##Set up distributions for aster model 
famlistF <- list(fam.normal.location(1))
famF <- c(1,1)
##Check
cbind(varsF, famF)



##Subset by garden
GV_fecund<-subset(COHORT12_fecund, garden=="Gothic ")
Sco_fecund<-subset(COHORT12_fecund, garden=="Sco")




#Best model has linear effect of source elevation as a covariate
linear_fecund<- aster(resp ~varb+fit: (garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

linear_fecundE<-aster(resp ~varb+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

linear_fecundE2<- aster(resp ~varb+fit: (garden*elev+garden*I(elev^2)+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

#Best model has linear effect of source elevation as a covariate
anova(linear_fecund, linear_fecundE, linear_fecundE2)




##Look at significance of each trait (drop interaction and trait)
linear_fecundSLAE<-aster(resp ~varb+fit: (garden*elev+garden*sdeltaC + garden*sFDsnow + garden*sheight), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

linear_fecunddeltaCE<-aster(resp ~varb+fit: (garden*elev+garden*sSLA+ garden*sFDsnow + garden*sheight), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

linear_fecundFDsnowE<-aster(resp ~varb+fit: (garden*elev+garden*sSLA+garden*sdeltaC +  garden*sheight), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

linear_fecundheightE<-aster(resp ~varb+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow ), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

anova(linear_fecundSLAE, linear_fecundE) #Sig
anova(linear_fecunddeltaCE, linear_fecundE) #Sig
anova(linear_fecundFDsnowE, linear_fecundE) #Sig
anova(linear_fecundheightE, linear_fecundE) #Sig



##Look at significance of each interaction (drop interaction but retain trait)
linear_fecundSLA2E<-aster(resp ~varb+fit: (garden*elev+garden*sdeltaC + garden*sFDsnow + garden*sheight+sSLA), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

linear_fecunddeltaC2E<-aster(resp ~varb+fit: (garden*elev+garden*sSLA+ garden*sFDsnow + garden*sheight+sdeltaC), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

linear_fecundFDsnow2E<-aster(resp ~varb+fit: (garden*elev+garden*sSLA+garden*sdeltaC +  garden*sheight+sFDsnow), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

linear_fecundheight2E<-aster(resp ~varb+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow+sheight), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

anova(linear_fecundSLA2E, linear_fecundE) #Sig
anova(linear_fecunddeltaC2E, linear_fecundE) #Sig
anova(linear_fecundFDsnow2E, linear_fecundE) #Sig
anova(linear_fecundheight2E, linear_fecundE) #Sig



##Separate aster models for each garden to test for the significance of traits in each garden when the interaction between trait*garden is significant
linear_fecundE_GV<-aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight), predF, famF, famlist=famlistF, varb, id, root, data = GV_fecund)

linear_fecundE_Sco<-aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight), predF, famF, famlist=famlistF, varb, id, root, data = Sco_fecund)


####Deviance values in analyses of separate gardens for traits involved in interacions with garden in the overall models
linear_fecundE_GVSLA<-aster(resp ~varb+fit: (elev+sdeltaC+ sFDsnow + sheight), predF, famF, famlist=famlistF, varb, id, root, data = GV_fecund)

linear_fecundE_GVdeltaC<-aster(resp ~varb+fit: (elev+sSLA+ sFDsnow + sheight), predF, famF, famlist=famlistF, varb, id, root, data = GV_fecund)

linear_fecundE_GVFDsnow<-aster(resp ~varb+fit: (elev+sSLA+ sdeltaC + sheight), predF, famF, famlist=famlistF, varb, id, root, data = GV_fecund)

linear_fecundE_GVheight<-aster(resp ~varb+fit: (elev+sSLA+sdeltaC+ sFDsnow), predF, famF, famlist=famlistF, varb, id, root, data = GV_fecund)

anova(linear_fecundE_GVSLA, linear_fecundE_GV) #Sig
anova(linear_fecundE_GVdeltaC, linear_fecundE_GV)
anova(linear_fecundE_GVFDsnow, linear_fecundE_GV) #Sig 
anova(linear_fecundE_GVheight, linear_fecundE_GV) #Sig

linear_fecundE_ScoSLA<-aster(resp ~varb+fit: (elev+sdeltaC+ sFDsnow + sheight), predF, famF, famlist=famlistF, varb, id, root, data = Sco_fecund)

linear_fecundE_ScodeltaC<-aster(resp ~varb+fit: (elev+sSLA+ sFDsnow + sheight), predF, famF, famlist=famlistF, varb, id, root, data = Sco_fecund)

linear_fecundE_ScoFDsnow<-aster(resp ~varb+fit: (elev+sSLA+ sdeltaC + sheight), predF, famF, famlist=famlistF, varb, id, root, data = Sco_fecund)

linear_fecundE_Scoheight<-aster(resp ~varb+fit: (elev+sSLA+sdeltaC+ sFDsnow), predF, famF, famlist=famlistF, varb, id, root, data = Sco_fecund)

anova(linear_fecundE_ScoSLA, linear_fecundE_Sco) #Sig
anova(linear_fecundE_ScodeltaC, linear_fecundE_Sco) #Sig
anova(linear_fecundE_ScoFDsnow, linear_fecundE_Sco) #Sig
anova(linear_fecundE_Scoheight, linear_fecundE_Sco) #Sig













 ######## To extract selection gradients #########
 ###linear model, WITH source elevation##
#first, we extract the predicted fitness values from our model "linear_fecundE" and arrange them nicely
pout_fecund<-matrix(predict(linear_fecundE), nrow=nrow(linear_fecundE$x), ncol=ncol(linear_fecundE$x))
colnames(pout_fecund)<-colnames(linear_fecundE$x)


##Retain garden and geno from data set created up top and add predicted values
Cohort12_fecundity<-data.frame(COHORT12_fecund[,c(2:3)], pout_fecund)

##Convert from wide to long format
Cohort12_fecundity <- gather(Cohort12_fecundity, season, Expected_fitness, fecund13:fecund14)

##Make season numeric
levels(Cohort12_fecundity $season)[levels(Cohort12_fecundity $season)=="fecund13"] <- "2013"

levels(Cohort12_fecundity $season)[levels(Cohort12_fecundity $season)=="fecund14"] <- "2014"

##Delete unnecessary columns
COHORT12_fecund2 <-COHORT12_fecund[,-c(9:11,13:15,21:22)] 

##Merge
Cohort12_fecundity <- merge(Cohort12_fecundity, COHORT12_fecund[,-c(9:11,13:15,21:22)], by=c("garden","season","geno"))
colnames(Cohort12_fecundity)[10] <- "observed_fecundity"

###Calculate relative fitness based on aster predicted fitness##
Cohort12_fecundity $w_expected<- Cohort12_fecundity $Expected_fitness/mean(Cohort12_fecundity $Expected_fitness)
  
#Calculate observed relative fitness 
Cohort12_fecundity $w_obs<-Cohort12_fecundity $observed_fecundity/mean(Cohort12_fecundity $observed_fecundity)




##Run the OLS model, from which we can extract selection gradients
  
wmoutF<-lmer(w_expected ~sSLA*garden+sdeltaC*garden+sFDsnow*garden+sheight*garden+garden*elev+garden*season+(1|geno),data= Cohort12_fecundity) 

summary(wmoutF)










###nonlinear election via fecundity##
quadratic_fecund<- aster(resp ~varb+fit: (garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sSLA^2)+ garden*I(sdeltaC^2) +garden*I(sFDsnow^2) + garden*I(sheight^2) ), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

quadratic_fecundE<- aster(resp ~varb+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sSLA^2)+ garden*I(sdeltaC^2) +garden*I(sFDsnow^2) + garden*I(sheight^2) ), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

quadratic_fecundE2<- aster(resp ~varb+fit: (garden*elev+garden*I(elev^2)+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sSLA^2)+ garden*I(sdeltaC^2) +garden*I(sFDsnow^2) + garden*I(sheight^2) ), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

#Keep elevation as a covariate
anova(quadratic_fecund, quadratic_fecundE, quadratic_fecundE2)


##Examine significance of individual traits (remove both interaction and trait)
quadratic_fecundESLA<- aster(resp ~varb+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sdeltaC^2) +garden*I(sFDsnow^2) + garden*I(sheight^2) ), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

quadratic_fecundEdeltaC<- aster(resp ~varb+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sSLA^2)+ garden*I(sFDsnow^2) + garden*I(sheight^2) ), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

quadratic_fecundEFDsnow<- aster(resp ~varb+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sSLA^2)+ garden*I(sdeltaC^2) + garden*I(sheight^2) ), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

quadratic_fecundEheight<- aster(resp ~varb+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sSLA^2)+ garden*I(sdeltaC^2) +garden*I(sFDsnow^2)), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

anova(quadratic_fecundESLA, quadratic_fecundE) #Sig
anova(quadratic_fecundEdeltaC, quadratic_fecundE)
anova(quadratic_fecundEFDsnow, quadratic_fecundE) #Sig
anova(quadratic_fecundEheight, quadratic_fecundE) #Sig




##Examine significance of interactions (remove interaction but not trait trait)
quadratic_fecundESLA2<- aster(resp ~varb+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+I(sSLA^2)+ garden*I(sdeltaC^2) +garden*I(sFDsnow^2) + garden*I(sheight^2) ), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

quadratic_fecundEdeltaC2<- aster(resp ~varb+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sSLA^2)+ I(sdeltaC^2) +garden*I(sFDsnow^2) + garden*I(sheight^2) ), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

quadratic_fecundEFDsnow2<- aster(resp ~varb+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sSLA^2)+ garden*I(sdeltaC^2) +I(sFDsnow^2) + garden*I(sheight^2) ), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

quadratic_fecundEheight2<- aster(resp ~varb+fit: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sSLA^2)+ garden*I(sdeltaC^2) +garden*I(sFDsnow^2) + I(sheight^2) ), predF, famF, famlist=famlistF, varb, id, root, data = COHORT12_fecund)

anova(quadratic_fecundESLA2, quadratic_fecundE) #Sig
anova(quadratic_fecundEdeltaC2, quadratic_fecundE)
anova(quadratic_fecundEFDsnow2, quadratic_fecundE) #Sig
anova(quadratic_fecundEheight2, quadratic_fecundE) #Sig



##Aster models of separate gardens to test for significance of traits in each garden when interaction between trait*garden is significant
quadratic_fecundE_GV<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight+I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ), predF, famF, famlist=famlistF, varb, id, root, data = GV_fecund)

quadratic_fecundE_Sco<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight+I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ), predF, famF, famlist=famlistF, varb, id, root, data = Sco_fecund)






####Deviance values in analyses of separate gardens for traits involved in interacions with garden in the overall models
quadratic_fecundE_GVSLA<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ), predF, famF, famlist=famlistF, varb, id, root, data = GV_fecund)

quadratic_fecundE_GVFDsnow<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight+ I(sSLA^2) +I(sdeltaC^2)  + I(sheight^2) ), predF, famF, famlist=famlistF, varb, id, root, data = GV_fecund)

quadratic_fecundE_GVheight<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight+ I(sSLA^2) +I(sdeltaC^2) +I(sFDsnow^2) ), predF, famF, famlist=famlistF, varb, id, root, data = GV_fecund)

anova(quadratic_fecundE_GVSLA, quadratic_fecundE_GV) #Sig
anova(quadratic_fecundE_GVFDsnow, quadratic_fecundE_GV) #Sig
anova(quadratic_fecundE_GVheight, quadratic_fecundE_GV) #Sig



quadratic_fecundE_ScoSLA<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2) ), predF, famF, famlist=famlistF, varb, id, root, data = Sco_fecund)

quadratic_fecundE_ScoFDsnow<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight+ I(sSLA^2) +I(sdeltaC^2)  + I(sheight^2) ), predF, famF, famlist=famlistF, varb, id, root, data = Sco_fecund)

quadratic_fecundE_Scoheight<- aster(resp ~varb+fit: (elev+sSLA+sdeltaC + sFDsnow + sheight+ I(sSLA^2) +I(sdeltaC^2) +I(sFDsnow^2) ), predF, famF, famlist=famlistF, varb, id, root, data = Sco_fecund)

anova(quadratic_fecundE_ScoSLA, quadratic_fecundE_Sco) #Sig
anova(quadratic_fecundE_ScoFDsnow, quadratic_fecundE_Sco) #Sig
anova(quadratic_fecundE_Scoheight, quadratic_fecundE_Sco) #Sig










####To extract betas for quadratic models ####

#first, we extract the predicted fitness values from our model "quadratic_fecundE" and arrange them nicely
pout_fecundQ<-matrix(predict(quadratic_fecundE), nrow=nrow(quadratic_fecundE$x), ncol=ncol(quadratic_fecundE$x))
colnames(pout_fecundQ)<-colnames(quadratic_fecundE$x)
 
 

##Retain garden and geno from data set created up top and add predicted values
Cohort12_fecundityQ<-data.frame(COHORT12_fecund[,c(2:3)], pout_fecundQ)

##Convert from wide to long format
Cohort12_fecundityQ<- gather(Cohort12_fecundityQ, season, Expected_fitness, fecund13:fecund14)

##Make season numeric
levels(Cohort12_fecundityQ$season)[levels(Cohort12_fecundityQ $season)=="fecund13"] <- "2013"
levels(Cohort12_fecundityQ$season)[levels(Cohort12_fecundityQ $season)=="fecund14"] <- "2014"

##Delete columns that aren't needed
COHORT12_fecund2 <-COHORT12_fecund[,-c(9:11,13:15,21:22)] 

##Merge data sets 
Cohort12_fecundityQ <- merge(Cohort12_fecundityQ, COHORT12_fecund2, by=c("garden","season","geno"))
colnames(Cohort12_fecundityQ)[10] <- "observed_fecundity"


###Calculate relative fitness based on aster predicted fitness##
Cohort12_fecundityQ $w_expected<- Cohort12_fecundityQ$Expected_fitness/mean(Cohort12_fecundityQ $Expected_fitness)
  
#Calculate observed relative fitness 
Cohort12_fecundityQ $w_obs<-Cohort12_fecundityQ $observed_fecundity/mean(Cohort12_fecundityQ $observed_fecundity)





##Run the OLS model, from which we can extract selection gradients 
wmoutQF<-lmer(w_expected ~sSLA*garden+garden*I(sSLA^2)+sdeltaC*garden+garden*I(sdeltaC ^2)+sFDsnow*garden+garden*I(sFDsnow ^2)+sheight*garden+garden*I(sheight ^2)+garden*elev+garden*season+(1|geno),data= Cohort12_fecundityQ) 

summary(wmoutQF)
fixef(wmoutQF)*2 #Double gradients



 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
  
#########################################################################################
 #####This third aster model is for estimating linear selection via viability (e.g.flowering success) and does not include fecundity #####
 #########################################################################################

##Create data set without fecundity data
COHORT12_viab<-COHORT12[-grep("^fecund", COHORT12$varb),]
COHORT12_viab<-COHORT12_viab[-grep("^survived", COHORT12_viab $varb),]

######Structure of aster model
##Variables tha correspond to nodes of the graph
varsV<-c("flr13","begin14","flr14")
##Vector for graphical strucure listed in vars - initial nodes is 0
predV<- c(0,0,2)
##Check structure
all(predV<seq(along=predV)) #Should be true
foo <- rbind(varsV, c("initial", varsV)[predV + 1])
rownames(foo) <- c("successor", "predecessor")
t(foo)


##The distribution used in our aster model corresponding to order of vars
famlistV<- list(fam.bernoulli())
famV<- c(1,1,1)
##Check
cbind(varsV, famV)



##Add a column that distinguishes rows representing prob of flr data and survival data with a 0 or a 1, respectivly 
COHORT12_viab$flower<-as.numeric(COHORT12_viab$layer=="flr")


##Subsest by garden
GV_viab<-subset(COHORT12_viab, garden=="Gothic ")
Sco_viab<-subset(COHORT12_viab, garden=="Sco")
 
 

 
## We should include a linear and quadratic covariate for elevation in the linear models
linear_flr<- aster(resp ~varb+ flower: (garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight), predV, famV, famlist=famlistV,varb, id, root, data = COHORT12_viab)

linear_flrE<- aster(resp ~varb+ flower: (garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight), predV, famV, famlist=famlistV,varb, id, root, data = COHORT12_viab)

linear_flrE2<- aster(resp ~varb+ flower: (garden*elev+garden*I(elev^2)+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight), predV, famV, famlist=famlistV,varb, id, root, data = COHORT12_viab)

anova(linear_flr, linear_flrE, linear_flrE2)


##Significance of each trait (remove interaction and trait)
linear_flrE2SLA<- aster(resp ~varb+ flower: (garden*elev+garden*I(elev^2)+garden*sdeltaC + garden*sFDsnow + garden*sheight), predV, famV, famlist=famlistV,varb, id, root, data = COHORT12_viab)

linear_flrE2deltaC<- aster(resp ~varb+ flower: (garden*elev+garden*I(elev^2)+garden*sSLA+ garden*sFDsnow + garden*sheight), predV, famV, famlist=famlistV,varb, id, root, data = COHORT12_viab)

linear_flrE2FDsnow<- aster(resp ~varb+ flower: (garden*elev+garden*I(elev^2)+garden*sSLA+garden*sdeltaC + garden*sheight), predV, famV, famlist=famlistV,varb, id, root, data = COHORT12_viab)

linear_flrE2height<- aster(resp ~varb+ flower: (garden*elev+garden*I(elev^2)+garden*sSLA+garden*sdeltaC + garden*sFDsnow), predV, famV, famlist=famlistV,varb, id, root, data = COHORT12_viab)

anova(linear_flrE2SLA, linear_flrE2) #Sig
anova(linear_flrE2deltaC, linear_flrE2) #Sig
anova(linear_flrE2FDsnow, linear_flrE2) #Sig
anova(linear_flrE2height, linear_flrE2) #Sig




##Significance of each interaction (remove interaction but leave trait)
linear_flrE2SLA2<- aster(resp ~varb+ flower: (garden*elev+garden*I(elev^2)+sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight), predV, famV, famlist=famlistV,varb, id, root, data = COHORT12_viab)

linear_flrE2deltaC2<- aster(resp ~varb+ flower: (garden*elev+garden*I(elev^2)+garden*sSLA+sdeltaC + garden*sFDsnow + garden*sheight), predV, famV, famlist=famlistV,varb, id, root, data = COHORT12_viab)

linear_flrE2FDsnow2<- aster(resp ~varb+ flower: (garden*elev+garden*I(elev^2)+garden*sSLA+garden*sdeltaC +sFDsnow + garden*sheight), predV, famV, famlist=famlistV,varb, id, root, data = COHORT12_viab)

linear_flrE2height2<- aster(resp ~varb+ flower: (garden*elev+garden*I(elev^2)+garden*sSLA+garden*sdeltaC + garden*sFDsnow +sheight), predV, famV, famlist=famlistV,varb, id, root, data = COHORT12_viab)

anova(linear_flrE2SLA2, linear_flrE2) #Sig
anova(linear_flrE2deltaC2, linear_flrE2) 
anova(linear_flrE2FDsnow2, linear_flrE2) #Sig
anova(linear_flrE2height2, linear_flrE2) #Sig


##Separate aster models for each garden to test for the significance of traits in each garden when the interaction between trait*garden is significant
linear_flrE2_GV<- aster(resp ~varb+ flower: (elev+I(elev^2)+sSLA+sdeltaC + sFDsnow + sheight), predV, famV, famlist=famlistV,varb, id, root, data = GV_viab)

linear_flrE2_Sco<- aster(resp ~varb+ flower: (elev+I(elev^2)+sSLA+sdeltaC + sFDsnow + sheight), predV, famV, famlist=famlistV,varb, id, root, data = Sco_viab)


##Deviance for traits involved in interactions in main model
linear_flrE2_GVSLA<- aster(resp ~varb+ flower: (elev+I(elev^2) + sdeltaC+sFDsnow + sheight), predV, famV, famlist=famlistV,varb, id, root, data = GV_viab)

linear_flrE2_GVFDsnow<- aster(resp ~varb+ flower: (elev+I(elev^2)+ sSLA+ sdeltaC+ sheight), predV, famV, famlist=famlistV,varb, id, root, data = GV_viab)

linear_flrE2_GVheight<- aster(resp ~varb+ flower: (elev+I(elev^2)+sSLA + sdeltaC+sFDsnow ), predV, famV, famlist=famlistV,varb, id, root, data = GV_viab)


anova(linear_flrE2_GVSLA, linear_flrE2_GV) 
anova(linear_flrE2_GVFDsnow, linear_flrE2_GV)
anova(linear_flrE2_GVheight, linear_flrE2_GV)



linear_flrE2_ScoSLA<- aster(resp ~varb+ flower: (elev+I(elev^2) + sdeltaC+sFDsnow + sheight), predV, famV, famlist=famlistV,varb, id, root, data = Sco_viab)

linear_flrE2_ScoFDsnow<- aster(resp ~varb+ flower: (elev+I(elev^2)+ sSLA+ sdeltaC+ sheight), predV, famV, famlist=famlistV,varb, id, root, data = Sco_viab)

linear_flrE2_Scoheight<- aster(resp ~varb+ flower: (elev+I(elev^2)+sSLA + sdeltaC+sFDsnow ), predV, famV, famlist=famlistV,varb, id, root, data = Sco_viab)


anova(linear_flrE2_ScoSLA, linear_flrE2_Sco) 
anova(linear_flrE2_ScoFDsnow, linear_flrE2_Sco)
anova(linear_flrE2_Scoheight, linear_flrE2_Sco) #Sig






 



###############################################################
 ####To extract betas for linear models WITH linear covariates for elevation ####
 ###############################################################

#first, we extract the predicted fitness values from our model "linear_fecundE2" and arrange them nicely
pout_flr <-round(matrix(predict(linear_flrE2), nrow=nrow(linear_flrE2$x), ncol=ncol(linear_flrE2$x)))
colnames(pout_flr)<-colnames(linear_flrE2$x)


##Now I need to calculate flowering success (flr/begin)
started<-c(COHORT12$root[1:46], pout_flr[,2])

flowered<-c(pout_flr[,1], pout_flr[,3])

veg13<-COHORT12$root[1:46]-pout_flr[,1]
veg14<-pout_flr[,2]-pout_flr[,3]

notflowered<-c(veg13, veg14)

flr_prop<-flowered/(flowered+notflowered)

#Checks - should be the same
cbind(flowered+notflowered, started)

cbind(flr_prop, c(pout_flr[,1]/COHORT12$root[1:46], pout_flr[,3]/pout_flr[,2]))

##Add data
COHORT12_viab2<-COHORT12[grep("flr", COHORT12$varb),]
COHORT12_viab2<-data.frame(COHORT12_viab2, started, flowered, notflowered, flr_prop)


#Calculate expected relative fitness
COHORT12_viab2$w_expected <-COHORT12_viab2$flr_prop/mean(COHORT12_viab2$flr_prop)




####The model 

w_logisticE<-glmer(cbind(flowered,notflowered)~sSLA*garden +sdeltaC*garden+ sFDsnow*garden+sheight*garden+ elev*garden+(elev^2)*garden+garden*season+(1|geno),family=binomial, data=COHORT12_viab2)

summary(w_logisticE)
















##Nonlinear selection via viability##

## We should include a linear and quadratic covariate for elevation in the quadratic models
quadratic_flr<- aster(resp ~varb+ flower:(garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+ garden*I(sSLA^2)+ garden*I(sdeltaC^2) +garden*I(sFDsnow^2) + garden*I(sheight^2)) , predV, famV, famlist=famlistV, varb, id, root, data = COHORT12_viab)

quadratic_flrE<- aster(resp ~varb+ flower:(garden*elev+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+ garden*I(sSLA^2)+ garden*I(sdeltaC^2) +garden*I(sFDsnow^2) + garden*I(sheight^2)) , predV, famV, famlist=famlistV, varb, id, root, data = COHORT12_viab)

quadratic_flrE2<- aster(resp ~varb+ flower:(garden*elev+garden*I(elev^2)+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+ garden*I(sSLA^2)+ garden*I(sdeltaC^2) +garden*I(sFDsnow^2) + garden*I(sheight^2)) , predV, famV, famlist=famlistV, varb, id, root, data = COHORT12_viab) 

anova(quadratic_flr, quadratic_flrE, quadratic_flrE2)


##Assess significance of individual traits (remove interaction and trait)
quadratic_flrE2SLA<- aster(resp ~varb+ flower:(garden*elev+garden*I(elev^2)+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+garden*I(sdeltaC^2) +garden*I(sFDsnow^2) + garden*I(sheight^2)) , predV, famV, famlist=famlistV, varb, id, root, data = COHORT12_viab) 

quadratic_flrE2deltaC<- aster(resp ~varb+ flower:(garden*elev+garden*I(elev^2)+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+ garden*I(sSLA^2)+garden*I(sFDsnow^2) + garden*I(sheight^2)) , predV, famV, famlist=famlistV, varb, id, root, data = COHORT12_viab) 

quadratic_flrE2FDsnow<- aster(resp ~varb+ flower:(garden*elev+garden*I(elev^2)+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+ garden*I(sSLA^2)+ garden*I(sdeltaC^2)  + garden*I(sheight^2)) , predV, famV, famlist=famlistV, varb, id, root, data = COHORT12_viab) 

quadratic_flrE2height<- aster(resp ~varb+ flower:(garden*elev+garden*I(elev^2)+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+ garden*I(sSLA^2)+ garden*I(sdeltaC^2) +garden*I(sFDsnow^2) ) , predV, famV, famlist=famlistV, varb, id, root, data = COHORT12_viab) 

anova(quadratic_flrE2SLA, quadratic_flrE2) #Sig
anova(quadratic_flrE2deltaC, quadratic_flrE2) #Sig
anova(quadratic_flrE2FDsnow, quadratic_flrE2) #Sig
anova(quadratic_flrE2height, quadratic_flrE2)



##Assess significance of interaction (remove interaction but leave trait)
quadratic_flrE2SLA2<- aster(resp ~varb+ flower:(garden*elev+garden*I(elev^2)+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+ I(sSLA^2)+ garden*I(sdeltaC^2) +garden*I(sFDsnow^2) + garden*I(sheight^2)) , predV, famV, famlist=famlistV, varb, id, root, data = COHORT12_viab) 

quadratic_flrE2deltaC2<- aster(resp ~varb+ flower:(garden*elev+garden*I(elev^2)+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+ garden*I(sSLA^2)+ I(sdeltaC^2) +garden*I(sFDsnow^2) + garden*I(sheight^2)) , predV, famV, famlist=famlistV, varb, id, root, data = COHORT12_viab) 

quadratic_flrE2FDsnow2<- aster(resp ~varb+ flower:(garden*elev+garden*I(elev^2)+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+ garden*I(sSLA^2)+ garden*I(sdeltaC^2) +I(sFDsnow^2) + garden*I(sheight^2)) , predV, famV, famlist=famlistV, varb, id, root, data = COHORT12_viab) 

quadratic_flrE2height2<- aster(resp ~varb+ flower:(garden*elev+garden*I(elev^2)+garden*sSLA+garden*sdeltaC + garden*sFDsnow + garden*sheight+ garden*I(sSLA^2)+ garden*I(sdeltaC^2) +garden*I(sFDsnow^2) + I(sheight^2)) , predV, famV, famlist=famlistV, varb, id, root, data = COHORT12_viab) 

anova(quadratic_flrE2SLA2, quadratic_flrE2)
anova(quadratic_flrE2deltaC2, quadratic_flrE2) #Sig
anova(quadratic_flrE2FDsnow2, quadratic_flrE2) #Sig
anova(quadratic_flrE2height2, quadratic_flrE2) 




##Separate models for each garden to test for the significance of traits in each garden when interactions between garden*trait is significant
quadratic_flrE2_GV<- aster(resp ~varb+ flower:(elev+I(elev^2)+sSLA+sdeltaC + sFDsnow + sheight+ I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2)) , predV, famV, famlist=famlistV, varb, id, root, data = GV_viab) 

quadratic_flrE2_Sco<- aster(resp ~varb+ flower:(elev+I(elev^2)+sSLA+sdeltaC + sFDsnow + sheight+ I(sSLA^2)+ I(sdeltaC^2) +I(sFDsnow^2) + I(sheight^2)) , predV, famV, famlist=famlistV, varb, id, root, data = Sco_viab) 

##Deviance for terms involved in interactions with garden in the main models
quadratic_flrE2_GVdeltaC<- aster(resp ~varb+ flower:(elev+I(elev^2)+sSLA+sdeltaC + sFDsnow + sheight+ I(sSLA^2) +I(sFDsnow^2) + I(sheight^2)) , predV, famV, famlist=famlistV, varb, id, root, data = GV_viab) 

quadratic_flrE2_GVFDsnow<- aster(resp ~varb+ flower:(elev+I(elev^2)+sSLA+sdeltaC + sFDsnow + sheight+ I(sSLA^2)+ I(sdeltaC^2) + I(sheight^2)) , predV, famV, famlist=famlistV, varb, id, root, data = GV_viab) 

anova(quadratic_flrE2_GVdeltaC, quadratic_flrE2_GV) #Sig
anova(quadratic_flrE2_GVFDsnow, quadratic_flrE2_GV) #Sig


quadratic_flrE2_ScodeltaC<- aster(resp ~varb+ flower:(elev+I(elev^2)+sSLA+sdeltaC + sFDsnow + sheight+ I(sSLA^2) +I(sFDsnow^2) + I(sheight^2)) , predV, famV, famlist=famlistV, varb, id, root, data = Sco_viab) 

quadratic_flrE2_ScoFDsnow<- aster(resp ~varb+ flower:(elev+I(elev^2)+sSLA+sdeltaC + sFDsnow + sheight+ I(sSLA^2)+ I(sdeltaC^2) + I(sheight^2)) , predV, famV, famlist=famlistV, varb, id, root, data = Sco_viab) 

anova(quadratic_flrE2_ScodeltaC, quadratic_flrE2_Sco) #Sig
anova(quadratic_flrE2_ScoFDsnow, quadratic_flrE2_Sco) #Sig









###############################################################
 ####To extract betas for quadratic models WITH elevation ####
 ###############################################################


 
####To extract betas for quadratic models ####
pout_flrQ<-round(matrix(predict(quadratic_flrE2), nrow=nrow(quadratic_flrE2$x), ncol=ncol(quadratic_flrE2$x)))
colnames(pout_flrQ)<-colnames(quadratic_flrE2$x)

##Now I need to calculate flowering success (flr/begin)
startedQ<-c(COHORT12$root[1:46], pout_flrQ[,2])

floweredQ<-c(pout_flrQ[,1], pout_flrQ[,3])

veg13<-COHORT12$root[1:46]-pout_flrQ[,1]
veg14<-pout_flrQ[,2]-pout_flrQ[,3]

notfloweredQ<-c(veg13, veg14)

flr_prop<-floweredQ/(floweredQ+notfloweredQ)

#Checks - should be the same
cbind(floweredQ+notfloweredQ, started)

cbind(flr_prop, c(pout_flrQ[,1]/COHORT12$root[1:46], pout_flrQ[,3]/pout_flrQ[,2]))

##Add data
COHORT12_flrQ<-COHORT12[grep("flr", COHORT12 $varb),]
COHORT12_flrQ<-data.frame(COHORT12_flrQ, startedQ, floweredQ, notfloweredQ, flr_prop)




#Calculate expected relative fitness
COHORT12_flrQ$w_expected<-COHORT12_flrQ $flr_prop/mean(COHORT12_flrQ$flr_prop)


#########The model
w_logisticE2Q<-glmer(cbind(floweredQ ,notfloweredQ)~sSLA*garden +sdeltaC*garden+ sFDsnow*garden+sheight*garden+ I(sSLA^2)*garden+ I(sdeltaC^2)*garden +I(sFDsnow^2)*garden + I(sheight^2)*garden+ elev*garden+(elev^2)*garden+garden*season+(1|geno),family=binomial, data= COHORT12_flrQ) 

summary(w_logisticE2Q)
fixef(w_logisticE2Q)*2 #Double for gradients

