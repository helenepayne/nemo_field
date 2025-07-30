

### ANALYSIS OF DIFFERENCES IN TOTAL FITNESS USING ASTER
#phenotypic data on Arabidopsis lyrata grown in
#a transplant experiment in Norway (Leinonen, Remington, Savolainen 2011 Evolution) 

## NORWAY SITE (file: AlyrNorway_SpMaF12Aster.txt)

# Aster analysis using normal distribution after transformation
# presented in table 2

snaster<-read.table("AlyrNorway_SpMaF12Aster.txt",header=TRUE)
attach (snaster)
library (aster)

# Response variables
#
# SurvWin: winter survival
# ReprS: reproduced or not (did the plant have good seeds)
# FruitSeedcr: cubic root transformed value of: (total # fruits) * ((# seeds collected) / (#fruits collected))

#response variables
vars <- c(	"SurvWin1","SurvWin2","SurvWin3",
		"ReprS1","ReprS2","ReprS3",
		"FruitSeedcr1","FruitSeedcr2","FruitSeedcr3")

redata <- reshape(snaster, varying = list(vars),
		direction="long",timevar="varb",
		times=as.factor(vars),v.names="resp")
names (redata)
class(redata$varb)
levels(redata$varb)


redata <- data.frame(redata, root=1)

#how fitness components depend on each other
pred <- c(0,1,2,1,2,3,4,5,6)

#using lm to get values used in fam.normal.location()

sdresid1 <- sd(residuals (lm (FruitSeedcr1[ReprS1==1] ~ Block[ReprS1==1] + Pop[ReprS1==1])))
sdresid2 <- sd(residuals (lm (FruitSeedcr2[ReprS2==1] ~ Block[ReprS2==1] + Pop[ReprS2==1])))
sdresid3 <- sd(residuals (lm (FruitSeedcr3[ReprS3==1] ~ Block[ReprS3==1] + Pop[ReprS3==1])))

sd(FruitSeedcr1[ReprS1==1])
sd(FruitSeedcr2[ReprS2==1])
sd(FruitSeedcr3[ReprS3==1])

famlist <- list(	fam.bernoulli(),
			fam.normal.location(sdresid1),
			fam.normal.location(sdresid2),
			fam.normal.location(sdresid3)
			)

#families for response variables 
fam  <- c(1,1,1,1,1,1,2,3,4)

#combining fruit*seed^(1/3) data from all years
FruitSeedcr<- grep("FruitSeedcr", as.character(redata$varb))
FruitSeedcr<- is.element(seq(along = redata$varb), FruitSeedcr)
redata <- data.frame(redata, FruitSeedcr= as.numeric(FruitSeedcr))

redata$FruitSeedcr

summary (redata)

#to check predecessors for each variable
foo <- c("root", vars)
pvars <- foo[pred + 1]
bar <- cbind(pvars, vars)
colnames(bar) <- c("pred", "succ")
bar

#Setting up population categories for between-population comparisons
#(populations that are compared are combined to test with a model where
#they are defined separately to see if categorizing them separately 
#has a significant effect

redata$Pop
PopSN = with(redata, Pop)
levels(PopSN)[c(1,2)] <- "SandN"

PopSF1 = with(redata, Pop)
levels(PopSF1)[c(1,3)] <- "SandF1"

PopSF2 = with(redata, Pop)
levels(PopSF2)[c(1,4)] <- "SandF2"

PopF1F2 = with(redata, Pop)
levels(PopF1F2)[c(3,4)] <- "F1andF2"

PopF1recip = with(redata, CrossPop)
levels(PopF1recip)[c(3,4)] <- "SxandNx"

PopF2recip = with(redata, CrossPop)
levels(PopF2recip)[c(5,6)] <- "SNxandNSx"



 

# ASTER MODELS WITH ALL POPULATIONS SEPARATELY

SpNCnoraster4 = aster (resp ~ varb +Block   + FruitSeedcr*Pop - Pop, pred, fam, varb, id, root, data=redata, famlist=famlist)
SpNCnorasterrecip = aster (resp ~ varb + Block    + FruitSeedcr*CrossPop - CrossPop, pred, fam, varb, id, root, data=redata, famlist=famlist)

# ASTER MODELS WITH COMPARED POPULATIONS COMBINED

SpNCnorasterSN = aster (resp ~ varb + Block    + FruitSeedcr*PopSN - PopSN, pred, fam, varb, id, root, data=redata, famlist=famlist)
SpNCnorasterSF1 = aster (resp ~ varb + Block    + FruitSeedcr*PopSF1 - PopSF1, pred, fam, varb, id, root, data=redata, famlist=famlist)
SpNCnorasterSF2 = aster (resp ~ varb + Block    + FruitSeedcr*PopSF2 - PopSF2, pred, fam, varb, id, root, data=redata, famlist=famlist)
SpNCnorasterF1F2 = aster (resp ~ varb + Block    + FruitSeedcr*PopF1F2 - PopF1F2, pred, fam, varb, id, root, data=redata, famlist=famlist)
SpNCnorasterF1recip = aster (resp ~ varb + Block    + FruitSeedcr*PopF1recip- PopF1recip, pred, fam, varb, id, root, data=redata, famlist=famlist)
SpNCnorasterF2recip = aster (resp ~ varb + Block    + FruitSeedcr*PopF2recip- PopF2recip, pred, fam, varb, id, root, data=redata, famlist=famlist)


#LIKELIHOOD RATIO TESTS PRESENTED IN TABLE 2


#Spiterstulen - Mayodan - comparison
#one-tailed p-value was calculated from this value

anova (SpNCnorasterSN,SpNCnoraster4)

#Spiterstulen - F1 - comparison
anova (SpNCnorasterSF1,SpNCnoraster4)

#Spiterstulen - F2 - comparison
anova (SpNCnorasterSF2,SpNCnoraster4)

#F1 - F2 - comparison
anova (SpNCnorasterF1F2,SpNCnoraster4)

#F1(Sp) - F1(Ma) reciprocal comparison
#one-tailed p-value was calculated from this value

anova (SpNCnorasterF1recip,SpNCnorasterrecip)

#F2 (Sp) - F2(Ma) reciprocal comparison
#one-tailed p-value was calculated from this value

anova (SpNCnorasterF2recip,SpNCnorasterrecip)



