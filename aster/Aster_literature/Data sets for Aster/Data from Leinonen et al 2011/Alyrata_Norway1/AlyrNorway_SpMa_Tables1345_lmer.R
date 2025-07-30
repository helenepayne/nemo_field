
##### NORWAY SITE (file:"AlyrNorway_SpMaF12.txt")

## LMER ANALYSES at the NORWAY field site presented in tables 1,3,4 and 5 of 
#phenotypic data on Arabidopsis lyrata grown in
#a transplant experiment in Norway (Leinonen, Remington, Savolainen 2011 Evolution) 

sns4<-read.table("AlyrNorway_SpMaF12.txt",header=TRUE)

library (lme4)

levels (sns4$Pop)

#First set up population categories for between-population comparisons
#(populations that are compared are combined to test with a model where
#they are defined separately to see if categorizing them separately 
#has a significant effect

PopSpMa = with(sns4, Pop)
levels(PopSpMa)[c(3,4)] <- "SpMa"

PopSpF1 = with(sns4, Pop)
levels(PopSpF1)[c(1,4)] <- "SpF1"

PopSpF2 = with(sns4, Pop)
levels(PopSpF2)[c(2,4)] <- "SpF2"

PopMaF1 = with(sns4, Pop)
levels(PopMaF1)[c(1,3)] <- "MaF1"

PopMaF2 = with(sns4, Pop)
levels(PopMaF2)[c(2,3)] <- "MaF2"


#LMER MODELS FOR ANALYSES IN TABLES 1 AND 3

#Full models for between-population comparisons:

CSurvRFull 		<- lmer	(SurvWin4 	~ Pop + (1|Block)+ (1|Fam)	  					, REML=FALSE, family=binomial, data=sns4)
Flow1RFull 		<- lmer	(Flow1 	~ Pop + (1|Block)+ (1|Fam)	, subset=(SurvWin1 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow2RFull 		<- lmer	(Flow2 	~ Pop + (1|Block)+ (1|Fam)	, subset=(SurvWin2 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow3RFull 		<- lmer	(Flow3 	~ Pop + (1|Block)+ (1|Fam)	, subset=(SurvWin3 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow4RFull 		<- lmer	(Flow4 	~ Pop + (1|Block)+ (1|Fam)	, subset=(SurvWin4 == 1) 	, REML=FALSE, family=binomial, data=sns4)
TotSh1RFull 	<- lmer(sqrt(TotSh1)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh2RFull 	<- lmer(sqrt(TotSh2)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh3RFull 	<- lmer(sqrt(TotSh3)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh1RFull 		<- lmer(sqrt(FrSh1)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh2RFull 		<- lmer(sqrt(FrSh2)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh3RFull 		<- lmer(sqrt(FrSh3)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
SeFrG1RFull 	<- lmer(sqrt(SeFrG1)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG2RFull 	<- lmer(sqrt(SeFrG2)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG3RFull 	<- lmer(sqrt(SeFrG3)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart1RFull 	<- lmer(sqrt(FlStart1)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart2RFull 	<- lmer(sqrt(FlStart2)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
FlStart3RFull 	<- lmer(sqrt(FlStart3)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
H1Sh1RFull 		<- lmer(sqrt(H1Sh1)	~ Pop + (1|Block)+ (1|Fam)	 			 		, REML=FALSE, family=gaussian, data=sns4)
PsM12RFull 		<- lmer	(PsM12	~ Pop + (1|Block)+ (1|Fam)	 			 		, REML=FALSE, family=gaussian, data=sns4)
TotFl1RFull 	<- lmer(sqrt(TotFl1)	~ Pop + (1|Block)+ (1|Fam)						, REML=FALSE, family=gaussian, data=sns4)


# Models for between-parentals comparisons:

CSurvRSpMaFull 		<- lmer	(SurvWin4 	~ PopSpMa + (1|Block)+ (1|Fam)	  					, REML=FALSE, family=binomial, data=sns4)
Flow1RSpMaFull 		<- lmer	(Flow1 	~ PopSpMa + (1|Block)+ (1|Fam)	, subset=(SurvWin1 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow2RSpMaFull 		<- lmer	(Flow2 	~ PopSpMa + (1|Block)+ (1|Fam)	, subset=(SurvWin2 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow3RSpMaFull 		<- lmer	(Flow3 	~ PopSpMa + (1|Block)+ (1|Fam)	, subset=(SurvWin3 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow4RSpMaFull 		<- lmer	(Flow4 	~ PopSpMa + (1|Block)+ (1|Fam)	, subset=(SurvWin4 == 1) 	, REML=FALSE, family=binomial, data=sns4)
TotSh1RSpMaFull 		<- lmer(sqrt(TotSh1)	~ PopSpMa + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh2RSpMaFull 		<- lmer(sqrt(TotSh2)	~ PopSpMa + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh3RSpMaFull 		<- lmer(sqrt(TotSh3)	~ PopSpMa + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh1RSpMaFull 		<- lmer(sqrt(FrSh1)	~ PopSpMa + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh2RSpMaFull 		<- lmer(sqrt(FrSh2)	~ PopSpMa + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh3RSpMaFull 		<- lmer(sqrt(FrSh3)	~ PopSpMa + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
SeFrG1RSpMaFull 		<- lmer(sqrt(SeFrG1)	~ PopSpMa + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG2RSpMaFull 		<- lmer(sqrt(SeFrG2)	~ PopSpMa + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG3RSpMaFull 		<- lmer(sqrt(SeFrG3)	~ PopSpMa + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart1RSpMaFull 	<- lmer(sqrt(FlStart1)	~ PopSpMa + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart2RSpMaFull 	<- lmer(sqrt(FlStart2)	~ PopSpMa + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
FlStart3RSpMaFull 	<- lmer(sqrt(FlStart3)	~ PopSpMa + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
H1Sh1RSpMaFull 		<- lmer(sqrt(H1Sh1)	~ PopSpMa + (1|Block)+ (1|Fam)	 			 		, REML=FALSE, family=gaussian, data=sns4)
PsM12RSpMaFull 		<- lmer	(PsM12	~ PopSpMa + (1|Block)+ (1|Fam)	 			 		, REML=FALSE, family=gaussian, data=sns4)
TotFl1RSpMaFull 		<- lmer(sqrt(TotFl1)	~ PopSpMa + (1|Block)+ (1|Fam)						, REML=FALSE, family=gaussian, data=sns4)

# Models for F1-Sp comparisons:


CSurvRSpF1Full 		<- lmer	(SurvWin4 	~ PopSpF1 + (1|Block)+ (1|Fam)	  					, REML=FALSE, family=binomial, data=sns4)
Flow1RSpF1Full 		<- lmer	(Flow1 	~ PopSpF1 + (1|Block)+ (1|Fam)	, subset=(SurvWin1 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow2RSpF1Full 		<- lmer	(Flow2 	~ PopSpF1 + (1|Block)+ (1|Fam)	, subset=(SurvWin2 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow3RSpF1Full 		<- lmer	(Flow3 	~ PopSpF1 + (1|Block)+ (1|Fam)	, subset=(SurvWin3 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow4RSpF1Full 		<- lmer	(Flow4 	~ PopSpF1 + (1|Block)+ (1|Fam)	, subset=(SurvWin4 == 1) 	, REML=FALSE, family=binomial, data=sns4)
TotSh1RSpF1Full 		<- lmer(sqrt(TotSh1)	~ PopSpF1 + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh2RSpF1Full 		<- lmer(sqrt(TotSh2)	~ PopSpF1 + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh3RSpF1Full 		<- lmer(sqrt(TotSh3)	~ PopSpF1 + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh1RSpF1Full 		<- lmer(sqrt(FrSh1)	~ PopSpF1 + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh2RSpF1Full 		<- lmer(sqrt(FrSh2)	~ PopSpF1 + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh3RSpF1Full 		<- lmer(sqrt(FrSh3)	~ PopSpF1 + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
SeFrG1RSpF1Full 		<- lmer(sqrt(SeFrG1)	~ PopSpF1 + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG2RSpF1Full 		<- lmer(sqrt(SeFrG2)	~ PopSpF1 + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG3RSpF1Full 		<- lmer(sqrt(SeFrG3)	~ PopSpF1 + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart1RSpF1Full 	<- lmer(sqrt(FlStart1)	~ PopSpF1 + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart2RSpF1Full 	<- lmer(sqrt(FlStart2)	~ PopSpF1 + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
FlStart3RSpF1Full 	<- lmer(sqrt(FlStart3)	~ PopSpF1 + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
H1Sh1RSpF1Full 		<- lmer(sqrt(H1Sh1)	~ PopSpF1 + (1|Block)+ (1|Fam)	 			 		, REML=FALSE, family=gaussian, data=sns4)
PsM12RSpF1Full 		<- lmer	(PsM12	~ PopSpF1 + (1|Block)+ (1|Fam)	 			 		, REML=FALSE, family=gaussian, data=sns4)
TotFl1RSpF1Full 		<- lmer(sqrt(TotFl1)	~ PopSpF1 + (1|Block)+ (1|Fam)						, REML=FALSE, family=gaussian, data=sns4)

# Models for F2-Sp comparisons:

CSurvRSpF2Full 		<- lmer	(SurvWin4 	~ PopSpF2 + (1|Block)+ (1|Fam)	  					, REML=FALSE, family=binomial, data=sns4)
Flow1RSpF2Full 		<- lmer	(Flow1 	~ PopSpF2 + (1|Block)+ (1|Fam)	, subset=(SurvWin1 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow2RSpF2Full 		<- lmer	(Flow2 	~ PopSpF2 + (1|Block)+ (1|Fam)	, subset=(SurvWin2 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow3RSpF2Full 		<- lmer	(Flow3 	~ PopSpF2 + (1|Block)+ (1|Fam)	, subset=(SurvWin3 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow4RSpF2Full 		<- lmer	(Flow4 	~ PopSpF2 + (1|Block)+ (1|Fam)	, subset=(SurvWin4 == 1) 	, REML=FALSE, family=binomial, data=sns4)
TotSh1RSpF2Full 		<- lmer(sqrt(TotSh1)	~ PopSpF2 + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh2RSpF2Full 		<- lmer(sqrt(TotSh2)	~ PopSpF2 + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh3RSpF2Full 		<- lmer(sqrt(TotSh3)	~ PopSpF2 + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh1RSpF2Full 		<- lmer(sqrt(FrSh1)	~ PopSpF2 + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh2RSpF2Full 		<- lmer(sqrt(FrSh2)	~ PopSpF2 + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh3RSpF2Full 		<- lmer(sqrt(FrSh3)	~ PopSpF2 + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
SeFrG1RSpF2Full 		<- lmer(sqrt(SeFrG1)	~ PopSpF2 + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG2RSpF2Full 		<- lmer(sqrt(SeFrG2)	~ PopSpF2 + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG3RSpF2Full 		<- lmer(sqrt(SeFrG3)	~ PopSpF2 + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart1RSpF2Full 	<- lmer(sqrt(FlStart1)	~ PopSpF2 + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart2RSpF2Full 	<- lmer(sqrt(FlStart2)	~ PopSpF2 + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
FlStart3RSpF2Full 	<- lmer(sqrt(FlStart3)	~ PopSpF2 + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
H1Sh1RSpF2Full 		<- lmer(sqrt(H1Sh1)	~ PopSpF2 + (1|Block)+ (1|Fam)	 			 		, REML=FALSE, family=gaussian, data=sns4)
PsM12RSpF2Full 		<- lmer	(PsM12	~ PopSpF2 + (1|Block)+ (1|Fam)	 			 		, REML=FALSE, family=gaussian, data=sns4)
TotFl1RSpF2Full 		<- lmer(sqrt(TotFl1)	~ PopSpF2 + (1|Block)+ (1|Fam)						, REML=FALSE, family=gaussian, data=sns4)

#Models for Ma - F1 comparisons

CSurvRMaF1Full 		<- lmer	(SurvWin4 	~ PopMaF1 + (1|Block)+ (1|Fam)	  					, REML=FALSE, family=binomial, data=sns4)
Flow1RMaF1Full 		<- lmer	(Flow1 	~ PopMaF1 + (1|Block)+ (1|Fam)	, subset=(SurvWin1 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow2RMaF1Full 		<- lmer	(Flow2 	~ PopMaF1 + (1|Block)+ (1|Fam)	, subset=(SurvWin2 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow3RMaF1Full 		<- lmer	(Flow3 	~ PopMaF1 + (1|Block)+ (1|Fam)	, subset=(SurvWin3 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow4RMaF1Full 		<- lmer	(Flow4 	~ PopMaF1 + (1|Block)+ (1|Fam)	, subset=(SurvWin4 == 1) 	, REML=FALSE, family=binomial, data=sns4)
TotSh1RMaF1Full 		<- lmer(sqrt(TotSh1)	~ PopMaF1 + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh2RMaF1Full 		<- lmer(sqrt(TotSh2)	~ PopMaF1 + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh3RMaF1Full 		<- lmer(sqrt(TotSh3)	~ PopMaF1 + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh1RMaF1Full 		<- lmer(sqrt(FrSh1)	~ PopMaF1 + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh2RMaF1Full 		<- lmer(sqrt(FrSh2)	~ PopMaF1 + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh3RMaF1Full 		<- lmer(sqrt(FrSh3)	~ PopMaF1 + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
SeFrG1RMaF1Full 		<- lmer(sqrt(SeFrG1)	~ PopMaF1 + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG2RMaF1Full 		<- lmer(sqrt(SeFrG2)	~ PopMaF1 + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG3RMaF1Full 		<- lmer(sqrt(SeFrG3)	~ PopMaF1 + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart1RMaF1Full 	<- lmer(sqrt(FlStart1)	~ PopMaF1 + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart2RMaF1Full 	<- lmer(sqrt(FlStart2)	~ PopMaF1 + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
FlStart3RMaF1Full 	<- lmer(sqrt(FlStart3)	~ PopMaF1 + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
H1Sh1RMaF1Full 		<- lmer(sqrt(H1Sh1)	~ PopMaF1 + (1|Block)+ (1|Fam)	 			 		, REML=FALSE, family=gaussian, data=sns4)
PsM12RMaF1Full 		<- lmer	(PsM12	~ PopMaF1 + (1|Block)+ (1|Fam)	 			 		, REML=FALSE, family=gaussian, data=sns4)
TotFl1RMaF1Full 		<- lmer(sqrt(TotFl1)	~ PopMaF1 + (1|Block)+ (1|Fam)						, REML=FALSE, family=gaussian, data=sns4)



#Models for Ma - F2 comparisons


CSurvRMaF2Full 		<- lmer	(SurvWin4 	~ PopMaF2 + (1|Block)+ (1|Fam)	  					, REML=FALSE, family=binomial, data=sns4)
Flow1RMaF2Full 		<- lmer	(Flow1 	~ PopMaF2 + (1|Block)+ (1|Fam)	, subset=(SurvWin1 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow2RMaF2Full 		<- lmer	(Flow2 	~ PopMaF2 + (1|Block)+ (1|Fam)	, subset=(SurvWin2 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow3RMaF2Full 		<- lmer	(Flow3 	~ PopMaF2 + (1|Block)+ (1|Fam)	, subset=(SurvWin3 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow4RMaF2Full 		<- lmer	(Flow4 	~ PopMaF2 + (1|Block)+ (1|Fam)	, subset=(SurvWin4 == 1) 	, REML=FALSE, family=binomial, data=sns4)
TotSh1RMaF2Full 		<- lmer(sqrt(TotSh1)	~ PopMaF2 + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh2RMaF2Full 		<- lmer(sqrt(TotSh2)	~ PopMaF2 + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh3RMaF2Full 		<- lmer(sqrt(TotSh3)	~ PopMaF2 + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh1RMaF2Full 		<- lmer(sqrt(FrSh1)	~ PopMaF2 + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh2RMaF2Full 		<- lmer(sqrt(FrSh2)	~ PopMaF2 + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh3RMaF2Full 		<- lmer(sqrt(FrSh3)	~ PopMaF2 + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
SeFrG1RMaF2Full 		<- lmer(sqrt(SeFrG1)	~ PopMaF2 + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG2RMaF2Full 		<- lmer(sqrt(SeFrG2)	~ PopMaF2 + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG3RMaF2Full 		<- lmer(sqrt(SeFrG3)	~ PopMaF2 + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart1RMaF2Full 	<- lmer(sqrt(FlStart1)	~ PopMaF2 + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart2RMaF2Full 	<- lmer(sqrt(FlStart2)	~ PopMaF2 + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
FlStart3RMaF2Full 	<- lmer(sqrt(FlStart3)	~ PopMaF2 + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
H1Sh1RMaF2Full 		<- lmer(sqrt(H1Sh1)	~ PopMaF2 + (1|Block)+ (1|Fam)	 			 		, REML=FALSE, family=gaussian, data=sns4)
PsM12RMaF2Full 		<- lmer	(PsM12	~ PopMaF2 + (1|Block)+ (1|Fam)	 			 		, REML=FALSE, family=gaussian, data=sns4)
TotFl1RMaF2Full 		<- lmer(sqrt(TotFl1)	~ PopMaF2 + (1|Block)+ (1|Fam)						, REML=FALSE, family=gaussian, data=sns4)



#LIKELIHOOD RATIO TESTS FOR THE NORWAY SITE - TABLES 1 AND 3

#Sp - Ma - IN NORWAY

SpMa_anova_CSurv <- anova(CSurvRSpMaFull, CSurvRFull)  	
SpMa_anova_Flow1 <-anova(Flow1RSpMaFull, Flow1RFull)  	
SpMa_anova_Flow2 <-anova(Flow2RSpMaFull, Flow2RFull)  	
SpMa_anova_Flow3 <-anova(Flow3RSpMaFull, Flow3RFull)
SpMa_anova_Flow4 <-anova(Flow4RSpMaFull, Flow4RFull)  	  	
SpMa_anova_TotSh1 <-anova(TotSh1RSpMaFull, TotSh1RFull)  	
SpMa_anova_TotSh2 <-anova(TotSh2RSpMaFull, TotSh2RFull)  	
SpMa_anova_TotSh3 <-anova(TotSh3RSpMaFull, TotSh3RFull)  	
SpMa_anova_FrSh1 <-anova(FrSh1RSpMaFull, FrSh1RFull)  	
SpMa_anova_FrSh2 <-anova(FrSh2RSpMaFull, FrSh2RFull)
SpMa_anova_FrSh3 <-anova(FrSh3RSpMaFull, FrSh3RFull)
SpMa_anova_SeFrG1 <-anova(SeFrG1RSpMaFull, SeFrG1RFull)
SpMa_anova_SeFrG2 <-anova(SeFrG2RSpMaFull, SeFrG2RFull)
SpMa_anova_SeFrG3 <-anova(SeFrG3RSpMaFull, SeFrG3RFull)
SpMa_anova_FlStart1 <-anova(FlStart1RSpMaFull, FlStart1RFull)
SpMa_anova_FlStart2 <-anova(FlStart2RSpMaFull, FlStart2RFull)
SpMa_anova_FlStart3 <-anova(FlStart3RSpMaFull, FlStart3RFull)
SpMa_anova_H1Sh1 <-anova(H1Sh1RSpMaFull, H1Sh1RFull)
SpMa_anova_PsM12 <-anova(PsM12RSpMaFull, PsM12RFull)
SpMa_anova_TotFl1 <-anova(TotFl1RSpMaFull, TotFl1RFull)

#Sp - F1- IN NORWAY

SpF1_anova_CSurv <- anova(CSurvRSpF1Full, CSurvRFull)  	
SpF1_anova_Flow1 <-anova(Flow1RSpF1Full, Flow1RFull)  	
SpF1_anova_Flow2 <-anova(Flow2RSpF1Full, Flow2RFull)  	
SpF1_anova_Flow3 <-anova(Flow3RSpF1Full, Flow3RFull)
SpF1_anova_Flow4 <-anova(Flow4RSpF1Full, Flow4RFull)  	  	
SpF1_anova_TotSh1 <-anova(TotSh1RSpF1Full, TotSh1RFull)  	
SpF1_anova_TotSh2 <-anova(TotSh2RSpF1Full, TotSh2RFull)  	
SpF1_anova_TotSh3 <-anova(TotSh3RSpF1Full, TotSh3RFull)  	
SpF1_anova_FrSh1 <-anova(FrSh1RSpF1Full, FrSh1RFull)  	
SpF1_anova_FrSh2 <-anova(FrSh2RSpF1Full, FrSh2RFull)
SpF1_anova_FrSh3 <-anova(FrSh3RSpF1Full, FrSh3RFull)
SpF1_anova_SeFrG1 <-anova(SeFrG1RSpF1Full, SeFrG1RFull)
SpF1_anova_SeFrG2 <-anova(SeFrG2RSpF1Full, SeFrG2RFull)
SpF1_anova_SeFrG3 <-anova(SeFrG3RSpF1Full, SeFrG3RFull)
SpF1_anova_FlStart1 <-anova(FlStart1RSpF1Full, FlStart1RFull)
SpF1_anova_FlStart2 <-anova(FlStart2RSpF1Full, FlStart2RFull)
SpF1_anova_FlStart3 <-anova(FlStart3RSpF1Full, FlStart3RFull)
SpF1_anova_H1Sh1 <-anova(H1Sh1RSpF1Full, H1Sh1RFull)
SpF1_anova_PsM12 <-anova(PsM12RSpF1Full, PsM12RFull)
SpF1_anova_TotFl1 <-anova(TotFl1RSpF1Full, TotFl1RFull)

#Sp - F2- IN NORWAY

SpF2_anova_CSurv <- anova(CSurvRSpF2Full, CSurvRFull)  	
SpF2_anova_Flow1 <-anova(Flow1RSpF2Full, Flow1RFull)  	
SpF2_anova_Flow2 <-anova(Flow2RSpF2Full, Flow2RFull)  	
SpF2_anova_Flow3 <-anova(Flow3RSpF2Full, Flow3RFull)
SpF2_anova_Flow4 <-anova(Flow4RSpF2Full, Flow4RFull)  	  	
SpF2_anova_TotSh1 <-anova(TotSh1RSpF2Full, TotSh1RFull)  	
SpF2_anova_TotSh2 <-anova(TotSh2RSpF2Full, TotSh2RFull)  	
SpF2_anova_TotSh3 <-anova(TotSh3RSpF2Full, TotSh3RFull)  	
SpF2_anova_FrSh1 <-anova(FrSh1RSpF2Full, FrSh1RFull)  	
SpF2_anova_FrSh2 <-anova(FrSh2RSpF2Full, FrSh2RFull)
SpF2_anova_FrSh3 <-anova(FrSh3RSpF2Full, FrSh3RFull)
SpF2_anova_SeFrG1 <-anova(SeFrG1RSpF2Full, SeFrG1RFull)
SpF2_anova_SeFrG2 <-anova(SeFrG2RSpF2Full, SeFrG2RFull)
SpF2_anova_SeFrG3 <-anova(SeFrG3RSpF2Full, SeFrG3RFull)
SpF2_anova_FlStart1 <-anova(FlStart1RSpF2Full, FlStart1RFull)
SpF2_anova_FlStart2 <-anova(FlStart2RSpF2Full, FlStart2RFull)
SpF2_anova_FlStart3 <-anova(FlStart3RSpF2Full, FlStart3RFull)
SpF2_anova_H1Sh1 <-anova(H1Sh1RSpF2Full, H1Sh1RFull)
SpF2_anova_PsM12 <-anova(PsM12RSpF2Full, PsM12RFull)
SpF2_anova_TotFl1 <-anova(TotFl1RSpF2Full, TotFl1RFull)

#Ma - F1- IN NORWAY

MaF1_anova_CSurv <- anova(CSurvRMaF1Full, CSurvRFull)  	
MaF1_anova_Flow1 <-anova(Flow1RMaF1Full, Flow1RFull)  	
MaF1_anova_Flow2 <-anova(Flow2RMaF1Full, Flow2RFull)  	
MaF1_anova_Flow3 <-anova(Flow3RMaF1Full, Flow3RFull)
MaF1_anova_Flow4 <-anova(Flow4RMaF1Full, Flow4RFull)  	  	
MaF1_anova_TotSh1 <-anova(TotSh1RMaF1Full, TotSh1RFull)  	
MaF1_anova_TotSh2 <-anova(TotSh2RMaF1Full, TotSh2RFull)  	
MaF1_anova_TotSh3 <-anova(TotSh3RMaF1Full, TotSh3RFull)  	
MaF1_anova_FrSh1 <-anova(FrSh1RMaF1Full, FrSh1RFull)  	
MaF1_anova_FrSh2 <-anova(FrSh2RMaF1Full, FrSh2RFull)
MaF1_anova_FrSh3 <-anova(FrSh3RMaF1Full, FrSh3RFull)
MaF1_anova_SeFrG1 <-anova(SeFrG1RMaF1Full, SeFrG1RFull)
MaF1_anova_SeFrG2 <-anova(SeFrG2RMaF1Full, SeFrG2RFull)
MaF1_anova_SeFrG3 <-anova(SeFrG3RMaF1Full, SeFrG3RFull)
MaF1_anova_FlStart1 <-anova(FlStart1RMaF1Full, FlStart1RFull)
MaF1_anova_FlStart2 <-anova(FlStart2RMaF1Full, FlStart2RFull)
MaF1_anova_FlStart3 <-anova(FlStart3RMaF1Full, FlStart3RFull)
MaF1_anova_H1Sh1 <-anova(H1Sh1RMaF1Full, H1Sh1RFull)
MaF1_anova_PsM12 <-anova(PsM12RMaF1Full, PsM12RFull)
MaF1_anova_TotFl1 <-anova(TotFl1RMaF1Full, TotFl1RFull)

#Ma - F2- IN NORWAY

MaF2_anova_CSurv <- anova(CSurvRMaF2Full, CSurvRFull)  	
MaF2_anova_Flow1 <-anova(Flow1RMaF2Full, Flow1RFull)  	
MaF2_anova_Flow2 <-anova(Flow2RMaF2Full, Flow2RFull)  	
MaF2_anova_Flow3 <-anova(Flow3RMaF2Full, Flow3RFull)
MaF2_anova_Flow4 <-anova(Flow4RMaF2Full, Flow4RFull)  	  	
MaF2_anova_TotSh1 <-anova(TotSh1RMaF2Full, TotSh1RFull)  	
MaF2_anova_TotSh2 <-anova(TotSh2RMaF2Full, TotSh2RFull)  	
MaF2_anova_TotSh3 <-anova(TotSh3RMaF2Full, TotSh3RFull)  	
MaF2_anova_FrSh1 <-anova(FrSh1RMaF2Full, FrSh1RFull)  	
MaF2_anova_FrSh2 <-anova(FrSh2RMaF2Full, FrSh2RFull)
MaF2_anova_FrSh3 <-anova(FrSh3RMaF2Full, FrSh3RFull)
MaF2_anova_SeFrG1 <-anova(SeFrG1RMaF2Full, SeFrG1RFull)
MaF2_anova_SeFrG2 <-anova(SeFrG2RMaF2Full, SeFrG2RFull)
MaF2_anova_SeFrG3 <-anova(SeFrG3RMaF2Full, SeFrG3RFull)
MaF2_anova_FlStart1 <-anova(FlStart1RMaF2Full, FlStart1RFull)
MaF2_anova_FlStart2 <-anova(FlStart2RMaF2Full, FlStart2RFull)
MaF2_anova_FlStart3 <-anova(FlStart3RMaF2Full, FlStart3RFull)
MaF2_anova_H1Sh1 <-anova(H1Sh1RMaF2Full, H1Sh1RFull)
MaF2_anova_PsM12 <-anova(PsM12RMaF2Full, PsM12RFull)
MaF2_anova_TotFl1 <-anova(TotFl1RMaF2Full, TotFl1RFull)


#EXTRACTING RESULTS FOR TABLES 1 AND 3

#Sp - Ma- IN NORWAY

SpMa_CSurv_Summary <- rbind (SpMa_anova_CSurv$Chisq[2], SpMa_anova_CSurv$P[2])
SpMa_Flow1_Summary <- rbind (SpMa_anova_Flow1$Chisq[2], SpMa_anova_Flow1$P[2])
SpMa_Flow2_Summary <- rbind (SpMa_anova_Flow2$Chisq[2], SpMa_anova_Flow2$P[2])
SpMa_Flow3_Summary <- rbind (SpMa_anova_Flow3$Chisq[2], SpMa_anova_Flow3$P[2])
SpMa_Flow4_Summary <- rbind (SpMa_anova_Flow4$Chisq[2], SpMa_anova_Flow4$P[2])
SpMa_TotSh1_Summary <- rbind (SpMa_anova_TotSh1$Chisq[2], SpMa_anova_TotSh1$P[2])
SpMa_TotSh2_Summary <- rbind (SpMa_anova_TotSh2$Chisq[2], SpMa_anova_TotSh2$P[2])
SpMa_TotSh3_Summary <- rbind (SpMa_anova_TotSh3$Chisq[2], SpMa_anova_TotSh3$P[2])
SpMa_FrSh1_Summary <- rbind (SpMa_anova_FrSh1$Chisq[2], SpMa_anova_FrSh1$P[2])
SpMa_FrSh2_Summary <- rbind (SpMa_anova_FrSh2$Chisq[2], SpMa_anova_FrSh2$P[2])
SpMa_FrSh3_Summary <- rbind (SpMa_anova_FrSh3$Chisq[2], SpMa_anova_FrSh3$P[2])
SpMa_SeFrG1_Summary <- rbind (SpMa_anova_SeFrG1$Chisq[2], SpMa_anova_SeFrG1$P[2])
SpMa_SeFrG2_Summary <- rbind (SpMa_anova_SeFrG2$Chisq[2], SpMa_anova_SeFrG2$P[2])
SpMa_SeFrG3_Summary <- rbind (SpMa_anova_SeFrG3$Chisq[2], SpMa_anova_SeFrG3$P[2])
SpMa_FlStart1_Summary <- rbind (SpMa_anova_FlStart1$Chisq[2], SpMa_anova_FlStart1$P[2])
SpMa_FlStart2_Summary <- rbind (SpMa_anova_FlStart2$Chisq[2], SpMa_anova_FlStart2$P[2])
SpMa_FlStart3_Summary <- rbind (SpMa_anova_FlStart3$Chisq[2], SpMa_anova_FlStart3$P[2])
SpMa_H1Sh1_Summary <- rbind (SpMa_anova_H1Sh1$Chisq[2], SpMa_anova_H1Sh1$P[2])
SpMa_PsM12_Summary <- rbind (SpMa_anova_PsM12$Chisq[2], SpMa_anova_PsM12$P[2])
SpMa_TotFl1_Summary <- rbind (SpMa_anova_TotFl1$Chisq[2], SpMa_anova_TotFl1$P[2])

#Sp - F1- IN NORWAY

SpF1_CSurv_Summary <- rbind (SpF1_anova_CSurv$Chisq[2], SpF1_anova_CSurv$P[2])
SpF1_Flow1_Summary <- rbind (SpF1_anova_Flow1$Chisq[2], SpF1_anova_Flow1$P[2])
SpF1_Flow2_Summary <- rbind (SpF1_anova_Flow2$Chisq[2], SpF1_anova_Flow2$P[2])
SpF1_Flow3_Summary <- rbind (SpF1_anova_Flow3$Chisq[2], SpF1_anova_Flow3$P[2])
SpF1_Flow4_Summary <- rbind (SpF1_anova_Flow4$Chisq[2], SpF1_anova_Flow4$P[2])
SpF1_TotSh1_Summary <- rbind (SpF1_anova_TotSh1$Chisq[2], SpF1_anova_TotSh1$P[2])
SpF1_TotSh2_Summary <- rbind (SpF1_anova_TotSh2$Chisq[2], SpF1_anova_TotSh2$P[2])
SpF1_TotSh3_Summary <- rbind (SpF1_anova_TotSh3$Chisq[2], SpF1_anova_TotSh3$P[2])
SpF1_FrSh1_Summary <- rbind (SpF1_anova_FrSh1$Chisq[2], SpF1_anova_FrSh1$P[2])
SpF1_FrSh2_Summary <- rbind (SpF1_anova_FrSh2$Chisq[2], SpF1_anova_FrSh2$P[2])
SpF1_FrSh3_Summary <- rbind (SpF1_anova_FrSh3$Chisq[2], SpF1_anova_FrSh3$P[2])
SpF1_SeFrG1_Summary <- rbind (SpF1_anova_SeFrG1$Chisq[2], SpF1_anova_SeFrG1$P[2])
SpF1_SeFrG2_Summary <- rbind (SpF1_anova_SeFrG2$Chisq[2], SpF1_anova_SeFrG2$P[2])
SpF1_SeFrG3_Summary <- rbind (SpF1_anova_SeFrG3$Chisq[2], SpF1_anova_SeFrG3$P[2])
SpF1_FlStart1_Summary <- rbind (SpF1_anova_FlStart1$Chisq[2], SpF1_anova_FlStart1$P[2])
SpF1_FlStart2_Summary <- rbind (SpF1_anova_FlStart2$Chisq[2], SpF1_anova_FlStart2$P[2])
SpF1_FlStart3_Summary <- rbind (SpF1_anova_FlStart3$Chisq[2], SpF1_anova_FlStart3$P[2])
SpF1_H1Sh1_Summary <- rbind (SpF1_anova_H1Sh1$Chisq[2], SpF1_anova_H1Sh1$P[2])
SpF1_PsM12_Summary <- rbind (SpF1_anova_PsM12$Chisq[2], SpF1_anova_PsM12$P[2])
SpF1_TotFl1_Summary <- rbind (SpF1_anova_TotFl1$Chisq[2], SpF1_anova_TotFl1$P[2])

#Sp - F2- IN NORWAY

SpF2_CSurv_Summary <- rbind (SpF2_anova_CSurv$Chisq[2], SpF2_anova_CSurv$P[2])
SpF2_Flow1_Summary <- rbind (SpF2_anova_Flow1$Chisq[2], SpF2_anova_Flow1$P[2])
SpF2_Flow2_Summary <- rbind (SpF2_anova_Flow2$Chisq[2], SpF2_anova_Flow2$P[2])
SpF2_Flow3_Summary <- rbind (SpF2_anova_Flow3$Chisq[2], SpF2_anova_Flow3$P[2])
SpF2_Flow4_Summary <- rbind (SpF2_anova_Flow4$Chisq[2], SpF2_anova_Flow4$P[2])
SpF2_TotSh1_Summary <- rbind (SpF2_anova_TotSh1$Chisq[2], SpF2_anova_TotSh1$P[2])
SpF2_TotSh2_Summary <- rbind (SpF2_anova_TotSh2$Chisq[2], SpF2_anova_TotSh2$P[2])
SpF2_TotSh3_Summary <- rbind (SpF2_anova_TotSh3$Chisq[2], SpF2_anova_TotSh3$P[2])
SpF2_FrSh1_Summary <- rbind (SpF2_anova_FrSh1$Chisq[2], SpF2_anova_FrSh1$P[2])
SpF2_FrSh2_Summary <- rbind (SpF2_anova_FrSh2$Chisq[2], SpF2_anova_FrSh2$P[2])
SpF2_FrSh3_Summary <- rbind (SpF2_anova_FrSh3$Chisq[2], SpF2_anova_FrSh3$P[2])
SpF2_SeFrG1_Summary <- rbind (SpF2_anova_SeFrG1$Chisq[2], SpF2_anova_SeFrG1$P[2])
SpF2_SeFrG2_Summary <- rbind (SpF2_anova_SeFrG2$Chisq[2], SpF2_anova_SeFrG2$P[2])
SpF2_SeFrG3_Summary <- rbind (SpF2_anova_SeFrG3$Chisq[2], SpF2_anova_SeFrG3$P[2])
SpF2_FlStart1_Summary <- rbind (SpF2_anova_FlStart1$Chisq[2], SpF2_anova_FlStart1$P[2])
SpF2_FlStart2_Summary <- rbind (SpF2_anova_FlStart2$Chisq[2], SpF2_anova_FlStart2$P[2])
SpF2_FlStart3_Summary <- rbind (SpF2_anova_FlStart3$Chisq[2], SpF2_anova_FlStart3$P[2])
SpF2_H1Sh1_Summary <- rbind (SpF2_anova_H1Sh1$Chisq[2], SpF2_anova_H1Sh1$P[2])
SpF2_PsM12_Summary <- rbind (SpF2_anova_PsM12$Chisq[2], SpF2_anova_PsM12$P[2])
SpF2_TotFl1_Summary <- rbind (SpF2_anova_TotFl1$Chisq[2], SpF2_anova_TotFl1$P[2])

#Ma - F1- IN NORWAY

MaF1_CSurv_Summary <- rbind (MaF1_anova_CSurv$Chisq[2], MaF1_anova_CSurv$P[2])
MaF1_Flow1_Summary <- rbind (MaF1_anova_Flow1$Chisq[2], MaF1_anova_Flow1$P[2])
MaF1_Flow2_Summary <- rbind ( MaF1_anova_Flow2$Chisq[2], MaF1_anova_Flow2$P[2])
MaF1_Flow3_Summary <- rbind (MaF1_anova_Flow3$Chisq[2], MaF1_anova_Flow3$P[2])
MaF1_Flow4_Summary <- rbind (MaF1_anova_Flow4$Chisq[2], MaF1_anova_Flow4$P[2])
MaF1_TotSh1_Summary <- rbind (MaF1_anova_TotSh1$Chisq[2], MaF1_anova_TotSh1$P[2])
MaF1_TotSh2_Summary <- rbind (MaF1_anova_TotSh2$Chisq[2], MaF1_anova_TotSh2$P[2])
MaF1_TotSh3_Summary <- rbind (MaF1_anova_TotSh3$Chisq[2], MaF1_anova_TotSh3$P[2])
MaF1_FrSh1_Summary <- rbind (MaF1_anova_FrSh1$Chisq[2], MaF1_anova_FrSh1$P[2])
MaF1_FrSh2_Summary <- rbind (MaF1_anova_FrSh2$Chisq[2], MaF1_anova_FrSh2$P[2])
MaF1_FrSh3_Summary <- rbind (MaF1_anova_FrSh3$Chisq[2], MaF1_anova_FrSh3$P[2])
MaF1_SeFrG1_Summary <- rbind (MaF1_anova_SeFrG1$Chisq[2], MaF1_anova_SeFrG1$P[2])
MaF1_SeFrG2_Summary <- rbind (MaF1_anova_SeFrG2$Chisq[2], MaF1_anova_SeFrG2$P[2])
MaF1_SeFrG3_Summary <- rbind (MaF1_anova_SeFrG3$Chisq[2], MaF1_anova_SeFrG3$P[2])
MaF1_FlStart1_Summary <- rbind (MaF1_anova_FlStart1$Chisq[2], MaF1_anova_FlStart1$P[2])
MaF1_FlStart2_Summary <- rbind (MaF1_anova_FlStart2$Chisq[2], MaF1_anova_FlStart2$P[2])
MaF1_FlStart3_Summary <- rbind (MaF1_anova_FlStart3$Chisq[2], MaF1_anova_FlStart3$P[2])
MaF1_H1Sh1_Summary <- rbind (MaF1_anova_H1Sh1$Chisq[2], MaF1_anova_H1Sh1$P[2])
MaF1_PsM12_Summary <- rbind (MaF1_anova_PsM12$Chisq[2], MaF1_anova_PsM12$P[2])
MaF1_TotFl1_Summary <- rbind (MaF1_anova_TotFl1$Chisq[2], MaF1_anova_TotFl1$P[2])

#Ma - F2- IN NORWAY

MaF2_CSurv_Summary <- rbind (MaF2_anova_CSurv$Chisq[2], MaF2_anova_CSurv$P[2])
MaF2_Flow1_Summary <- rbind (MaF2_anova_Flow1$Chisq[2], MaF2_anova_Flow1$P[2])
MaF2_Flow2_Summary <- rbind (MaF2_anova_Flow2$Chisq[2], MaF2_anova_Flow2$P[2])
MaF2_Flow3_Summary <- rbind (MaF2_anova_Flow3$Chisq[2], MaF2_anova_Flow3$P[2])
MaF2_Flow4_Summary <- rbind (MaF2_anova_Flow4$Chisq[2], MaF2_anova_Flow4$P[2])
MaF2_TotSh1_Summary <- rbind (MaF2_anova_TotSh1$Chisq[2], MaF2_anova_TotSh1$P[2])
MaF2_TotSh2_Summary <- rbind (MaF2_anova_TotSh2$Chisq[2], MaF2_anova_TotSh2$P[2])
MaF2_TotSh3_Summary <- rbind (MaF2_anova_TotSh3$Chisq[2], MaF2_anova_TotSh3$P[2])
MaF2_FrSh1_Summary <- rbind (MaF2_anova_FrSh1$Chisq[2], MaF2_anova_FrSh1$P[2])
MaF2_FrSh2_Summary <- rbind (MaF2_anova_FrSh2$Chisq[2], MaF2_anova_FrSh2$P[2])
MaF2_FrSh3_Summary <- rbind (MaF2_anova_FrSh3$Chisq[2], MaF2_anova_FrSh3$P[2])
MaF2_SeFrG1_Summary <- rbind (MaF2_anova_SeFrG1$Chisq[2], MaF2_anova_SeFrG1$P[2])
MaF2_SeFrG2_Summary <- rbind (MaF2_anova_SeFrG2$Chisq[2], MaF2_anova_SeFrG2$P[2])
MaF2_SeFrG3_Summary <- rbind (MaF2_anova_SeFrG3$Chisq[2], MaF2_anova_SeFrG3$P[2])
MaF2_FlStart1_Summary <- rbind (MaF2_anova_FlStart1$Chisq[2], MaF2_anova_FlStart1$P[2])
MaF2_FlStart2_Summary <- rbind (MaF2_anova_FlStart2$Chisq[2], MaF2_anova_FlStart2$P[2])
MaF2_FlStart3_Summary <- rbind (MaF2_anova_FlStart3$Chisq[2], MaF2_anova_FlStart3$P[2])
MaF2_H1Sh1_Summary <- rbind (MaF2_anova_H1Sh1$Chisq[2], MaF2_anova_H1Sh1$P[2])
MaF2_PsM12_Summary <- rbind (MaF2_anova_PsM12$Chisq[2], MaF2_anova_PsM12$P[2])
MaF2_TotFl1_Summary <- rbind (MaF2_anova_TotFl1$Chisq[2], MaF2_anova_TotFl1$P[2])

#COMBINING DATA FOR SUMMARY TABLES 1 AND 3

SpMaF1F2rownames <- rbind("Chisq", "P-value")

SpMacomparisons_colnames <- cbind(
"Trait",
"Nor_SpMa_Surv", 
"Nor_SpMa_Flprob1", 	"Nor_SpMa_Flprob2", 	"Nor_SpMa_Flprob3",	"Nor_SpMa_Flprob4",
"Nor_SpMa_Infl1",		"Nor_SpMa_Infl2",		"Nor_SpMa_Infl3",
"Nor_SpMa_Frinfl1",	"Nor_SpMa_Frinfl2",	"Nor_SpMa_Frinfl3",
"Nor_SpMa_Seedsfr1",	"Nor_SpMa_Seedsfr2",	"Nor_SpMa_Seedsfr3",
"Nor_SpMa_FlStart1",	"Nor_SpMa_FlStart2",	"Nor_SpMa_FlStart3",
"Nor_SpMa_Infllength",	"Nor_SpMa_Petalsize",	"Nor_SpMa_Flowers")

SpF1comparisons_colnames <- cbind(
"Trait",
"Nor_SpF1_Surv", 
"Nor_SpF1_Flprob1", 	"Nor_SpF1_Flprob2", 	"Nor_SpF1_Flprob3",	"Nor_SpF1_Flprob4",
"Nor_SpF1_Infl1",		"Nor_SpF1_Infl2",		"Nor_SpF1_Infl3",
"Nor_SpF1_Frinfl1",	"Nor_SpF1_Frinfl2",	"Nor_SpF1_Frinfl3",
"Nor_SpF1_Seedsfr1",	"Nor_SpF1_Seedsfr2",	"Nor_SpF1_Seedsfr3",
"Nor_SpF1_FlStart1",	"Nor_SpF1_FlStart2",	"Nor_SpF1_FlStart3",
"Nor_SpF1_Infllength",	"Nor_SpF1_Petalsize",	"Nor_SpF1_Flowers")

SpF2comparisons_colnames <- cbind(
"Trait",
"Nor_SpF2_Surv", 
"Nor_SpF2_Flprob1", 	"Nor_SpF2_Flprob2", 	"Nor_SpF2_Flprob3",	"Nor_SpF2_Flprob4",
"Nor_SpF2_Infl1",		"Nor_SpF2_Infl2",		"Nor_SpF2_Infl3",
"Nor_SpF2_Frinfl1",	"Nor_SpF2_Frinfl2",	"Nor_SpF2_Frinfl3",
"Nor_SpF2_Seedsfr1",	"Nor_SpF2_Seedsfr2",	"Nor_SpF2_Seedsfr3",
"Nor_SpF2_FlStart1",	"Nor_SpF2_FlStart2",	"Nor_SpF2_FlStart3",
"Nor_SpF2_Infllength",	"Nor_SpF2_Petalsize",	"Nor_SpF2_Flowers")

MaF1comparisons_colnames <- cbind(
"Trait",
"Nor_MaF1_Surv", 
"Nor_MaF1_Flprob1", 	"Nor_MaF1_Flprob2", 	"Nor_MaF1_Flprob3",	"Nor_MaF1_Flprob4",
"Nor_MaF1_Infl1",		"Nor_MaF1_Infl2",		"Nor_MaF1_Infl3",
"Nor_MaF1_Frinfl1",	"Nor_MaF1_Frinfl2",	"Nor_MaF1_Frinfl3",
"Nor_MaF1_Seedsfr1",	"Nor_MaF1_Seedsfr2",	"Nor_MaF1_Seedsfr3",
"Nor_MaF1_FlStart1",	"Nor_MaF1_FlStart2",	"Nor_MaF1_FlStart3",
"Nor_MaF1_Infllength",	"Nor_MaF1_Petalsize",	"Nor_MaF1_Flowers")

MaF2comparisons_colnames <- cbind(
"Trait",
"Nor_MaF2_Surv", 
"Nor_MaF2_Flprob1", 	"Nor_MaF2_Flprob2", 	"Nor_MaF2_Flprob3",	"Nor_MaF2_Flprob4",
"Nor_MaF2_Infl1",		"Nor_MaF2_Infl2",		"Nor_MaF2_Infl3",
"Nor_MaF2_Frinfl1",	"Nor_MaF2_Frinfl2",	"Nor_MaF2_Frinfl3",
"Nor_MaF2_Seedsfr1",	"Nor_MaF2_Seedsfr2",	"Nor_MaF2_Seedsfr3",
"Nor_MaF2_FlStart1",	"Nor_MaF2_FlStart2",	"Nor_MaF2_FlStart3",
"Nor_MaF2_Infllength",	"Nor_MaF2_Petalsize",	"Nor_MaF2_Flowers")


SpMa_Summary <- cbind(SpMaF1F2rownames,
SpMa_CSurv_Summary, 
SpMa_Flow1_Summary, 	SpMa_Flow2_Summary, 	SpMa_Flow3_Summary, SpMa_Flow4_Summary,
SpMa_TotSh1_Summary, 	SpMa_TotSh2_Summary, 	SpMa_TotSh3_Summary, 
SpMa_FrSh1_Summary, 	SpMa_FrSh2_Summary, 	SpMa_FrSh3_Summary, 
SpMa_SeFrG1_Summary, 	SpMa_SeFrG2_Summary, 	SpMa_SeFrG3_Summary, 
SpMa_FlStart1_Summary, 	SpMa_FlStart2_Summary, 	SpMa_FlStart3_Summary, 
SpMa_H1Sh1_Summary, 	
SpMa_PsM12_Summary, 	
SpMa_TotFl1_Summary)


SpF1_Summary <- cbind(SpMaF1F2rownames,
SpF1_CSurv_Summary, 
SpF1_Flow1_Summary, 	SpF1_Flow2_Summary, 	SpF1_Flow3_Summary, SpF1_Flow4_Summary,
SpF1_TotSh1_Summary, 	SpF1_TotSh2_Summary, 	SpF1_TotSh3_Summary, 
SpF1_FrSh1_Summary, 	SpF1_FrSh2_Summary, 	SpF1_FrSh3_Summary, 
SpF1_SeFrG1_Summary, 	SpF1_SeFrG2_Summary, 	SpF1_SeFrG3_Summary, 
SpF1_FlStart1_Summary, 	SpF1_FlStart2_Summary, 	SpF1_FlStart3_Summary, 
SpF1_H1Sh1_Summary, 	
SpF1_PsM12_Summary, 	
SpF1_TotFl1_Summary)


SpF2_Summary <- cbind(SpMaF1F2rownames,
SpF2_CSurv_Summary, 
SpF2_Flow1_Summary, 	SpF2_Flow2_Summary, 	SpF2_Flow3_Summary, SpF2_Flow4_Summary,
SpF2_TotSh1_Summary, 	SpF2_TotSh2_Summary, 	SpF2_TotSh3_Summary, 
SpF2_FrSh1_Summary, 	SpF2_FrSh2_Summary, 	SpF2_FrSh3_Summary, 
SpF2_SeFrG1_Summary, 	SpF2_SeFrG2_Summary, 	SpF2_SeFrG3_Summary, 
SpF2_FlStart1_Summary, 	SpF2_FlStart2_Summary, 	SpF2_FlStart3_Summary, 
SpF2_H1Sh1_Summary, 	
SpF2_PsM12_Summary, 	
SpF2_TotFl1_Summary)


MaF1_Summary <- cbind(SpMaF1F2rownames,
MaF1_CSurv_Summary, 
MaF1_Flow1_Summary, 	MaF1_Flow2_Summary, 	MaF1_Flow3_Summary, MaF1_Flow4_Summary,
MaF1_TotSh1_Summary, 	MaF1_TotSh2_Summary, 	MaF1_TotSh3_Summary, 
MaF1_FrSh1_Summary, 	MaF1_FrSh2_Summary, 	MaF1_FrSh3_Summary, 
MaF1_SeFrG1_Summary, 	MaF1_SeFrG2_Summary, 	MaF1_SeFrG3_Summary, 
MaF1_FlStart1_Summary, 	MaF1_FlStart2_Summary, 	MaF1_FlStart3_Summary, 
MaF1_H1Sh1_Summary, 	
MaF1_PsM12_Summary, 	
MaF1_TotFl1_Summary)

MaF2_Summary <- cbind(SpMaF1F2rownames,
MaF2_CSurv_Summary, 
MaF2_Flow1_Summary, 	MaF2_Flow2_Summary, 	MaF2_Flow3_Summary, MaF2_Flow4_Summary,
MaF2_TotSh1_Summary, 	MaF2_TotSh2_Summary, 	MaF2_TotSh3_Summary, 
MaF2_FrSh1_Summary, 	MaF2_FrSh2_Summary, 	MaF2_FrSh3_Summary, 
MaF2_SeFrG1_Summary, 	MaF2_SeFrG2_Summary, 	MaF2_SeFrG3_Summary, 
MaF2_FlStart1_Summary, 	MaF2_FlStart2_Summary, 	MaF2_FlStart3_Summary, 
MaF2_H1Sh1_Summary, 	
MaF2_PsM12_Summary, 	
MaF2_TotFl1_Summary)

# RESULTS SHOWN IN TABLES 1 AND 3
# note the different order of the columns in the paper

rbind (SpMacomparisons_colnames,SpMa_Summary)

rbind (SpF1comparisons_colnames,SpF1_Summary)

rbind (SpF2comparisons_colnames,SpF2_Summary)

rbind (MaF1comparisons_colnames,MaF1_Summary)

rbind (MaF2comparisons_colnames,MaF2_Summary)



# testing skewed traits with Kruskal-Wallis (shown in table 3)

SpMa_KW_FlStart1 <- kruskal.test (FlStart1~Pop, subset=(Pop == "N" | Pop == "S"), data=sns4) 
SpMa_KW_FlStart2 <- kruskal.test (FlStart2~Pop, subset=(Pop == "N" | Pop == "S"),data=sns4) 
SpMa_KW_FlStart3 <- kruskal.test (FlStart3~Pop, subset=(Pop == "N" | Pop == "S"),data=sns4) 

SpF1_KW_FlStart1 <- kruskal.test (FlStart1~Pop, subset=(Pop == "F1" | Pop == "S"), data=sns4) 
SpF1_KW_FlStart2 <- kruskal.test (FlStart2~Pop, subset=(Pop == "F1" | Pop == "S"),data=sns4) 
SpF1_KW_FlStart3 <- kruskal.test (FlStart3~Pop, subset=(Pop == "F1" | Pop == "S"),data=sns4) 

SpF2_KW_FlStart1 <- kruskal.test (FlStart1~Pop, subset=(Pop == "F2" | Pop == "S"), data=sns4) 
SpF2_KW_FlStart2 <- kruskal.test (FlStart2~Pop, subset=(Pop == "F2" | Pop == "S"),data=sns4) 
SpF2_KW_FlStart3 <- kruskal.test (FlStart3~Pop, subset=(Pop == "F2" | Pop == "S"),data=sns4) 

MaF1_KW_FlStart1 <- kruskal.test (FlStart1~Pop, subset=(Pop == "F1" | Pop == "N"), data=sns4) 
MaF1_KW_FlStart2 <- kruskal.test (FlStart2~Pop, subset=(Pop == "F1" | Pop == "N"),data=sns4) 
MaF1_KW_FlStart3 <- kruskal.test (FlStart3~Pop, subset=(Pop == "F1" | Pop == "N"),data=sns4) 

MaF2_KW_FlStart1 <- kruskal.test (FlStart1~Pop, subset=(Pop == "F2" | Pop == "N"), data=sns4) 
MaF2_KW_FlStart2 <- kruskal.test (FlStart2~Pop, subset=(Pop == "F2" | Pop == "N"),data=sns4) 
MaF2_KW_FlStart3 <- kruskal.test (FlStart3~Pop, subset=(Pop == "F2" | Pop == "N"),data=sns4) 



# TABLE 4 - EPISTASIS

table (sns4$Pop, sns4$FamA)

#SETTING UP CONTRASTS FOR EPISTASIS TESTING
			#	 F1,	F2,	Ma,	Sp)
Fam4Contrast <-c(	 -2,	4,  	-1,	-1)

dim(Fam4Contrast ) <- c(4,1)

#LMER MODELS FOR ANALYSES IN TABLE 4

CSurvRFull4ctr 		<- lmer	(SurvWin4 	~ Pop + (1|Block)+ (1|Fam)	  					, REML=FALSE, family=binomial, data=sns4,  contrasts=list(Pop=Fam4Contrast))
Flow1RFull4ctr 		<- lmer	(Flow1 	~ Pop + (1|Block)+ (1|Fam)	, subset=(SurvWin1 == 1) 	, REML=FALSE, family=binomial, data=sns4,  contrasts=list(Pop=Fam4Contrast))
Flow2RFull4ctr 		<- lmer	(Flow2 	~ Pop + (1|Block)+ (1|Fam)	, subset=(SurvWin2 == 1) 	, REML=FALSE, family=binomial, data=sns4,  contrasts=list(Pop=Fam4Contrast))
Flow3RFull4ctr 		<- lmer	(Flow3 	~ Pop + (1|Block)+ (1|Fam)	, subset=(SurvWin3 == 1) 	, REML=FALSE, family=binomial, data=sns4,  contrasts=list(Pop=Fam4Contrast))
Flow4RFull4ctr 		<- lmer	(Flow4 	~ Pop + (1|Block)+ (1|Fam)	, subset=(SurvWin4 == 1) 	, REML=FALSE, family=binomial, data=sns4,  contrasts=list(Pop=Fam4Contrast))
TotSh1RFull4ctr 		<- lmer(sqrt(TotSh1)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4,  contrasts=list(Pop=Fam4Contrast))
TotSh2RFull4ctr 		<- lmer(sqrt(TotSh2)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4,  contrasts=list(Pop=Fam4Contrast))
TotSh3RFull4ctr 		<- lmer(sqrt(TotSh3)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4,  contrasts=list(Pop=Fam4Contrast))
FrSh1RFull4ctr 		<- lmer(sqrt(FrSh1)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4,  contrasts=list(Pop=Fam4Contrast))
FrSh2RFull4ctr 		<- lmer(sqrt(FrSh2)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4,  contrasts=list(Pop=Fam4Contrast))
FrSh3RFull4ctr 		<- lmer(sqrt(FrSh3)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1)	 	, REML=FALSE, family=gaussian, data=sns4,  contrasts=list(Pop=Fam4Contrast))
SeFrG1RFull4ctr 		<- lmer(sqrt(SeFrG1)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4,  contrasts=list(Pop=Fam4Contrast))
SeFrG2RFull4ctr 		<- lmer(sqrt(SeFrG2)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4,  contrasts=list(Pop=Fam4Contrast))
FlStart1RFull4ctr 	<- lmer(sqrt(FlStart1)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4,  contrasts=list(Pop=Fam4Contrast))
FlStart2RFull4ctr 	<- lmer(sqrt(FlStart2)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow2 == 1)	 	, REML=FALSE, family=gaussian, data=sns4,  contrasts=list(Pop=Fam4Contrast))
FlStart3RFull4ctr 	<- lmer(sqrt(FlStart3)	~ Pop + (1|Block)+ (1|Fam)	, subset=(Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4,  contrasts=list(Pop=Fam4Contrast))
H1Sh1RFull4ctr 		<- lmer(sqrt(H1Sh1)	~ Pop + (1|Block)+ (1|Fam)	 			 		, REML=FALSE, family=gaussian, data=sns4,  contrasts=list(Pop=Fam4Contrast))
PsM12RFull4ctr 		<- lmer	(PsM12	~ Pop + (1|Block)+ (1|Fam)	 			 		, REML=FALSE, family=gaussian, data=sns4,  contrasts=list(Pop=Fam4Contrast))
TotFl1RFull4ctr 		<- lmer(sqrt(TotFl1)	~ Pop + (1|Block)+ (1|Fam)						, REML=FALSE, family=gaussian, data=sns4,  contrasts=list(Pop=Fam4Contrast))

#MCMC RUNS FOR TABLE 4
  	
TotSh1RFull4ctr_mcmc 	<- mcmcsamp(TotSh1RFull4ctr, 		n = 1000)  	
TotSh2RFull4ctr_mcmc 	<- mcmcsamp(TotSh2RFull4ctr, 		n = 1000)  	
TotSh3RFull4ctr_mcmc 	<- mcmcsamp(TotSh3RFull4ctr, 		n = 1000)  	
FrSh1RFull4ctr_mcmc 	<- mcmcsamp(FrSh1RFull4ctr, 		n = 1000)  	
FrSh2RFull4ctr_mcmc 	<- mcmcsamp(FrSh2RFull4ctr, 		n = 1000)  	
FrSh3RFull4ctr_mcmc 	<- mcmcsamp(FrSh3RFull4ctr, 		n = 1000)  	
SeFrG1RFull4ctr_mcmc 	<- mcmcsamp(SeFrG1RFull4ctr, 		n = 1000)  	
SeFrG2RFull4ctr_mcmc 	<- mcmcsamp(SeFrG2RFull4ctr, 		n = 1000)  	
FlStart1RFull4ctr_mcmc 	<- mcmcsamp(FlStart1RFull4ctr, 	n = 1000)  	
FlStart2RFull4ctr_mcmc 	<- mcmcsamp(FlStart2RFull4ctr, 	n = 1000)  	
FlStart3RFull4ctr_mcmc 	<- mcmcsamp(FlStart3RFull4ctr, 	n = 1000)
H1Sh1RFull4ctr_mcmc 	<- mcmcsamp(H1Sh1RFull4ctr, 		n = 1000)  	
PsM12RFull4ctr_mcmc 	<- mcmcsamp(PsM12RFull4ctr, 		n = 1000)  	
TotFl1RFull4ctr_mcmc 	<- mcmcsamp(TotFl1RFull4ctr, 		n = 1000)  	

# RESULTS PRESENTED IN TABLE 4
summary (CSurvRFull4ctr)
summary (Flow1RFull4ctr )
summary (Flow2RFull4ctr )
summary (Flow3RFull4ctr )
summary (Flow4RFull4ctr )
HPDinterval(TotSh1RFull4ctr_mcmc, 	prob = 0.95)  	
HPDinterval(TotSh2RFull4ctr_mcmc, 	prob = 0.95)  	
HPDinterval(TotSh3RFull4ctr_mcmc, 	prob = 0.95)  	
HPDinterval(FrSh1RFull4ctr_mcmc, 	prob = 0.95)  	
HPDinterval(FrSh2RFull4ctr_mcmc, 	prob = 0.95)  	
HPDinterval(FrSh3RFull4ctr_mcmc, 	prob = 0.95)  	
HPDinterval(SeFrG1RFull4ctr_mcmc, 	prob = 0.95)  	
HPDinterval(SeFrG2RFull4ctr_mcmc, 	prob = 0.95)  	
HPDinterval(FlStart1RFull4ctr_mcmc, prob = 0.95)  	
HPDinterval(FlStart2RFull4ctr_mcmc, prob = 0.95)  	
HPDinterval(FlStart3RFull4ctr_mcmc, prob = 0.95)
HPDinterval(H1Sh1RFull4ctr_mcmc, 	prob = 0.95)  	
HPDinterval(PsM12RFull4ctr_mcmc, 	prob = 0.95)  	
HPDinterval(TotFl1RFull4ctr_mcmc, 	prob = 0.95)  	



#LMER MODELS FOR ANALYSES IN TABLE 5

#F1 - full models

CSurvRF1addCF 	<- lmer	(SurvWin4 	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1") 				, REML=FALSE, family=binomial, data=sns4)
Flow1RF1addCF 	<- lmer	(Flow1 	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1" & SurvWin1 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow2RF1addCF 	<- lmer	(Flow2 	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1" & SurvWin2 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow3RF1addCF 	<- lmer	(Flow3 	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1" & SurvWin3 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow4RF1addCF 	<- lmer	(Flow4 	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1" & SurvWin4 == 1) 	, REML=FALSE, family=binomial, data=sns4)
TotSh1RF1addCF 	<- lmer(sqrt(TotSh1)	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1" & Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh2RF1addCF 	<- lmer(sqrt(TotSh2)	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1" & Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh3RF1addCF 	<- lmer(sqrt(TotSh3)	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1" & Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh1RF1addCF 	<- lmer(sqrt(FrSh1)	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1" & Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh2RF1addCF 	<- lmer(sqrt(FrSh2)	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1" & Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh3RF1addCF 	<- lmer(sqrt(FrSh3)	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1" & Flow3 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
SeFrG1RF1addCF 	<- lmer(sqrt(SeFrG1)	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1" & Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG2RF1addCF 	<- lmer(sqrt(SeFrG2)	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1" & Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG3RF1addCF 	<- lmer(sqrt(SeFrG3)	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1" & Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart1RF1addCF 	<- lmer(sqrt(FlStart1)	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1" & Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart2RF1addCF 	<- lmer(sqrt(FlStart2)	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1" & Flow2 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
FlStart3RF1addCF 	<- lmer(sqrt(FlStart3)	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1" & Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
H1Sh1RF1addCF 	<- lmer(sqrt(H1Sh1)	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1")			 	, REML=FALSE, family=gaussian, data=sns4)
PsM12RF1addCF 	<- lmer	(PsM12	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1")			 	, REML=FALSE, family=gaussian, data=sns4)
TotFl1RF1addCF 	<- lmer(sqrt(TotFl1)	~ CrossPop + FamA + (1|Block)	, subset=(Pop=="F1")				, REML=FALSE, family=gaussian, data=sns4)

#F1 - reduced models

CSurvRF1FamCF 	<- lmer	(SurvWin4 	~ FamA + (1|Block)	, subset=(Pop=="F1") 				, REML=FALSE, family=binomial, data=sns4)
Flow1RF1FamCF 	<- lmer	(Flow1 	~ FamA + (1|Block)	, subset=(Pop=="F1" & SurvWin1 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow2RF1FamCF 	<- lmer	(Flow2 	~ FamA + (1|Block)	, subset=(Pop=="F1" & SurvWin2 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow3RF1FamCF 	<- lmer	(Flow3 	~ FamA + (1|Block)	, subset=(Pop=="F1" & SurvWin3 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow4RF1FamCF 	<- lmer	(Flow4 	~ FamA + (1|Block)	, subset=(Pop=="F1" & SurvWin4 == 1) 	, REML=FALSE, family=binomial, data=sns4)
TotSh1RF1FamCF 	<- lmer(sqrt(TotSh1)	~ FamA + (1|Block)	, subset=(Pop=="F1" & Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh2RF1FamCF 	<- lmer(sqrt(TotSh2)	~ FamA + (1|Block)	, subset=(Pop=="F1" & Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh3RF1FamCF 	<- lmer(sqrt(TotSh3)	~ FamA + (1|Block)	, subset=(Pop=="F1" & Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh1RF1FamCF 	<- lmer(sqrt(FrSh1)	~ FamA + (1|Block)	, subset=(Pop=="F1" & Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh2RF1FamCF 	<- lmer(sqrt(FrSh2)	~ FamA + (1|Block)	, subset=(Pop=="F1" & Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh3RF1FamCF 	<- lmer(sqrt(FrSh3)	~ FamA + (1|Block)	, subset=(Pop=="F1" & Flow3 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
SeFrG1RF1FamCF 	<- lmer(sqrt(SeFrG1)	~ FamA + (1|Block)	, subset=(Pop=="F1" & Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG2RF1FamCF 	<- lmer(sqrt(SeFrG2)	~ FamA + (1|Block)	, subset=(Pop=="F1" & Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG3RF1FamCF 	<- lmer(sqrt(SeFrG3)	~ FamA + (1|Block)	, subset=(Pop=="F1" & Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart1RF1FamCF 	<- lmer(sqrt(FlStart1)	~ FamA + (1|Block)	, subset=(Pop=="F1" & Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart2RF1FamCF 	<- lmer(sqrt(FlStart2)	~ FamA + (1|Block)	, subset=(Pop=="F1" & Flow2 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
FlStart3RF1FamCF 	<- lmer(sqrt(FlStart3)	~ FamA + (1|Block)	, subset=(Pop=="F1" & Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
H1Sh1RF1FamCF 	<- lmer(sqrt(H1Sh1)	~ FamA + (1|Block)	, subset=(Pop=="F1")			 	, REML=FALSE, family=gaussian, data=sns4)
PsM12RF1FamCF 	<- lmer	(PsM12	~ FamA + (1|Block)	, subset=(Pop=="F1")			 	, REML=FALSE, family=gaussian, data=sns4)
TotFl1RF1FamCF 	<- lmer(sqrt(TotFl1)	~ FamA + (1|Block)	, subset=(Pop=="F1")				, REML=FALSE, family=gaussian, data=sns4)


# F2 full models

CSurvRF2Full 	<- lmer	(SurvWin4 	~ Fam + (1|Block)	, subset=(Pop=="F2") 				, REML=FALSE, family=binomial, data=sns4)
Flow1RF2Full 	<- lmer	(Flow1 	~ Fam + (1|Block)	, subset=(Pop=="F2" & SurvWin1 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow2RF2Full 	<- lmer	(Flow2 	~ Fam + (1|Block)	, subset=(Pop=="F2" & SurvWin2 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow3RF2Full 	<- lmer	(Flow3 	~ Fam + (1|Block)	, subset=(Pop=="F2" & SurvWin3 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow4RF2Full 	<- lmer	(Flow4 	~ Fam + (1|Block)	, subset=(Pop=="F2" & SurvWin4 == 1) 	, REML=FALSE, family=binomial, data=sns4)
TotSh1RF2Full 	<- lmer(sqrt(TotSh1)	~ Fam + (1|Block)	, subset=(Pop=="F2" & Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh2RF2Full 	<- lmer(sqrt(TotSh2)	~ Fam + (1|Block)	, subset=(Pop=="F2" & Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh3RF2Full 	<- lmer(sqrt(TotSh3)	~ Fam + (1|Block)	, subset=(Pop=="F2" & Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh1RF2Full 	<- lmer(sqrt(FrSh1)	~ Fam + (1|Block)	, subset=(Pop=="F2" & Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh2RF2Full 	<- lmer(sqrt(FrSh2)	~ Fam + (1|Block)	, subset=(Pop=="F2" & Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh3RF2Full 	<- lmer(sqrt(FrSh3)	~ Fam + (1|Block)	, subset=(Pop=="F2" & Flow3 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
SeFrG1RF2Full 	<- lmer(sqrt(SeFrG1)	~ Fam + (1|Block)	, subset=(Pop=="F2" & Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG2RF2Full 	<- lmer(sqrt(SeFrG2)	~ Fam + (1|Block)	, subset=(Pop=="F2" & Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG3RF2Full 	<- lmer(sqrt(SeFrG3)	~ Fam + (1|Block)	, subset=(Pop=="F2" & Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart1RF2Full 	<- lmer(sqrt(FlStart1)	~ Fam + (1|Block)	, subset=(Pop=="F2" & Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart2RF2Full 	<- lmer(sqrt(FlStart2)	~ Fam + (1|Block)	, subset=(Pop=="F2" & Flow2 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
FlStart3RF2Full 	<- lmer(sqrt(FlStart3)	~ Fam + (1|Block)	, subset=(Pop=="F2" & Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
H1Sh1RF2Full 	<- lmer(sqrt(H1Sh1)	~ Fam + (1|Block)	, subset=(Pop=="F2")			 	, REML=FALSE, family=gaussian, data=sns4)
PsM12RF2Full 	<- lmer	(PsM12	~ Fam + (1|Block)	, subset=(Pop=="F2")			 	, REML=FALSE, family=gaussian, data=sns4)
TotFl1RF2Full 	<- lmer(sqrt(TotFl1)	~ Fam + (1|Block)	, subset=(Pop=="F2")				, REML=FALSE, family=gaussian, data=sns4)

# F2 - reduced models


CSurvRF2Rec 	<- lmer	(SurvWin4 	~ 1 + (1|Block)	, subset=(Pop=="F2") 				, REML=FALSE, family=binomial, data=sns4)
Flow1RF2Rec 	<- lmer	(Flow1 	~ 1 + (1|Block)	, subset=(Pop=="F2" & SurvWin1 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow2RF2Rec 	<- lmer	(Flow2 	~ 1 + (1|Block)	, subset=(Pop=="F2" & SurvWin2 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow3RF2Rec 	<- lmer	(Flow3 	~ 1 + (1|Block)	, subset=(Pop=="F2" & SurvWin3 == 1) 	, REML=FALSE, family=binomial, data=sns4)
Flow4RF2Rec 	<- lmer	(Flow4 	~ 1 + (1|Block)	, subset=(Pop=="F2" & SurvWin4 == 1) 	, REML=FALSE, family=binomial, data=sns4)
TotSh1RF2Rec 	<- lmer(sqrt(TotSh1)	~ 1 + (1|Block)	, subset=(Pop=="F2" & Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh2RF2Rec 	<- lmer(sqrt(TotSh2)	~ 1 + (1|Block)	, subset=(Pop=="F2" & Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
TotSh3RF2Rec 	<- lmer(sqrt(TotSh3)	~ 1 + (1|Block)	, subset=(Pop=="F2" & Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh1RF2Rec 	<- lmer(sqrt(FrSh1)	~ 1 + (1|Block)	, subset=(Pop=="F2" & Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh2RF2Rec 	<- lmer(sqrt(FrSh2)	~ 1 + (1|Block)	, subset=(Pop=="F2" & Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FrSh3RF2Rec 	<- lmer(sqrt(FrSh3)	~ 1 + (1|Block)	, subset=(Pop=="F2" & Flow3 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
SeFrG1RF2Rec 	<- lmer(sqrt(SeFrG1)	~ 1 + (1|Block)	, subset=(Pop=="F2" & Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG2RF2Rec 	<- lmer(sqrt(SeFrG2)	~ 1 + (1|Block)	, subset=(Pop=="F2" & Flow2 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
SeFrG3RF2Rec 	<- lmer(sqrt(SeFrG3)	~ 1 + (1|Block)	, subset=(Pop=="F2" & Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart1RF2Rec 	<- lmer(sqrt(FlStart1)	~ 1 + (1|Block)	, subset=(Pop=="F2" & Flow1 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
FlStart2RF2Rec 	<- lmer(sqrt(FlStart2)	~ 1 + (1|Block)	, subset=(Pop=="F2" & Flow2 == 1)	 	, REML=FALSE, family=gaussian, data=sns4)
FlStart3RF2Rec 	<- lmer(sqrt(FlStart3)	~ 1 + (1|Block)	, subset=(Pop=="F2" & Flow3 == 1) 		, REML=FALSE, family=gaussian, data=sns4)
H1Sh1RF2Rec 	<- lmer(sqrt(H1Sh1)	~ 1 + (1|Block)	, subset=(Pop=="F2")			 	, REML=FALSE, family=gaussian, data=sns4)
PsM12RF2Rec 	<- lmer	(PsM12	~ 1 + (1|Block)	, subset=(Pop=="F2")			 	, REML=FALSE, family=gaussian, data=sns4)
TotFl1RF2Rec 	<- lmer(sqrt(TotFl1)	~ 1 + (1|Block)	, subset=(Pop=="F2")				, REML=FALSE, family=gaussian, data=sns4)




#LIKELIHOOD RATIO TESTS FOR THE NORWAY SITE - TABLE 5

#F1 reciprocals - IN NORWAY

F1recip_anova_cytop_CSurv <- anova(CSurvRF1FamCF, CSurvRF1addCF)  	
F1recip_anova_cytop_Flow1 <-anova(Flow1RF1FamCF, Flow1RF1addCF)  	
F1recip_anova_cytop_Flow2 <-anova(Flow2RF1FamCF, Flow2RF1addCF)  	
F1recip_anova_cytop_Flow3 <-anova(Flow3RF1FamCF, Flow3RF1addCF)
F1recip_anova_cytop_Flow4 <-anova(Flow4RF1FamCF, Flow4RF1addCF)  	  	
F1recip_anova_cytop_TotSh1 <-anova(TotSh1RF1FamCF, TotSh1RF1addCF)  	
F1recip_anova_cytop_TotSh2 <-anova(TotSh2RF1FamCF, TotSh2RF1addCF)  	
F1recip_anova_cytop_TotSh3 <-anova(TotSh3RF1FamCF, TotSh3RF1addCF)  	
F1recip_anova_cytop_FrSh1 <-anova(FrSh1RF1FamCF, FrSh1RF1addCF)  	
F1recip_anova_cytop_FrSh2 <-anova(FrSh2RF1FamCF, FrSh2RF1addCF)
F1recip_anova_cytop_FrSh3 <-anova(FrSh3RF1FamCF, FrSh3RF1addCF)
F1recip_anova_cytop_SeFrG1 <-anova(SeFrG1RF1FamCF, SeFrG1RF1addCF)
F1recip_anova_cytop_SeFrG2 <-anova(SeFrG2RF1FamCF, SeFrG2RF1addCF)
F1recip_anova_cytop_SeFrG3 <-anova(SeFrG3RF1FamCF, SeFrG3RF1addCF)
F1recip_anova_cytop_FlStart1 <-anova(FlStart1RF1FamCF, FlStart1RF1addCF)
F1recip_anova_cytop_FlStart2 <-anova(FlStart2RF1FamCF, FlStart2RF1addCF)
F1recip_anova_cytop_FlStart3 <-anova(FlStart3RF1FamCF, FlStart3RF1addCF)
F1recip_anova_cytop_H1Sh1 <-anova(H1Sh1RF1FamCF, H1Sh1RF1addCF)
F1recip_anova_cytop_PsM12 <-anova(PsM12RF1FamCF, PsM12RF1addCF)
F1recip_anova_cytop_TotFl1 <-anova(TotFl1RF1FamCF, TotFl1RF1addCF)

#F2 reciprocals - IN NORWAY

F2recip_anova_cytop_CSurv <- anova(CSurvRF2Full, CSurvRF2Rec)  	
F2recip_anova_cytop_Flow1 <-anova(Flow1RF2Full, Flow1RF2Rec)  	
F2recip_anova_cytop_Flow2 <-anova(Flow2RF2Full, Flow2RF2Rec)  	
F2recip_anova_cytop_Flow3 <-anova(Flow3RF2Full, Flow3RF2Rec)
F2recip_anova_cytop_Flow4 <-anova(Flow4RF2Full, Flow4RF2Rec)  	  	
F2recip_anova_cytop_TotSh1 <-anova(TotSh1RF2Full, TotSh1RF2Rec)  	
F2recip_anova_cytop_TotSh2 <-anova(TotSh2RF2Full, TotSh2RF2Rec)  	
F2recip_anova_cytop_TotSh3 <-anova(TotSh3RF2Full, TotSh3RF2Rec)  	
F2recip_anova_cytop_FrSh1 <-anova(FrSh1RF2Full, FrSh1RF2Rec)  	
F2recip_anova_cytop_FrSh2 <-anova(FrSh2RF2Full, FrSh2RF2Rec)
F2recip_anova_cytop_FrSh3 <-anova(FrSh3RF2Full, FrSh3RF2Rec)
F2recip_anova_cytop_SeFrG1 <-anova(SeFrG1RF2Full, SeFrG1RF2Rec)
F2recip_anova_cytop_SeFrG2 <-anova(SeFrG2RF2Full, SeFrG2RF2Rec)
F2recip_anova_cytop_SeFrG3 <-anova(SeFrG3RF2Full, SeFrG3RF2Rec)
F2recip_anova_cytop_FlStart1 <-anova(FlStart1RF2Full, FlStart1RF2Rec)
F2recip_anova_cytop_FlStart2 <-anova(FlStart2RF2Full, FlStart2RF2Rec)
F2recip_anova_cytop_FlStart3 <-anova(FlStart3RF2Full, FlStart3RF2Rec)
F2recip_anova_cytop_H1Sh1 <-anova(H1Sh1RF2Full, H1Sh1RF2Rec)
F2recip_anova_cytop_PsM12 <-anova(PsM12RF2Full, PsM12RF2Rec)
F2recip_anova_cytop_TotFl1 <-anova(TotFl1RF2Full, TotFl1RF2Rec)


#EXTRACTING RESULTS FOR TABLE 5

#F1 reciprocals - IN NORWAY


F1recip_CSurv_cytop_summary <- rbind (F1recip_anova_cytop_CSurv$Chisq[2], F1recip_anova_cytop_CSurv$P[2])
F1recip_Flow1_cytop_summary <- rbind (F1recip_anova_cytop_Flow1$Chisq[2], F1recip_anova_cytop_Flow1$P[2])
F1recip_Flow2_cytop_summary <- rbind (F1recip_anova_cytop_Flow2$Chisq[2], F1recip_anova_cytop_Flow2$P[2])
F1recip_Flow3_cytop_summary <- rbind (F1recip_anova_cytop_Flow3$Chisq[2], F1recip_anova_cytop_Flow3$P[2])
F1recip_Flow4_cytop_summary <- rbind (F1recip_anova_cytop_Flow4$Chisq[2], F1recip_anova_cytop_Flow4$P[2])
F1recip_TotSh1_cytop_summary <- rbind (F1recip_anova_cytop_TotSh1$Chisq[2], F1recip_anova_cytop_TotSh1$P[2])
F1recip_TotSh2_cytop_summary <- rbind (F1recip_anova_cytop_TotSh2$Chisq[2], F1recip_anova_cytop_TotSh2$P[2])
F1recip_TotSh3_cytop_summary <- rbind (F1recip_anova_cytop_TotSh3$Chisq[2], F1recip_anova_cytop_TotSh3$P[2])
F1recip_FrSh1_cytop_summary <- rbind (F1recip_anova_cytop_FrSh1$Chisq[2], F1recip_anova_cytop_FrSh1$P[2])
F1recip_FrSh2_cytop_summary <- rbind (F1recip_anova_cytop_FrSh2$Chisq[2], F1recip_anova_cytop_FrSh2$P[2])
F1recip_FrSh3_cytop_summary <- rbind (F1recip_anova_cytop_FrSh3$Chisq[2], F1recip_anova_cytop_FrSh3$P[2])
F1recip_SeFrG1_cytop_summary <- rbind (F1recip_anova_cytop_SeFrG1$Chisq[2], F1recip_anova_cytop_SeFrG1$P[2])
F1recip_SeFrG2_cytop_summary <- rbind (F1recip_anova_cytop_SeFrG2$Chisq[2], F1recip_anova_cytop_SeFrG2$P[2])
F1recip_SeFrG3_cytop_summary <- rbind (F1recip_anova_cytop_SeFrG3$Chisq[2], F1recip_anova_cytop_SeFrG3$P[2])
F1recip_FlStart1_cytop_summary <- rbind (F1recip_anova_cytop_FlStart1$Chisq[2], F1recip_anova_cytop_FlStart1$P[2])
F1recip_FlStart2_cytop_summary <- rbind (F1recip_anova_cytop_FlStart2$Chisq[2], F1recip_anova_cytop_FlStart2$P[2])
F1recip_FlStart3_cytop_summary <- rbind (F1recip_anova_cytop_FlStart3$Chisq[2], F1recip_anova_cytop_FlStart3$P[2])
F1recip_H1Sh1_cytop_summary <- rbind (F1recip_anova_cytop_H1Sh1$Chisq[2], F1recip_anova_cytop_H1Sh1$P[2])
F1recip_PsM12_cytop_summary <- rbind (F1recip_anova_cytop_PsM12$Chisq[2], F1recip_anova_cytop_PsM12$P[2])
F1recip_TotFl1_cytop_summary <- rbind (F1recip_anova_cytop_TotFl1$Chisq[2], F1recip_anova_cytop_TotFl1$P[2])

#F2 reciprocals - IN NORWAY

F2recip_CSurv_cytop_summary <- rbind (F2recip_anova_cytop_CSurv$Chisq[2], F2recip_anova_cytop_CSurv$P[2])
F2recip_Flow1_cytop_summary <- rbind (F2recip_anova_cytop_Flow1$Chisq[2], F2recip_anova_cytop_Flow1$P[2])
F2recip_Flow2_cytop_summary <- rbind (F2recip_anova_cytop_Flow2$Chisq[2], F2recip_anova_cytop_Flow2$P[2])
F2recip_Flow3_cytop_summary <- rbind (F2recip_anova_cytop_Flow3$Chisq[2], F2recip_anova_cytop_Flow3$P[2])
F2recip_Flow4_cytop_summary <- rbind (F2recip_anova_cytop_Flow4$Chisq[2], F2recip_anova_cytop_Flow4$P[2])
F2recip_TotSh1_cytop_summary <- rbind (F2recip_anova_cytop_TotSh1$Chisq[2], F2recip_anova_cytop_TotSh1$P[2])
F2recip_TotSh2_cytop_summary <- rbind (F2recip_anova_cytop_TotSh2$Chisq[2], F2recip_anova_cytop_TotSh2$P[2])
F2recip_TotSh3_cytop_summary <- rbind (F2recip_anova_cytop_TotSh3$Chisq[2], F2recip_anova_cytop_TotSh3$P[2])
F2recip_FrSh1_cytop_summary <- rbind (F2recip_anova_cytop_FrSh1$Chisq[2], F2recip_anova_cytop_FrSh1$P[2])
F2recip_FrSh2_cytop_summary <- rbind (F2recip_anova_cytop_FrSh2$Chisq[2], F2recip_anova_cytop_FrSh2$P[2])
F2recip_FrSh3_cytop_summary <- rbind (F2recip_anova_cytop_FrSh3$Chisq[2], F2recip_anova_cytop_FrSh3$P[2])
F2recip_SeFrG1_cytop_summary <- rbind (F2recip_anova_cytop_SeFrG1$Chisq[2], F2recip_anova_cytop_SeFrG1$P[2])
F2recip_SeFrG2_cytop_summary <- rbind (F2recip_anova_cytop_SeFrG2$Chisq[2], F2recip_anova_cytop_SeFrG2$P[2])
F2recip_SeFrG3_cytop_summary <- rbind (F2recip_anova_cytop_SeFrG3$Chisq[2], F2recip_anova_cytop_SeFrG3$P[2])
F2recip_FlStart1_cytop_summary <- rbind (F2recip_anova_cytop_FlStart1$Chisq[2], F2recip_anova_cytop_FlStart1$P[2])
F2recip_FlStart2_cytop_summary <- rbind (F2recip_anova_cytop_FlStart2$Chisq[2], F2recip_anova_cytop_FlStart2$P[2])
F2recip_FlStart3_cytop_summary <- rbind (F2recip_anova_cytop_FlStart3$Chisq[2], F2recip_anova_cytop_FlStart3$P[2])
F2recip_H1Sh1_cytop_summary <- rbind (F2recip_anova_cytop_H1Sh1$Chisq[2], F2recip_anova_cytop_H1Sh1$P[2])
F2recip_PsM12_cytop_summary <- rbind (F2recip_anova_cytop_PsM12$Chisq[2], F2recip_anova_cytop_PsM12$P[2])
F2recip_TotFl1_cytop_summary <- rbind (F2recip_anova_cytop_TotFl1$Chisq[2], F2recip_anova_cytop_TotFl1$P[2])

#COMBINING DATA FOR SUMMARY TABLES 1 AND 3

SpMaF1F2rownames <- rbind("Chisq", "P-value")

F1comparisons_colnames <- cbind(
"Trait",
"Nor_F1Recip_Surv", 	
"Nor_F1Recip_Flprob1", 		"Nor_F1Recip_Flprob2", 	"Nor_F1RecipF2_Flprob3",	"Nor_F1Recip_Flprob4",
"Nor_F1Recip_Infl1",		"Nor_F1Recip_Infl2",	"Nor_F1Recip_Infl3",
"Nor_F1Recip_Frinfl1",		"Nor_F1Recip_Frinfl2",	"Nor_F1Recip_Frinfl3",
"Nor_F1Recip_Seedsfr1",		"Nor_F1Recip_Seedsfr2",	"Nor_F1Recip_Seedsfr3",
"Nor_F1Recip_FlStart1",		"Nor_F1Recip_FlStart2",	"Nor_F1Recip_FlStart3",
"Nor_F1Recip_Infllength",	"Nor_F1Recip_Petalsize","Nor_F1Recip_Flowers")

F2comparisons_colnames <- cbind(
"Trait",
"Nor_F2Recip_Surv", 	
"Nor_F2Recip_Flprob1", 		"Nor_F2Recip_Flprob2", 	"Nor_F2Recip_Flprob3",	"Nor_F2Recip_Flprob4",
"Nor_F2Recip_Infl1",		"Nor_F2Recip_Infl2",	"Nor_F2Recip_Infl3",
"Nor_F2Recip_Frinfl1",		"Nor_F2Recip_Frinfl2",	"Nor_F2Recip_Frinfl3",
"Nor_F2Recip_Seedsfr1",		"Nor_F2Recip_Seedsfr2",	"Nor_F2Recip_Seedsfr3",
"Nor_F2Recip_FlStart1",		"Nor_F2Recip_FlStart2",	"Nor_F2Recip_FlStart3",
"Nor_F2Recip_Infllength",	"Nor_F2Recip_Petalsize","Nor_F2Recip_Flowers")


F1recip_cytop_summary <- cbind(
SpMaF1F2rownames,
F1recip_CSurv_cytop_summary, 
F1recip_Flow1_cytop_summary, 	F1recip_Flow2_cytop_summary, 	F1recip_Flow3_cytop_summary, F1recip_Flow4_cytop_summary,
F1recip_TotSh1_cytop_summary, 	F1recip_TotSh2_cytop_summary, 	F1recip_TotSh3_cytop_summary, 
F1recip_FrSh1_cytop_summary, 	F1recip_FrSh2_cytop_summary, 	F1recip_FrSh3_cytop_summary, 
F1recip_SeFrG1_cytop_summary, 	F1recip_SeFrG2_cytop_summary, 	F1recip_SeFrG3_cytop_summary, 
F1recip_FlStart1_cytop_summary, 	F1recip_FlStart2_cytop_summary, 	F1recip_FlStart3_cytop_summary, 
F1recip_H1Sh1_cytop_summary, 	
F1recip_PsM12_cytop_summary, 	
F1recip_TotFl1_cytop_summary)

F2recip_cytop_summary <- cbind(
SpMaF1F2rownames,
F2recip_CSurv_cytop_summary, 
F2recip_Flow1_cytop_summary, 		F2recip_Flow2_cytop_summary, 	F2recip_Flow3_cytop_summary, F2recip_Flow4_cytop_summary,
F2recip_TotSh1_cytop_summary, 	F2recip_TotSh2_cytop_summary, 	F2recip_TotSh3_cytop_summary, 
F2recip_FrSh1_cytop_summary, 		F2recip_FrSh2_cytop_summary, 	F2recip_FrSh3_cytop_summary, 
F2recip_SeFrG1_cytop_summary, 	F2recip_SeFrG2_cytop_summary, 	F2recip_SeFrG3_cytop_summary, 
F2recip_FlStart1_cytop_summary, 	F2recip_FlStart2_cytop_summary, 	F2recip_FlStart3_cytop_summary, 
F2recip_H1Sh1_cytop_summary, 	
F2recip_PsM12_cytop_summary, 	
F2recip_TotFl1_cytop_summary)


# RESULTS SHOWN IN TABLE 5

rbind (F1comparisons_colnames,F1recip_cytop_summary)
rbind (F2comparisons_colnames,F2recip_cytop_summary)


