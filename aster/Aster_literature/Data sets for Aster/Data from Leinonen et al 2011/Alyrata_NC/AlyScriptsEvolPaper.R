# load combined data file:
CombAnalyses <-read.table("Alyrata1105Data090808CombR.txt",
header=TRUE, sep="\t")


# SET UP NEW NAMES TO CHANGE ORDER OF FamFixed, SO FIGURES SHOW
  POPULATIONS IN CORRECT ORDER:

Newpop <- factor(CombAnalyses$FamFixed, levels = c("S","N","A","B"))
Newpop2 <- Newpop
levels(Newpop2)[1:4] <- c("S","N","F2S","F2N")

# MODEL WITH SEPARATE RECIPROCALS -- with revised order of populations:
# ** NOTE:  USED FOR ANALYSES IN PAPER, INCLUDING FIG 4**
StdSNF2a <- lmer (sqrt(S06SeedPerSil)  ~ Newpop2 + (1|Rep) + (1|Fam), REML=FALSE,
  subset=(Pop == "N" | Pop == "S" | Pop == "F2"),
   data=CombAnalyses)
summary(StdSNF2a)
vcov(StdSNF2a)
# MCMC to estimate confidence intervals:
StdSNF2amcmc <- mcmcsamp(StdSNF2a,n=1000)
StdSNF2aHPD <- HPDinterval(StdSNF2amcmc, prob=1-(1-pnorm(1))*2)
StdSNF2aSE <- as.data.frame(StdSNF2aHPD$fixef)

# Binary traits w/reciprocals separate -- reordered families:
# ** NOTE:  USED FOR ANALYSES IN PAPER, INCLUDING FIG 3**
MixedBinSNF2 <- lmer (S06Flowered ~ Newpop2 + (1|Rep) + (1|Fam),
  REML=FALSE, family=binomial,
  subset=(Pop == "N" | Pop == "S" | Pop == "F2"), data=CombAnalyses)
summary(MixedBinSNF2)


# TESTS USING REDUCED MODELS WITH COMBINED POPULATIONS:
#  These were used for analyses in Tables 1 and 3 of paper, with
#    combined reciprocals.
# NOTE:  These re-use generic object names -- change dependent variable
#   for different traits.
MixedFull <- lmer (sqrt(ShootHt306) ~ Pop + (1|Rep) + (1|Fam), REML=FALSE,
  subset=(Pop == "N" | Pop == "S" | Pop == "F2"), data=CombAnalyses)
summary(MixedFull)
MixedSN <- lmer (sqrt(ShootHt306) ~ PopSN + (1|Rep) + (1|Fam), REML=FALSE,
  subset=(Pop == "N" | Pop == "S" | Pop == "F2"), data=CombAnalyses)
summary(MixedSN)
MixedF2N <- lmer (sqrt(ShootHt306) ~ PopF2N + (1|Rep) + (1|Fam), REML=FALSE,
  subset=(Pop == "N" | Pop == "S" | Pop == "F2"), data=CombAnalyses)
summary(MixedF2N)
MixedF2S <- lmer (sqrt(ShootHt306) ~ PopF2S + (1|Rep) + (1|Fam), REML=FALSE,
  subset=(Pop == "N" | Pop == "S" | Pop == "F2"), data=CombAnalyses)
summary(MixedF2S)

anova(MixedFull,MixedSN) # test for significant N vs. S difference
anova(MixedFull,MixedF2N) # test for significant F2 vs. N difference
anova(MixedFull,MixedF2S) # test for significant F2 vs. S difference

# Version for binary traits:
MixedBinFull <- lmer (Surv1106 ~ Pop + (1|Rep) + (1|Fam),
  REML=FALSE, family=binomial,
  subset=(Pop == "N" | Pop == "S" | Pop == "F2"), data=CombAnalyses)
summary(MixedBinFull)
MixedBinSN <- lmer (Surv1106 ~ PopSN + (1|Rep) + (1|Fam),
  REML=FALSE, family=binomial,
  subset=(Pop == "N" | Pop == "S" | Pop == "F2"), data=CombAnalyses)
summary(MixedBinSN)
MixedBinF2N <- lmer (Surv1106 ~ PopF2N + (1|Rep) + (1|Fam),
  REML=FALSE, family=binomial,
  subset=(Pop == "N" | Pop == "S" | Pop == "F2"), data=CombAnalyses)
summary(MixedBinF2N)
MixedBinF2S <- lmer (Surv1106 ~ PopF2S + (1|Rep) + (1|Fam),
  REML=FALSE, family=binomial,
  subset=(Pop == "N" | Pop == "S" | Pop == "F2"), data=CombAnalyses)
summary(MixedBinF2S)

anova(MixedBinFull,MixedBinSN) # test for significant N vs. S difference
anova(MixedBinFull,MixedBinF2N) # test for significant F2 vs. N difference
anova(MixedBinFull,MixedBinF2S) # test for significant F2 vs. S difference


# PLOT MODEL OUTPUT (USED FOR FIGS 3B, 4B,D,F):

Estimate <- fixef(StdSNF2a)
for(i in 2:4) Estimate[i] <- Estimate[i]+Estimate[1]
Std.Error <- sqrt(diag(vcov(StdSNF2a)))
StdPlotObj <- data.frame(cbind(Estimate,Std.Error))
StdPlotObj$Pop <- levels(Newpop2)[1:4]
StdPlotObj$lower <- StdPlotObj$Estimate-StdPlotObj$Std.Error
StdPlotObj$upper <- StdPlotObj$Estimate+StdPlotObj$Std.Error

rm(Estimate,Std.Error)
ymax <- max(StdPlotObj$upper)

# If y values are not transformed:
with(StdPlotObj, barplot(Estimate, ylim=c(0,ymax),
  xlab="Population", ylab="Total Shoots",
  names.arg= c("Sp","Ma","SpMaF2","MaSpF2"),
  col=c("gray25","gray90","gray75","gray75")))
with(StdPlotObj, errbar(c(0.7,1.9,3.1,4.3),
  Estimate, upper, lower, cap=.08, add=TRUE))

# If y values are square-root transformed:
with(StdPlotObj, barplot(Estimate^2, ylim=c(0,ymax^2),
  xlab="Population", ylab="Average number of seeds per fruit",
  names.arg= c("Sp","Ma","SpMaF2","MaSpF2"),
  col=c("gray25","gray90","gray75","gray75")))
with(StdPlotObj, errbar(c(0.7,1.9,3.1,4.3), Estimate^2,
  upper^2, lower^2, cap=.08, add=TRUE))

# For binary traits:
Estimate <- fixef(MixedBinSNF2)
for(i in 2:4) Estimate[i] <- Estimate[i]+Estimate[1]
Std.Error <- sqrt(diag(vcov(MixedBinSNF2)))
StdPlotObj <- data.frame(cbind(Estimate,Std.Error))
StdPlotObj$Pop <- levels(Newpop2)[1:4]
StdPlotObj$lower <- StdPlotObj$Estimate-StdPlotObj$Std.Error
StdPlotObj$upper <- StdPlotObj$Estimate+StdPlotObj$Std.Error

rm(Estimate,Std.Error)
ymax <- max(StdPlotObj$upper)

with(StdPlotObj, barplot(exp(Estimate)/(exp(Estimate)+1), ylim=c(0,1),
  xlab="Population", ylab="Flowering Propensity",
  names.arg= c("Sp","Ma","SpMaF2","MaSpF2"),
  col=c("gray25","gray90","gray75","gray75")))
with(StdPlotObj, errbar(c(0.7,1.9,3.1,4.3), exp(Estimate)/(exp(Estimate)+1),
  exp(upper)/(exp(upper)+1), exp(lower)/(exp(lower)+1), cap=.08, add=TRUE))





