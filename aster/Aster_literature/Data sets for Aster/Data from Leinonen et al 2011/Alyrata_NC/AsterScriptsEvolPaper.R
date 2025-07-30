# load combined data file:
CombAnalyses <-read.table("Alyrata1105Data090808CombR.txt",
header=TRUE, sep="\t")

# Create data frame with just the needed columns:
AsterDataNC <- CombAnalyses[,c(1:11,14,15,16,17,22,23,25,28,29,37,43)]

# Make reproductive output an integer. NOTE:  not needed for normal dist!:
AsterDataNC$ReprodOutput <- with(AsterDataNC,round(ReprodOutput,digits = 0))
AsterDataNC$ReprodOutputZ <- with(AsterDataNC,round(ReprodOutputZ,digits = 0))
# AsterDataNC[1:5,]  # Check to make sure I have correct columns
# sapply(AsterDataNC,class)

# Create data frame with NC, Sp, and F2:
AsterDataNCPop <- with(AsterDataNC, AsterDataNC[Pop == "N" | Pop == "S"
  | Pop == "F2",])
AsterDataNCPop <- subset(AsterDataNCPop, !is.na(ReprodOutputZ))
# Remove records with seeds, no shoots or siliques:
AsterDataNCPop <- with(AsterDataNCPop, AsterDataNCPop[Final != 579,])
AsterDataNCPop <- with(AsterDataNCPop, AsterDataNCPop[Final != 140,])
# AsterDataNCPop[,c(1,3:5,8,13,14,16,17,19,21)] # Check to make sure I have the right rows.
levels(AsterDataNCPop$Pop)
levels(AsterDataNCPop$Rep)
# AsterDataNCPop[1:5,] # Check to make sure I have correct columns

# ---------------------------------------------------------------------------

VERSION WITH SURV, FLOWERED, REPRODUCTIVE OUTPUT:
# Create long form data frame:
vars <- c("Surv306","S06Flowered","ReprodOutputZ")
NCPopLong <- reshape(AsterDataNCPop, varying = list(vars), direction = "long",
  timevar = "varb", times = as.factor(vars), v.names = "resp")
NCPopLong$resp <- as.integer(NCPopLong$resp) # probably not necessary
NCPopLong[,c(1:5,18:20)] # Check format

# Digress to estimate reproductive output parameters
#  for trial inputs into aster.  (glm model should have similar structure
#  to aster model)  This uses the "wide" form of the data:
output1 <- glm(ReprodOutput ~ Pop + Rep, family=poisson, data=AsterDataNCPop)
summary(output1)
output1q <- glm(ReprodOutput ~ Pop + Rep, family=quasipoisson,
 data=AsterDataNCPop)
summary(output1q)
summ1q <- summary(output1q)
disp <- summ1q$dispersion
outmean <- mean(AsterDataNCPop$ReprodOutput, na.rm=TRUE)
# Since size parameter (alpha) in negative binomial = p/(1-p)E(k),
#  and E(disp)=V(k)/E(k)=1/p, then alpha can be estimated as follows:
alpha.out <- (1/disp)/(1-(1/disp))*outmean
# Now we can generate a list of probability distribution families:
famlist <- list(fam.bernoulli(), fam.truncated.negative.binomial
  (size = alpha.out, truncation = 0), fam.poisson(),
   fam.negative.binomial(size = alpha.out))

# Now we can set up the model:
#  First look at field names for both "wide" and "long" versions of data:
names(AsterDataNCPop)
names(NCPopLong)
#  Second, add root:
NCPopLong <- data.frame(NCPopLong, root=1)
#  Third, add the desired fitness measure as a pseudo-covariate:
ReprodOutputZ <- grep("ReprodOutputZ", as.character(NCPopLong$varb))
ReprodOutputZ <- is.element(seq(along = NCPopLong$varb), ReprodOutputZ)
NCPopLong <- data.frame(NCPopLong, ReprodOutputZ = as.integer(ReprodOutputZ))
#  Fourth, establish pred and families vectors:
vars  # as a reminder of the order
pred <- c(0, 1, 2)
fam <- c(1, 1, 4)
#  Check to see that the right families are specified:
sapply(famlist, as.character)[fam]

#  And, at last, the "reduced" model (NOTE:  giving errors so far):
RepOut1 <- aster(resp ~ varb + Rep, pred, fam, varb, id, root,
  data=NCPopLong, famlist=famlist)
summary(RepOut1, show.graph = TRUE)

#  Model with Pop effects on reproductive output (Also giving errors):
RepOut2 <- aster(resp ~ varb + Rep + Pop*ReprodOutputZ - Pop,
  pred, fam, varb, id, root, data=NCPopLong, famlist=famlist)
summary(RepOut2, show.graph = TRUE)

# ---------------------------------------------------------------------------

# ABOVE FOR VERSION WITHOUT SURVIVAL:
# NOTE:  Used this for zero-truncated neg. binomial analyses in first version
#  of Evolution paper!!

# Add fields for alternate population groupings:
AsterDataNCPop$PopSN <- AsterDataNCPop$Pop
levels(AsterDataNCPop$PopSN)[c(4,6)] <- "SandN"
AsterDataNCPop$PopF2N <- AsterDataNCPop$Pop
levels(AsterDataNCPop$PopF2N)[c(2,6)] <- "F2andN"

# Optional:  check format:
# AsterDataNCPop[,c(1:6)]

# Create long form data frame without survival:
vars <- c("S06Flowered","ReprodOutputZ")
NCPopLong2 <- reshape(AsterDataNCPop, varying = list(vars), direction = "long",
  timevar = "varb", times = as.factor(vars), v.names = "resp")
# NCPopLong2$resp <- as.integer(NCPopLong2$resp) # probably not necessary
# NCPopLong2[,c(1:5,18:20)] # Check format

# Estimate reproductive output parameters
#  for trial inputs into aster.  (glm model should have similar structure
#  to aster model)  This uses the "wide" form of the data:
# NOTE:  Only do this for initial versions, then use the ML estimate
#  routine down below here somewhere to get alpha.out!!!
output1q <- glm(ReprodOutput ~ Pop + Rep, family=quasipoisson,
 data=AsterDataNCPop)
summary(output1q)

# Test to see if null model gives correct var/mean ratio:
#  Dispersion parameter ~= var/mean ratio for all data,
#   so this procedure should work correctly.
#  Don't do this test normally!!
output1qnull <- glm(ReprodOutput ~ 1, family=quasipoisson,
 data=AsterDataNCPop)
summary(output1qnull)
anova(output1qnull,output1q)

summ1q <- summary(output1q)
disp <- summ1q$dispersion
outmean <- mean(AsterDataNCPop$ReprodOutput, na.rm=TRUE)
# Since negative binomial size parameter (alpha) = p/(1-p)E(k),
#  and E(disp)=V(k)/E(k)=1/p, then alpha can be estimated as follows:
alpha.out <- (1/disp)/(1-(1/disp))*outmean

# Now we can generate a list of probability distribution families:
famlist <- list(fam.bernoulli(), fam.truncated.negative.binomial
  (size = alpha.out, truncation = 0))

#  First look at field names for both "wide" and "long" versions of data:
names(AsterDataNCPop)
names(NCPopLong2)
#  Add root:
NCPopLong2 <- data.frame(NCPopLong2, root=1)
#  Add the desired fitness measure as a pseudo-covariate:
ReprodOutputZ <- grep("ReprodOutputZ", as.character(NCPopLong2$varb))
ReprodOutputZ <- is.element(seq(along = NCPopLong2$varb), ReprodOutputZ)
NCPopLong2 <- data.frame(NCPopLong2, ReprodOutputZ = as.integer(ReprodOutputZ))
#  Establish pred and families vectors:
vars  # as a reminder of the order
pred <- c(0, 1)
fam <- c(1, 2)
#  Check to see that the right families are specified:
sapply(famlist, as.character)[fam]

#  And, at last, the "reduced" model:
RepOut2.1 <- aster(resp ~ varb + Rep, pred, fam, varb, id, root,
  data=NCPopLong2, famlist=famlist)
feig <- eigen(RepOut2.1$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(RepOut2.1, show.graph = TRUE, info.tol=1e-9)

#  Model with Pop effects on reproductive output:
RepOut2.2 <- aster(resp ~ varb + Rep + Pop*ReprodOutputZ - Pop,
  pred, fam, varb, id, root, data=NCPopLong2, famlist=famlist)
feig <- eigen(RepOut2.2$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(RepOut2.2, show.graph = TRUE, info.tol=1e-10)

#  Model with separate F2 reciprocals:
RepOut2.3 <- aster(resp ~ varb + Rep + FamFixed*ReprodOutputZ - FamFixed,
  pred, fam, varb, id, root, data=NCPopLong2, famlist=famlist)
feig <- eigen(RepOut2.3$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(RepOut2.3, show.graph = TRUE, info.tol=1e-10)

#  Model with S and N combined:
RepOut2.4 <- aster(resp ~ varb + Rep + PopSN*ReprodOutputZ - PopSN,
  pred, fam, varb, id, root, data=NCPopLong2, famlist=famlist)
feig <- eigen(RepOut2.4$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(RepOut2.4, show.graph = TRUE, info.tol=1e-9)

#  Model with F2 and N combined:
RepOut2.5 <- aster(resp ~ varb + Rep + PopF2N*ReprodOutputZ - PopF2N,
  pred, fam, varb, id, root, data=NCPopLong2, famlist=famlist)
feig <- eigen(RepOut2.5$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(RepOut2.5, show.graph = TRUE, info.tol=1e-10)

anova(RepOut2.1, RepOut2.2) # Test overall Pop effect.
anova(RepOut2.2, RepOut2.3) # Test reciprocal effect.
anova(RepOut2.4, RepOut2.2) # Test N vs. S.
anova(RepOut2.5, RepOut2.2) # Test N vs. F2.

# ---------------------------------------------------------------------------

# VERSION FOR TRAIT REGRESSIONS ON FITNESS:
# NOTE:  Similar to previous model, but needs zeroes to replace NA
#  for phenotypic traits.
#  Uses zero-truncated negative binomial distribution.
#  The following does analyses separately by population:

# Optional:  check format:
# AsterDataNCPop[,c(1:6)]

# Create adjusted flowering date, earliest=0:
FlowerDateMin <- with(AsterDataNCPop,min(FlowerDate,na.rm=TRUE))
AsterDataNCPop$FlowerDateAdjNZ <- with(AsterDataNCPop,
 (FlowerDate-FlowerDateMin))
rm("FlowerDateMin")

# Change FlowerDateAdjNZ and Diam1_306 NA values to zeroes:
AsterDataNCPop$FlowerDateAdj <- replace(AsterDataNCPop$FlowerDateAdjNZ,
  is.na(AsterDataNCPop$FlowerDateAdjNZ), 0)
AsterDataNCPop$Diam1_306 <- replace(AsterDataNCPop$Diam1_306,
  is.na(AsterDataNCPop$Diam1_306), 0)


# Create long form data frame from scratch:
vars <- c("S06Flowered","ReprodOutputZ")
NCPopLongR <- reshape(AsterDataNCPop, varying = list(vars), direction = "long",
  timevar = "varb", times = as.factor(vars), v.names = "resp")
names(NCPopLongR) # Check field names, then modify next statements if needed
# OPTIONAL tests to check formatting:
# NCPopLongR[c(1:5),c(1:5,10,12,24,25)]
# NCPopLongR[c(1:5),]
# NCPopLongR[,c(1,3,4,25,28:31)]

#  Add root:
NCPopLongR <- data.frame(NCPopLongR, root=1)
#  Add the desired fitness measures as pseudo-covariates:
ReprodOutputZ <- grep("ReprodOutputZ", as.character(NCPopLongR$varb))
ReprodOutputZ <- is.element(seq(along = NCPopLongR$varb), ReprodOutputZ)
NCPopLongR <- data.frame(NCPopLongR, ReprodOutputZ = as.integer(ReprodOutputZ))

# Make indicator for ReprodOutputZ, and create conditional fields
#  for FlowerDateAdj, Diam1_306, and ShootHt306:
# First remove record with ReprodOutput, no FlowerDate:
NCPopLongR <- with(NCPopLongR, NCPopLongR[Final != 764,])
ReOutInd <- as.numeric(as.character(NCPopLongR$varb)=="ReprodOutputZ")
NCPopLongR$FlowerDateAdjRO <- NCPopLongR$FlowerDateAdj*ReOutInd
NCPopLongR$Diam1_306RO <- NCPopLongR$Diam1_306*ReOutInd
NCPopLongR$ShootHt306RO <- NCPopLongR$ShootHt306*ReOutInd

# Create long form versions for each population separately:
NCPopLongRN <- with(NCPopLongR, NCPopLongR[Pop == "N",])
NCPopLongRS <- with(NCPopLongR, NCPopLongR[Pop == "S",])
NCPopLongRF2 <- with(NCPopLongR, NCPopLongR[Pop == "F2",])
# names(NCPopLongRF2) # Check field names, modify next statement if needed
# NCPopLongRF2[,c(1:5,24,25)] # Check for correct records

# Estimate reproductive output parameters (IMPORTANT:  Preliminary only!!)
#  for trial inputs into aster.  (glm model should have similar structure
#  to aster model)  This uses the "wide" form of the data:
outputRN <- glm(ReprodOutput ~ Rep, family=quasipoisson,
 data=AsterDataNCPop, subset=(Pop == "N"))
summary(outputRN)
summRN <- summary(outputRN)
dispRN <- summRN$dispersion
outmeanRN <- with(AsterDataNCPop,
 mean(AsterDataNCPop$ReprodOutput[Pop == "N"], na.rm=TRUE))
# Since negative binomial size parameter (alpha) = p/(1-p)E(k),
#  and E(disp)=V(k)/E(k)=1/p, then alpha can be estimated as follows:
alpha.outRN <- (1/dispRN)/(1-(1/dispRN))*outmeanRN

outputRS <- glm(ReprodOutput ~ Rep, family=quasipoisson,
 data=AsterDataNCPop, subset=(Pop == "S"))
summary(outputRS)
summRS <- summary(outputRS)
dispRS <- summRS$dispersion
outmeanRS <- with(AsterDataNCPop,
 mean(AsterDataNCPop$ReprodOutput[Pop == "S"], na.rm=TRUE))
# Since negative binomial size parameter (alpha) = p/(1-p)E(k),
#  and E(disp)=V(k)/E(k)=1/p, then alpha can be estimated as follows:
alpha.outRS <- (1/dispRS)/(1-(1/dispRS))*outmeanRS

outputRF2 <- glm(ReprodOutput ~ Rep, family=quasipoisson,
 data=AsterDataNCPop, subset=(Pop == "F2"))
summary(outputRF2)
summRF2 <- summary(outputRF2)
dispRF2 <- summRF2$dispersion
outmeanRF2 <- with(AsterDataNCPop,
 mean(AsterDataNCPop$ReprodOutput[Pop == "F2"], na.rm=TRUE))
# Since negative binomial size parameter (alpha) = p/(1-p)E(k),
#  and E(disp)=V(k)/E(k)=1/p, then alpha can be estimated as follows:
alpha.outRF2 <- (1/dispRF2)/(1-(1/dispRF2))*outmeanRF2

# Generate list of probability distribution families:
famlist <- list(fam.bernoulli(), fam.truncated.negative.binomial
  (size = alpha.outRN, truncation = 0), fam.truncated.negative.binomial
  (size = alpha.outRS, truncation = 0), fam.truncated.negative.binomial
  (size = alpha.outRF2, truncation = 0))

#  Establish pred and families vectors:
vars  # as a reminder of the order
pred <- c(0, 1)
famN <- c(1, 2) # For NC regressions
famS <- c(1, 3) # For Spiterstulen regressions
famF2 <- c(1, 4) # For F2 regressions
#  Check to see that the right families are specified:
sapply(famlist, as.character)[famN]
sapply(famlist, as.character)[famS]
sapply(famlist, as.character)[famF2]

# ASTER ANALYSES FOR N:

# Reduced model: (NOTE - negative information eigenvectors)
NRed <- aster(resp ~ varb + Rep, pred, famN, varb, id, root,
  data=NCPopLongRN, famlist=famlist)
feig <- eigen(NRed$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(NRed, show.graph = TRUE, info.tol=1e-19)

# Full linear model:  (NOTE - computationally singular)
NFullL <- aster(resp ~ varb + Rep + sqrt(FlowerDateAdjRO)
 + Diam1_306RO + ShootHt306RO,
  pred, famN, varb, id, root, data=NCPopLongRN, famlist=famlist)
feig <- eigen(NFullL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(NFullL, show.graph = TRUE, info.tol=1e-18)

# Linear model with FlowerDateAdj and Diam1_306 only: (Negative eigenvalues)
NFDL <- aster(resp ~ varb + Rep + sqrt(FlowerDateAdjRO)
 + Diam1_306RO,
  pred, famN, varb, id, root, data=NCPopLongRN, famlist=famlist)
feig <- eigen(NFDL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(NFDL, show.graph = TRUE, info.tol=1e-18)

# Linear model with Diam1_306 and ShootHt only: (Negative eigenvalues)
NDSL <- aster(resp ~ varb + Rep + Diam1_306RO + ShootHt306RO,
  pred, famN, varb, id, root, data=NCPopLongRN, famlist=famlist)
feig <- eigen(NDSL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(NDSL, show.graph = TRUE, info.tol=1e-18)

# Linear model with FlowerDateAdj and ShootHt only: (Negative eigenvalues)
NFSL <- aster(resp ~ varb + Rep + sqrt(FlowerDateAdjRO) + ShootHt306RO,
  pred, famN, varb, id, root, data=NCPopLongRN, famlist=famlist)
feig <- eigen(NFSL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(NFSL, show.graph = TRUE, info.tol=1e-18)


# ASTER ANALYSES FOR S:

# Reduced model:
SRed <- aster(resp ~ varb + Rep, pred, famS, varb, id, root,
  data=NCPopLongRS, famlist=famlist)
feig <- eigen(SRed$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(SRed, show.graph = TRUE)
# SRed$deviance # Optional only

# Full linear model:)
SFullL <- aster(resp ~ varb + Rep + sqrt(FlowerDateAdjRO)
 + Diam1_306RO + ShootHt306RO,
  pred, famS, varb, id, root, data=NCPopLongRS, famlist=famlist)
feig <- eigen(SFullL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(SFullL, show.graph = TRUE, info.tol=1e-10)

anova(SRed,SFullL)

# Linear model with FlowerDateAdj and Diam1_306 only:
SFDL <- aster(resp ~ varb + Rep + sqrt(FlowerDateAdjRO) + Diam1_306RO,
  pred, famS, varb, id, root, data=NCPopLongRS, famlist=famlist)
feig <- eigen(SFDL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(SFDL, show.graph = TRUE, info.tol=1e-10)

anova(SFDL,SFullL)

# Linear model with FlowerDateAdj and ShootHt306 only:
SFSL <- aster(resp ~ varb + Rep + sqrt(FlowerDateAdjRO) + ShootHt306RO,
  pred, famS, varb, id, root, data=NCPopLongRS, famlist=famlist)
feig <- eigen(SFSL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(SFSL, show.graph = TRUE, info.tol=1e-9)

anova(SFSL,SFullL)

# Linear model with Diam1_306 and ShootHt306 only:
SDSL <- aster(resp ~ varb + Rep + Diam1_306RO + ShootHt306RO,
  pred, famS, varb, id, root, data=NCPopLongRS, famlist=famlist)
feig <- eigen(SDSL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(SDSL, show.graph = TRUE, info.tol=1e-10)

anova(SDSL,SFullL)

# Linear model with ShootHt306 only:
SSL <- aster(resp ~ varb + Rep + ShootHt306RO,
  pred, famS, varb, id, root, data=NCPopLongRS, famlist=famlist)
feig <- eigen(SSL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(SSL, show.graph = TRUE, info.tol=1e-9)

anova(SSL,SFullL)
anova(SSL,SDSL,SFullL)
anova(SRed,SSL,SDSL,SFullL)
anova(SRed,SSL,SDSL,SFullL)
anova(SRed,SSL,SFSL,SFullL) # Best set given results from above.

# Quadratic FS model:  
SFSQ <- aster(resp ~ varb + Rep + sqrt(FlowerDateAdjRO) + ShootHt306RO
  + I(sqrt(FlowerDateAdjRO)^2) + I(ShootHt306RO^2)
  + I(2 * sqrt(FlowerDateAdjRO) * ShootHt306RO),
  pred, famS, varb, id, root, data=NCPopLongRS, famlist=famlist)
feig <- eigen(SFSQ$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(SFSQ, show.graph = TRUE, info.tol=1e-12)

anova(SFSL, SFSQ)

# Quadratic DS model:
SDSQ <- aster(resp ~ varb + Rep + Diam1_306RO + ShootHt306RO
  + I(Diam1_306RO^2) + I(ShootHt306RO^2) + I(2 * Diam1_306RO * ShootHt306RO),
  pred, famS, varb, id, root, data=NCPopLongRS, famlist=famlist)
feig <- eigen(SDSQ$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(SDSQ, show.graph = TRUE, info.tol=1e-13)

anova(SDSL, SDSQ)

# Quadratic S model:
SSQ <- aster(resp ~ varb + Rep + ShootHt306RO + I(ShootHt306RO^2),
  pred, famS, varb, id, root, data=NCPopLongRS, famlist=famlist)
feig <- eigen(SSQ$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(SSQ, show.graph = TRUE, info.tol=1e-12)

anova(SSL, SSQ)

# PLOT RESULTS OF QUADRATIC DS MODEL:
#  NOTE:  Change to quadratic FS model by substituting traits below:
# First create wide form of data:
NCPopWideRS <- with(NCPopLongRS, reshape(NCPopLongRS, direction = "wide",
  timevar = "varb",
  v.names = "resp", varying = list(levels(NCPopLongRS$varb))))
names(NCPopWideRS)
NCPopWideRS[c(1:5),]

# Make A matrix:
a1 <- SFSQ$coefficients["sqrt(FlowerDateAdjRO)"]
a2 <- SFSQ$coefficients["ShootHt306RO"]
a <- c(a1, a2)
A11 <- SFSQ$coefficients["I(sqrt(FlowerDateAdjRO)^2)"]
A22 <- SFSQ$coefficients["I(ShootHt306RO^2)"]
A12 <- SFSQ$coefficients["I(2 * sqrt(FlowerDateAdjRO) * ShootHt306RO)"]
A <- matrix(c(A11, A12, A12, A22), 2, 2)

# Test shape:
eigen(A, symmetric = TRUE, only.values = TRUE)$values

# Find maxima, if possible:  NOTE: First Eigenvalue of A is positive,
#  so this might be a minimum
maxSFSQ <- (-solve(A, a)/2)
print(maxSFSQ)

# Generate graph:
#  NOTE:  Use original fields, as RO fields are corrupted in conversion
plot(sqrt(NCPopWideRS$FlowerDateAdj), NCPopWideRS$ShootHt306,
 xlab = "sqrt(FlowerDateAdj)", ylab = "ShootHt306")
ufoo <- par("usr")
nx <- 101
ny <- 101
z <- matrix(NA, nx, ny)
x <- seq(ufoo[1], ufoo[2], length = nx)
y <- seq(ufoo[3], ufoo[4], length = ny)
points(maxSFSQ[1], maxSFSQ[2], pch = 19)
for (i in 1:nx) {
for (j in 1:ny) {
b <- c(x[i], y[j])
z[i, j] <- sum(a * b) + as.numeric(t(b) %*% A %*%
b)
}
}
b <- as.numeric(maxSFSQ)
contour(x, y, z, add = TRUE)
contour(x, y, z, levels = c(0.325), add = TRUE)

# ASTER ANALYSES FOR F2:

# Reduced model:
F2Red <- aster(resp ~ varb + Rep, pred, famF2, varb, id, root,
  data=NCPopLongRF2, famlist=famlist)
feig <- eigen(F2Red$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(F2Red, show.graph = TRUE, info.tol=1e-10)

# Full linear model:
F2FullL <- aster(resp ~ varb + Rep + sqrt(FlowerDateAdjRO)
  + Diam1_306RO + ShootHt306RO,
  pred, famF2, varb, id, root, data=NCPopLongRF2, famlist=famlist)
feig <- eigen(F2FullL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(F2FullL, show.graph = TRUE, info.tol=1e-14)

anova(F2Red,F2FullL)

# Linear model with FlowerDateAdj and Diam1_306 only:
F2FDL <- aster(resp ~ varb + Rep + sqrt(FlowerDateAdjRO) + Diam1_306RO,
  pred, famF2, varb, id, root, data=NCPopLongRF2, famlist=famlist)
feig <- eigen(F2FDL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(F2FDL, show.graph = TRUE, info.tol=1e-14)

anova(F2FDL,F2FullL)

# Linear model with FlowerDate and ShootHt306 only:
F2FSL <- aster(resp ~ varb + Rep + sqrt(FlowerDateAdjRO) + ShootHt306RO,
  pred, famF2, varb, id, root, data=NCPopLongRF2, famlist=famlist)
feig <- eigen(F2FSL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(F2FSL, show.graph = TRUE, info.tol=1e-14)

anova(F2FSL,F2FullL)

# Linear model with Diam1_306 and ShootHt306 only:
F2DSL <- aster(resp ~ varb + Rep + Diam1_306RO + ShootHt306RO,
  pred, famF2, varb, id, root, data=NCPopLongRF2, famlist=famlist)
feig <- eigen(F2DSL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(F2DSL, show.graph = TRUE, info.tol=1e-14)

anova(F2DSL,F2FullL)

# Linear model with Diam1_306 only:
F2DL <- aster(resp ~ varb + Rep + Diam1_306RO,
  pred, famF2, varb, id, root, data=NCPopLongRF2, famlist=famlist)
feig <- eigen(F2DL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(F2DL, show.graph = TRUE, info.tol=1e-14)

anova(F2Red,F2DL,F2DSL,F2FullL)
anova(F2Red,F2DL,F2FDL,F2FullL)

# Full Quadratic model:  (NOTE:  Does not converge)
F2FullQ <- aster(resp ~ varb + Rep + Diam1_306RO + ShootHt306RO
  + sqrt(FlowerDateAdjRO) + I(sqrt(FlowerDateAdjRO^2))
  + I(Diam1_306RO^2) + I(ShootHt306RO^2) + I(2 * Diam1_306RO * ShootHt306RO)
  + I(2 * sqrt(FlowerDateAdjRO) * Diam1_306RO)
  + I(2 * sqrt(FlowerDateAdjRO) * ShootHt306RO),
  pred, famF2, varb, id, root, data=NCPopLongRF2, famlist=famlist,
  maxiter=8000)
feig <- eigen(F2FullQ$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(F2FullQ, show.graph = TRUE, info.tol=1e-18)

anova(F2FullL, F2FullQ)

# Quadratic DS model:  (NOTE:  Does not converge)
F2DSQ <- aster(resp ~ varb + Rep + Diam1_306RO + ShootHt306RO
  + I(Diam1_306RO^2) + I(ShootHt306RO^2) + I(2 * Diam1_306RO * ShootHt306RO),
  pred, famF2, varb, id, root, data=NCPopLongRF2, famlist=famlist,
  method="trust", maxiter=8000)
feig <- eigen(F2DSQ$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(F2DSQ, show.graph = TRUE, info.tol=1e-18)

anova(F2DSL, F2DSQ)

# Quadratic FS model:  (NOTE:  Computationally singular)
F2FSQ <- aster(resp ~ varb + Rep + sqrt(FlowerDateAdjRO) + ShootHt306RO
  + I(sqrt(FlowerDateAdjRO)^2) + I(ShootHt306RO^2)
  + I(2 * sqrt(FlowerDateAdjRO) * ShootHt306RO),
  pred, famF2, varb, id, root, data=NCPopLongRF2, famlist=famlist,
  maxiter=8000)
feig <- eigen(F2FSQ$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(F2FSQ, show.graph = TRUE, info.tol=1e-17)

anova(F2FSL, F2FSQ)

# Quadratic D model:  (NOTE:  Does not converge)
F2DQ <- aster(resp ~ varb + Rep + Diam1_306RO + I(Diam1_306RO^2),
  pred, famF2, varb, id, root, data=NCPopLongRF2, famlist=famlist,
  maxiter=8000)
feig <- eigen(F2DQ$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(F2DQ, show.graph = TRUE, info.tol=1e-14)

anova(F2DL, F2DQ)

# PLOT RESULTS OF LINEAR DS MODEL:
# First create wide form of data:
NCPopWideRF2 <- with(NCPopLongRF2, reshape(NCPopLongRF2, direction = "wide",
  timevar = "varb",
  v.names = "resp", varying = list(levels(NCPopLongRF2$varb))))
names(NCPopWideRF2)
NCPopWideRF2[c(1:5),]

# Make a and A matrix:
a1 <- F2DSL$coefficients["Diam1_306RO"]
a2 <- F2DSL$coefficients["ShootHt306RO"]
a <- c(a1, a2)
# A11 <- F2DSL$coefficients["I(Diam1_306RO^2)"]
# A22 <- F2DSL$coefficients["I(ShootHt306RO^2)"]
# A12 <- F2DSL$coefficients["I(2 * Diam1_306RO * ShootHt306RO)"]
# A <- matrix(c(A11, A12, A12, A22), 2, 2)

# Test shape:
# eigen(A, symmetric = TRUE, only.values = TRUE)$values

# Find maxima, if possible:  NOTE: Eigen of A is positive,
#  so this might be a minimum
# maxF2DSL <- (-solve(A, a)/2)
# print(maxF2DSL)

# Generate graph:
#  NOTE:  Use original fields, as RO fields are corrupted in conversion
plot(NCPopWideRF2$Diam1_306, NCPopWideRF2$ShootHt306, xlab = "Diam",
  ylab = "ShootHt")
ufoo <- par("usr")
nx <- 101
ny <- 101
z <- matrix(NA, nx, ny)
x <- seq(ufoo[1], ufoo[2], length = nx)
y <- seq(ufoo[3], ufoo[4], length = ny)
# points(maxF2DSL[1], maxF2DSL[2], pch = 19)
for (i in 1:nx) {
for (j in 1:ny) {
b <- c(x[i], y[j])
z[i, j] <- sum(a * b)
}
# b <- as.numeric(maxF2DSL)
contour(x, y, z, add = TRUE)
# contour(x, y, z, levels = c(0.325), add = TRUE)
}

# ---------------------------------------------------------------------------

# GENERATE ML ESTIMATE OF SIZE PARAMETER FOR ReprodOutputZ:
#  This is based on p. 43-45 of Shaw et al. Technical Report #658.

# Define zero-truncated negative binomial distribution:
#  NOTE:  Use an aster run with the same structure as the full model
#    to be tested,using a quasi-Poisson estimated size parameter as a
#    preliminary starting point.  The first line below and subsequent
#    steps should use this run object as its input.
# IMPORTANT:  This is generic -- replace with correct model and output
#    file names before running
x <- F2FullL$x  # Makes data frame with trait(s) in the aster model.
logl <- function(alpha.outRF2, theta, x) {
  x.flowered <- x[,1]
  theta.flowered <- theta[,1]
  p.flowered <- 1/(1 + exp(-theta.flowered))
  logl.flowered <- sum(dbinom(x.flowered, 1, p.flowered, log = TRUE))
  x.output <- x[x.flowered == 1, 2]
  theta.output <- theta[x.flowered == 1, 2]
  p.output <- (-expm1(theta.output ))
  logl.output <- sum(dnbinom(x.output , size = alpha.outRF2,
    prob = p.output, log = TRUE) - pnbinom(0, size = alpha.outRF2,
    prob = p.output, lower.tail = FALSE, log = TRUE))
  logl.flowered + logl.output
}
  
#  Evaluate likelihood profile over a grid of size values:
alpha.outRF2.seq <- seq(0.25, 4.5, 0.25) # Wider range than used in tr658.
logl.seq <- double(length(alpha.outRF2.seq))
for (i in 1:length(alpha.outRF2.seq)) {
  famlist.seq <- famlist
  famlist.seq[[2]] <- fam.truncated.negative.binomial(size = alpha.outRF2.seq[i],
    truncation = 0)
  F2FullL.seq <- aster(F2FullL$formula, pred, famF2, varb, id, root,
    data=NCPopLongRF2, famlist = famlist.seq)
  theta.seq <- predict(F2FullL.seq, model.type = "cond",
    parm.type = "canon")
  dim(theta.seq) <- dim(x)
  logl.seq[i] <- logl(alpha.outRF2.seq[i], theta.seq, x)
}
alpha.foo <- seq(min(alpha.outRF2.seq), max(alpha.outRF2.seq), 0.01)
logl.foo <- spline(alpha.outRF2.seq, logl.seq, n = length(alpha.foo))$y
imax <- seq(along = alpha.foo)[logl.foo == max(logl.foo)]
alpha.outRF2<- alpha.foo[imax]

# ---------------------------------------------------------------------------

# FITNESS REGRESSION MODEL WITH ALL POPULATIONS:
#  Need this to generate "correct" estimates of negative binomial size params.
# Create adjusted flowering date, earliest=0:
FlowerDateMin <- with(AsterDataNCPop,min(FlowerDate,na.rm=TRUE))
AsterDataNCPop$FlowerDateAdjNZ <- with(AsterDataNCPop,
 (FlowerDate-FlowerDateMin))
rm("FlowerDateMin")

# Change FlowerDateAdjNZ and Diam1_306 NA values to zeroes:
AsterDataNCPop$FlowerDateAdj <- replace(AsterDataNCPop$FlowerDateAdjNZ,
  is.na(AsterDataNCPop$FlowerDateAdjNZ), 0)
AsterDataNCPop$Diam1_306 <- replace(AsterDataNCPop$Diam1_306,
  is.na(AsterDataNCPop$Diam1_306), 0)

# Create long form data frame from scratch:
vars <- c("S06Flowered","ReprodOutputZ")
NCPopLongR <- reshape(AsterDataNCPop, varying = list(vars), direction = "long",
  timevar = "varb", times = as.factor(vars), v.names = "resp")
names(NCPopLongR) # Check field names, then modify next statements if needed
# OPTIONAL tests to check formatting:
# NCPopLongR[c(1:5),c(1:5,10,12,24,25)]
# NCPopLongR[c(1:5),]
# NCPopLongR[,c(1,3,4,25,28:31)]

#  Add root:
NCPopLongR <- data.frame(NCPopLongR, root=1)
#  Add the desired fitness measures as pseudo-covariates:
ReprodOutputZ <- grep("ReprodOutputZ", as.character(NCPopLongR$varb))
ReprodOutputZ <- is.element(seq(along = NCPopLongR$varb), ReprodOutputZ)
NCPopLongR <- data.frame(NCPopLongR, ReprodOutputZ = as.integer(ReprodOutputZ))

# Make indicator for ReprodOutputZ, and create conditional fields
#  for FlowerDateAdj, Diam1_306, and ShootHt306:
# First remove record with ReprodOutput, no FlowerDate:
NCPopLongR <- with(NCPopLongR, NCPopLongR[Final != 764,])
ReOutInd <- as.numeric(as.character(NCPopLongR$varb)=="ReprodOutputZ")
NCPopLongR$FlowerDateAdjRO <- NCPopLongR$FlowerDateAdj*ReOutInd
NCPopLongR$Diam1_306RO <- NCPopLongR$Diam1_306*ReOutInd
NCPopLongR$ShootHt306RO <- NCPopLongR$ShootHt306*ReOutInd

# Estimate reproductive output parameters for (temporary, I guess)
#  trial inputs into aster.  (glm model should have similar structure
#  to aster model)  This uses the "wide" form of the data:
outputR <- glm(ReprodOutput ~ Pop + Rep, family=quasipoisson,
 data=AsterDataNCPop)
summary(outputR)
summR <- summary(outputR)
dispR <- summR$dispersion
outmeanR <- with(AsterDataNCPop,
 mean(AsterDataNCPop$ReprodOutput, na.rm=TRUE))
# Since negative binomial size parameter (alpha) = p/(1-p)E(k),
#  and E(disp)=V(k)/E(k)=1/p, then alpha can be estimated as follows:
alpha.outR <- (1/dispR)/(1-(1/dispR))*outmeanR

# Generate list of probability distribution families:
famlist <- list(fam.bernoulli(), fam.truncated.negative.binomial
  (size = alpha.outR, truncation = 0))

#  Establish pred and families vectors:
vars  # as a reminder of the order
pred <- c(0, 1)
fam <- c(1, 2) # For NC regressions
#  Check to see that the right families are specified:
sapply(famlist, as.character)[fam]

# ASTER ANALYSES FOR COMBINED POPULATIONS:

# Reduced model:
CRed <- aster(resp ~ varb + Rep, pred, fam, varb, id, root,
  data=NCPopLongR, famlist=famlist)
feig <- eigen(CRed$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(CRed, show.graph = TRUE, info.tol=1e-9)

# Full linear model:
CFullL <- aster(resp ~ varb + Rep + Pop*ReprodOutputZ - Pop
  + sqrt(FlowerDateAdjRO) + Diam1_306RO + ShootHt306RO,
  pred, fam, varb, id, root, data=NCPopLongR, famlist=famlist)
feig <- eigen(CFullL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(CFullL, show.graph = TRUE, info.tol=1e-15)

anova(CRed,CFullL)

# Model with population effects but no additional traits:
CPopL <- aster(resp ~ varb + Rep + Pop*ReprodOutputZ - Pop,
  pred, fam, varb, id, root, data=NCPopLongR, famlist=famlist)
feig <- eigen(CPopL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(CPopL, show.graph = TRUE, info.tol=1e-10)

anova(CRed,CPopL)
anova(CRed,CPopL,CFullL)

# Linear model with FlowerDateAdj and Diam1_306 only:
CFDL <- aster(resp ~ varb + Rep + Pop*ReprodOutputZ - Pop
  + sqrt(FlowerDateAdjRO) + Diam1_306RO,
  pred, fam, varb, id, root, data=NCPopLongR, famlist=famlist)
feig <- eigen(CFDL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(CFDL, show.graph = TRUE, info.tol=1e-15)

anova(CFDL,CFullL)

# Linear model with FlowerDateAdj and ShootHt306 only:
CFSL <- aster(resp ~ varb + Rep + Pop*ReprodOutputZ - Pop
  + sqrt(FlowerDateAdjRO) + ShootHt306RO,
  pred, fam, varb, id, root, data=NCPopLongR, famlist=famlist)
feig <- eigen(CFSL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(CFSL, show.graph = TRUE, info.tol=1e-14)

anova(CFSL,CFullL)

# Linear model with Diam1_306 and ShootHt306 only:
CDSL <- aster(resp ~ varb + Rep + Pop*ReprodOutputZ - Pop
  + Diam1_306RO + ShootHt306RO,
  pred, fam, varb, id, root, data=NCPopLongR, famlist=famlist)
feig <- eigen(CDSL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(CDSL, show.graph = TRUE, info.tol=1e-14)

anova(CDSL,CFullL)

# Linear model with ShootHt306 only:
CSL <- aster(resp ~ varb + Rep + Pop*ReprodOutputZ - Pop + ShootHt306RO,
  pred, fam, varb, id, root, data=NCPopLongR, famlist=famlist)
feig <- eigen(CSL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(CSL, show.graph = TRUE, info.tol=1e-13)

anova(CSL,CDSL)

# Linear model with Diam1_306 only:
CDL <- aster(resp ~ varb + Rep + Pop*ReprodOutputZ - Pop + Diam1_306RO,
  pred, fam, varb, id, root, data=NCPopLongR, famlist=famlist)
feig <- eigen(CDL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(CDL, show.graph = TRUE, info.tol=1e-14)

anova(CDL,CDSL)

anova(CPopL,CSL,CDSL,CFullL)
anova(CPopL,CDL,CDSL,CFullL)

# NOTE:  Based on above, diameter and shoot height have biggest linear effects.
# Quadratic DS model:  (Note:  doesn't converge)
CDSQ <- aster(resp ~ varb + Rep + Pop*ReprodOutputZ - Pop
  + Diam1_306RO + ShootHt306RO
  + I(Diam1_306RO^2) + I(ShootHt306RO^2) + I(2 * Diam1_306RO * ShootHt306RO),
  pred, fam, varb, id, root, data=NCPopLongR, famlist=famlist)
feig <- eigen(CDSQ$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(CDSQ, show.graph = TRUE, info.tol=1e-14)
CDSQ$deviance

anova(CDSL, CDSQ)

# Quadratic FS model:  (NOTE:  Doesn't converge)
CFSQ <- aster(resp ~ varb + Rep + Pop*ReprodOutputZ - Pop
  + sqrt(FlowerDateAdjRO) + ShootHt306RO
  + I(sqrt(FlowerDateAdjRO)^2) + I(ShootHt306RO^2)
  + I(2 * sqrt(FlowerDateAdjRO) * ShootHt306RO),
  pred, fam, varb, id, root, data=NCPopLongR, famlist=famlist)
feig <- eigen(CFSQ$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(CFSQ, show.graph = TRUE, info.tol=1e-19)

anova(CFSL, CFSQ)

# Quadratic S model: (NOTE:  Doesn't converge)
CSQ <- aster(resp ~ varb + Rep + Pop*ReprodOutputZ - Pop
  + ShootHt306RO + I(ShootHt306RO^2),
  pred, fam, varb, id, root, data=NCPopLongR, famlist=famlist)
feig <- eigen(CSQ$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(CSQ, show.graph = TRUE, info.tol=1e-13)

anova(CSL, CSQ)

# Quadratic D model:  (NOTE:  Doesn't converge)
CDQ <- aster(resp ~ varb + Rep + Pop*ReprodOutputZ - Pop
  + Diam1_306RO + I(Diam1_306RO^2),
  pred, fam, varb, id, root, data=NCPopLongR, famlist=famlist)
feig <- eigen(CDQ$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(CDQ, show.graph = TRUE, info.tol=1e-17)

anova(CDL, CDQ)

# Quadratic F model:  (NOTE:  Doesn't converge)
CFQ <- aster(resp ~ varb + Rep + Pop*ReprodOutputZ - Pop
  + sqrt(FlowerDateAdjRO) + I(sqrt(FlowerDateAdjRO)^2),
  pred, fam, varb, id, root, data=NCPopLongR, famlist=famlist)
feig <- eigen(CFQ$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(CFQ, show.graph = TRUE, info.tol=1e-9)

anova(CDL, CDQ)

# ---------------------------------------------------------------------------

# GENERATE ML ESTIMATE OF SIZE PARAMETER FOR ReprodOutputZ:
#  This is based on p. 43-45 of Shaw et al. Technical Report #658.

# Define zero-truncated negative binomial distribution:
#  NOTE:  Use an aster run with the same structure as the full model
#    to be tested,using a quasi-Poisson estimated size parameter as a
#    preliminary starting point.  The first line below and subsequent
#    steps should use this run object as its input.
x <- CPopL$x  # Makes data frame with trait(s) in the aster model.
logl <- function(alpha.outR, theta, x) {
  x.flowered <- x[,1]
  theta.flowered <- theta[,1]
  p.flowered <- 1/(1 + exp(-theta.flowered))
  logl.flowered <- sum(dbinom(x.flowered, 1, p.flowered, log = TRUE))
  x.output <- x[x.flowered == 1, 2]
  theta.output <- theta[x.flowered == 1, 2]
  p.output <- (-expm1(theta.output ))
  logl.output <- sum(dnbinom(x.output , size = alpha.outR,
    prob = p.output, log = TRUE) - pnbinom(0, size = alpha.outR,
    prob = p.output, lower.tail = FALSE, log = TRUE))
  logl.flowered + logl.output
}
  
#  Evaluate likelihood profile over a grid of size values:
alpha.outR.seq <- seq(0.5, 4.5, 0.25) # Wider range than used in tr658.
logl.seq <- double(length(alpha.outR.seq))
for (i in 1:length(alpha.outR.seq)) {
  famlist.seq <- famlist
  famlist.seq[[2]] <- fam.truncated.negative.binomial(size = alpha.outR.seq[i],
    truncation = 0)
  CPopL.seq <- aster(CPopL$formula, pred, fam, varb, id, root,
    data=NCPopLongR, famlist = famlist.seq)
  theta.seq <- predict(CPopL.seq, model.type = "cond",
    parm.type = "canon")
  dim(theta.seq) <- dim(x)
  logl.seq[i] <- logl(alpha.outR.seq[i], theta.seq, x)
}
alpha.foo <- seq(min(alpha.outR.seq), max(alpha.outR.seq), 0.01)
logl.foo <- spline(alpha.outR.seq, logl.seq, n = length(alpha.foo))$y
imax <- seq(along = alpha.foo)[logl.foo == max(logl.foo)]
alpha.outR <- alpha.foo[imax]
alpha.out <- alpha.foo[imax] # Some versions of the model call it this.
# Maybe don't need the following:
save(alpha.outR, file = "AsterDataNCPop_alpha.rda", ascii = TRUE)

# ---------------------------------------------------------------------------

# VERSION WITH SEPARATE SHOOTS, SILIQUES AND SEEDS:

# Start with original AsterDataNCPop
# Round counts from sampled traits:
AsterDataNCPop$S06SilPerShoot <- round(AsterDataNCPop$S06SilPerShoot,digits = 0)
AsterDataNCPop$S06SeedPerSil <- round(AsterDataNCPop$S06SeedPerSil,digits = 0)

# Create long form data frame:
vars <- c("Surv306","S06Flowered","S06TotalShootsZ","S06SilPerShoot",
  "S06SeedPerSil")
NCPopLong3 <- reshape(AsterDataNCPop, varying = list(vars), direction = "long",
  timevar = "varb", times = as.factor(vars), v.names = "resp")
# Need to replace NA in non-flowering plants with zeroes:
NCPopLong3$resp <-replace(NCPopLong3$resp, is.na(NCPopLong3$resp), 0)
NCPopLong3[,c(1:5,16:18)] # Check format


# ESTIMATE REPRODUCTIVE OUTPUT PARAMETERS FOR SHOOTS, SILIQUES, SEEDS:
#  (glm model should have similar structure to aster model)
#   Zero-truncated negative binomial distribution
#  This uses the "wide" form of the data:
# SHOOTS:
Shq <- glm(S06TotalShoots ~ Pop + Rep, family=quasipoisson,
 data=AsterDataNCPop)
summShq <- summary(Shq)
dispSh <- summShq$dispersion
meanSh <- mean(AsterDataNCPop$S06TotalShoots, na.rm=TRUE)
# Since size parameter (alpha) in negative binomial = p/(1-p)E(k),
#  and E(disp)=V(k)/E(k)=1/p, then alpha can be estimated as follows:
alphaSh <- (1/dispSh)/(1-(1/dispSh))*meanSh
# SILIQUES/SHOOT:
Siq <- glm(S06SilPerShoot ~ Pop + Rep, family=quasipoisson,
 data=AsterDataNCPop)
summSiq <- summary(Siq)
dispSi <- summSiq$dispersion
meanSi <- mean(AsterDataNCPop$S06SilPerShoot, na.rm=TRUE)
# Since size parameter (alpha) in negative binomial = p/(1-p)E(k),
#  and E(disp)=V(k)/E(k)=1/p, then alpha can be estimated as follows:
alphaSi <- (1/dispSi)/(1-(1/dispSi))*meanSi
# SEEDS/SILIQUE:
Seq <- glm(S06SeedPerSil ~ Pop + Rep, family=quasipoisson,
 data=AsterDataNCPop)
summSeq <- summary(Seq)
dispSe <- summSeq$dispersion
meanSe <- mean(AsterDataNCPop$S06SeedPerSil, na.rm=TRUE)
# Since size parameter (alpha) in negative binomial = p/(1-p)E(k),
#  and E(disp)=V(k)/E(k)=1/p, then alpha can be estimated as follows:
alphaSe <- (1/dispSe)/(1-(1/dispSe))*meanSe

# GENERATE A LIST OF PROBABILITY DISTRIBUTION FAMILIES:
famlist <- list(fam.bernoulli(), fam.truncated.negative.binomial
  (size = alphaSh, truncation = 0), fam.truncated.negative.binomial
  (size = alphaSi, truncation = 0), fam.truncated.negative.binomial
  (size = alphaSe, truncation = 0))

# Now we can set up the model:
#  First look at field names for both "wide" and "long" versions of data:
names(AsterDataNCPop)
names(NCPopLong3)
#  Second, add root:
NCPopLong3 <- data.frame(NCPopLong3, root=1)
#  Third, add the desired fitness measures as pseudo-covariates:
S06TotalShootsZ <- grep("S06TotalShootsZ", as.character(NCPopLong3$varb))
S06TotalShootsZ <- is.element(seq(along = NCPopLong3$varb), S06TotalShootsZ)
NCPopLong3 <- data.frame(NCPopLong3, S06TotalShootsZ = as.integer(S06TotalShootsZ))
S06SilPerShoot <- grep("S06SilPerShoot", as.character(NCPopLong3$varb))
S06SilPerShoot <- is.element(seq(along = NCPopLong3$varb), S06SilPerShoot)
NCPopLong3 <- data.frame(NCPopLong3, S06SilPerShoot = as.integer(S06SilPerShoot))
S06SeedPerSil <- grep("S06SeedPerSil", as.character(NCPopLong3$varb))
S06SeedPerSil <- is.element(seq(along = NCPopLong3$varb), S06SeedPerSil)
NCPopLong3 <- data.frame(NCPopLong3, S06SeedPerSil = as.integer(S06SeedPerSil))
#  Fourth, establish pred and families vectors:
vars  # as a reminder of the order
pred <- c(0,1,2,3,4)
fam <- c(1,1,2,3,4)
#  Check to see that the right families are specified:
sapply(famlist, as.character)[fam]

#  And, at last, the "reduced" model (NOTE:  giving errors so far):
RepOut1 <- aster(resp ~ varb + Rep, pred, fam, varb, id, root,
  data=NCPopLong3, famlist=famlist)
summary(RepOut1, show.graph = TRUE)

#  Model with Pop effects on siliques (Also giving errors):
RepOut2 <- aster(resp ~ varb + Rep + Pop*S06SeedPerSil - Pop,
  pred, fam, varb, id, root, data=NCPopLong3, famlist=famlist)
summary(RepOut2, show.graph = TRUE)

# ---------------------------------------------------------------------------

# VERSION WITH SEPARATE SEEDS, SILIQUES, SHOOTS, W/O SURVIVAL:
#  Uses zero-truncated negative binomial distribution
# Start with original AsterDataNCPop
# Round counts from sampled traits:
AsterDataNCPop$S06SilPerShoot <- round(AsterDataNCPop$S06SilPerShoot,digits = 0)
AsterDataNCPop$S06SeedPerSil <- round(AsterDataNCPop$S06SeedPerSil,digits = 0)
# Optional:  check format:
# AsterDataNCPop[,c(1:6)]

# Create long form data frame from scratch:
vars <- c("S06Flowered","S06TotalShootsZ","S06SilPerShoot",
  "S06SeedPerSil")
NCPopLong4 <- reshape(AsterDataNCPop, varying = list(vars), direction = "long",
  timevar = "varb", times = as.factor(vars), v.names = "resp")
# Need to replace NA in non-flowering plants with zeroes:
NCPopLong4$resp <-replace(NCPopLong4$resp, is.na(NCPopLong4$resp), 0)
NCPopLong4[,c(1:5,16:18)] # Check format

# Use parameter estimation scripts ABOVE to estimate reproductive output
#  parameters for shoots, siliques, seeds, followed by script to generate
#  list of probability distribution families.

# Now we can set up the model:
#  First look at field names for both "wide" and "long" versions of data:
names(AsterDataNCPop)
names(NCPopLong4)

#  Second, add root:
NCPopLong4 <- data.frame(NCPopLong4, root=1)
#  Third, add the desired fitness measures as pseudo-covariates:
S06TotalShootsZ <- grep("S06TotalShootsZ", as.character(NCPopLong4$varb))
S06TotalShootsZ <- is.element(seq(along = NCPopLong4$varb), S06TotalShootsZ)
NCPopLong4 <- data.frame(NCPopLong4, S06TotalShootsZ = as.integer(S06TotalShootsZ))
S06SilPerShoot <- grep("S06SilPerShoot", as.character(NCPopLong4$varb))
S06SilPerShoot <- is.element(seq(along = NCPopLong4$varb), S06SilPerShoot)
NCPopLong4 <- data.frame(NCPopLong4, S06SilPerShoot = as.integer(S06SilPerShoot))
S06SeedPerSil <- grep("S06SeedPerSil", as.character(NCPopLong4$varb))
S06SeedPerSil <- is.element(seq(along = NCPopLong4$varb), S06SeedPerSil)
NCPopLong4 <- data.frame(NCPopLong4, S06SeedPerSil = as.integer(S06SeedPerSil))
#  Fourth, establish pred and families vectors:
vars  # as a reminder of the order
pred <- c(0, 1, 2, 3)
fam <- c(1, 2, 3, 4)
#  Check to see that the right families are specified:
sapply(famlist, as.character)[fam]

# Alternately, modify long form from version with survival:
NCPopLong4 <- subset(NCPopLong3, varb != "Surv306")
vars <- c("S06Flowered","S06TotalShootsZ","S06SilPerShoot",
  "S06SeedPerSil")  # Change to remove Surv306
# Then start with "add root" above.

#  And, at last, the "reduced" model [NOTE-Gives negative Fisher eigenvalues]:
RepOut4.1 <- aster(resp ~ varb + Rep, pred, fam, varb, id, root,
  data=NCPopLong4, famlist=famlist)
summary(RepOut4.1, show.graph = TRUE)
feig <- eigen(RepOut4.1$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(RepOut4.1, show.graph = TRUE, info.tol=1e-18)

#  Model with Pop effects on siliques [NOTE - Gives singular system]:
RepOut4.2 <- aster(resp ~ varb + Rep + Pop*S06SeedPerSil - Pop,
  pred, fam, varb, id, root, data=NCPopLong4, famlist=famlist)
summary(RepOut4.2, show.graph = TRUE)
feig <- eigen(RepOut4.2$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(RepOut4.2, show.graph = TRUE, info.tol=1e-19)

anova(RepOut4.1, RepOut4.2)

# ---------------------------------------------------------------------------

# VERSION WITH SEPARATE SEEDS, SILIQUES, SHOOTS, W/O SURVIVAL, NORMAL DIST:

# Start with original AsterDataNCPop
# Transform reproductive output variables:
AsterDataNCPop$ShootSqrt <- sqrt(AsterDataNCPop$S06TotalShootsZ)
AsterDataNCPop$SilSqrt <- sqrt(AsterDataNCPop$S06SilPerShoot)
AsterDataNCPop$SeedSqrt <- sqrt(AsterDataNCPop$S06SeedPerSil)
# Optional:  check format:
# AsterDataNCPop[,c(1:6)]
# Create long form data frame from scratch:
vars <- c("S06Flowered","ShootSqrt","SilSqrt","SeedSqrt")
NCPopLong4 <- reshape(AsterDataNCPop, varying = list(vars), direction = "long",
  timevar = "varb", times = as.factor(vars), v.names = "resp")
# Need to replace NA in non-flowering plants with zeroes:
NCPopLong4$resp <-replace(NCPopLong4$resp, is.na(NCPopLong4$resp), 0)
NCPopLong4[,c(1:5,16:18)] # Check format

# Parameter estimates for normal dist:
# SHOOTS:
Shn <- lm(ShootSqrt ~ Pop + Rep, data=AsterDataNCPop)
Sh.sd <- sd(resid(summary(Shn)))
ShnRed <- lm(ShootSqrt ~ Rep, data=AsterDataNCPop)
ShR.sd <- sd(resid(summary(ShnRed)))
# anova(Shn, ShnRed)
# SILIQUES/SHOOT:
Sin <- lm(SilSqrt ~ Pop + Rep, data=AsterDataNCPop)
Si.sd <- sd(resid(summary(Sin)))
SinRed <- lm(SilSqrt ~ Rep, data=AsterDataNCPop)
SiR.sd <- sd(resid(summary(SinRed)))
# SEEDS/SILIQUE:
Sen <- lm(SeedSqrt ~ Pop + Rep, data=AsterDataNCPop)
Se.sd <- sd(resid(summary(Sen)))
SenRed <- lm(SeedSqrt ~ Rep, data=AsterDataNCPop)
SeR.sd <- sd(resid(summary(SenRed)))

# test residual normality:
qqnorm(resid(Sen))
qqline(resid(Sen), lty=2)

# Generate list of probability distribution families:
famlist <- list(fam.bernoulli(), fam.normal.location(ShR.sd),
  fam.normal.location(SiR.sd), fam.normal.location(SeR.sd),
  fam.normal.location(Sh.sd), fam.normal.location(Si.sd),
  fam.normal.location(Se.sd))

# Now we can set up the model:
#  First look at field names for both "wide" and "long" versions of data:
names(AsterDataNCPop)
names(NCPopLong4)

#  Second, add root:
NCPopLong4 <- data.frame(NCPopLong4, root=1)
#  Third, add the desired fitness measures as pseudo-covariates:
ShootSqrt <- grep("ShootSqrt", as.character(NCPopLong4$varb))
ShootSqrt <- is.element(seq(along = NCPopLong4$varb), ShootSqrt)
NCPopLong4 <- data.frame(NCPopLong4, ShootSqrt = as.integer(ShootSqrt))
SilSqrt <- grep("SilSqrt", as.character(NCPopLong4$varb))
SilSqrt <- is.element(seq(along = NCPopLong4$varb), SilSqrt)
NCPopLong4 <- data.frame(NCPopLong4, SilSqrt = as.integer(SilSqrt))
SeedSqrt <- grep("SeedSqrt", as.character(NCPopLong4$varb))
SeedSqrt <- is.element(seq(along = NCPopLong4$varb), SeedSqrt)
NCPopLong4 <- data.frame(NCPopLong4, SeedSqrt = as.integer(SeedSqrt))
#  Fourth, establish pred and families vectors:
vars  # as a reminder of the order
pred <- c(0, 1, 2, 3)
#  For reduced model:
fam <- c(1, 2, 3, 4)
#  For full model:
famF <- c(1,5,6,7)
#  Check to see that the right families are specified:
sapply(famlist, as.character)[fam]
sapply(famlist, as.character)[famF]

# with(NCPopLong4, NCPopLong4[Final == 1 | Final == 2,])
# levels(NCPopLong4$FamFixed)

#  And, at last, the "reduced" model (WORKS!!):
RepOut1 <- aster(resp ~ varb + Rep, pred, fam, varb, id, root,
  data=NCPopLong4, famlist=famlist, method="trust")
summary(RepOut1, show.graph = TRUE)

#  Model with Pop effects on siliques:
RepOut2 <- aster(resp ~ varb + Rep + Pop*SeedSqrt - Pop,
  pred, famF, varb, id, root, data=NCPopLong4, famlist=famlist, method="trust")
summary(RepOut2, show.graph = TRUE)

anova(RepOut1, RepOut2)


# ---------------------------------------------------------------------------

# VERSION WITH TOTAL REPRODUCTION, W/O SURVIVAL, NORMAL DIST:

# Start with original AsterDataNCPop
# Transform reproductive output:
# NOTE: Use sqrt for comparability to version with separate traits,
#  even though cube root gives slightly better normality.
AsterDataNCPop$ReprodSqrt <- sqrt(AsterDataNCPop$ReprodOutputZ)

# Add fields for alternate population groupings:
AsterDataNCPop$PopSN <- AsterDataNCPop$Pop
levels(AsterDataNCPop$PopSN)[c(4,6)] <- "SandN"
AsterDataNCPop$PopF2N <- AsterDataNCPop$Pop
levels(AsterDataNCPop$PopF2N)[c(2,6)] <- "F2andN"

# Optional:  check format:
# AsterDataNCPop[,c(1:6)]

# Create long form data frame from scratch:
vars <- c("S06Flowered","ReprodSqrt")
NCPopLong3 <- reshape(AsterDataNCPop, varying = list(vars), direction = "long",
  timevar = "varb", times = as.factor(vars), v.names = "resp")
# Need to replace NA in non-flowering plants with zeroes (not needed here!):
# NCPopLong3$resp <-replace(NCPopLong3$resp, is.na(NCPopLong3$resp), 0)
NCPopLong3[,c(1:5,16:18)] # Check format

# Parameter estimates for reprod. output normal dist:
ROn <- lm(ReprodSqrt ~ Pop + Rep, data=AsterDataNCPop)
RO.sd <- sd(resid(summary(ROn)))
ROnRed <- lm(ReprodSqrt ~ Rep, data=AsterDataNCPop)
RoR.sd <- sd(resid(summary(ROnRed)))
ROF <- lm(ReprodSqrt ~ FamFixed + Rep, data=AsterDataNCPop)
ROF.sd <- sd(resid(summary(ROF)))
ROSN <- lm(ReprodSqrt ~ PopSN + Rep, data=AsterDataNCPop)
ROSN.sd <- sd(resid(summary(ROSN)))
ROF2N <- lm(ReprodSqrt ~ PopF2N + Rep, data=AsterDataNCPop)
ROF2N.sd <- sd(resid(summary(ROF2N)))

# Optional:  test marginal significance of reproductive output:
anova(ROn,ROnRed)
anova(ROF,ROn)
anova(ROn,ROSN)
anova(ROn,ROF2N)

# test residual normality:
qqnorm(resid(ROn))
qqline(resid(ROn), lty=2)

# Generate list of probability distribution families:
famlist <- list(fam.bernoulli(), fam.normal.location(RO.sd),
  fam.normal.location(ROF.sd))

# Now we can set up the model:
#  First look at field names for both "wide" and "long" versions of data:
names(AsterDataNCPop)
names(NCPopLong3)

#  Second, add root:
NCPopLong3 <- data.frame(NCPopLong3, root=1)
#  Third, add the desired fitness measures as pseudo-covariates:
ReprodSqrt <- grep("ReprodSqrt", as.character(NCPopLong3$varb))
ReprodSqrt <- is.element(seq(along = NCPopLong3$varb), ReprodSqrt)
NCPopLong3 <- data.frame(NCPopLong3, ReprodSqrt = as.integer(ReprodSqrt))
#  Fourth, establish pred and families vectors:
vars  # as a reminder of the order
pred <- c(0, 1)
fam <- c(1, 2) # Main model (reciprocals combined)
famF <- c(1, 3) # Full model (reciprocals separate)
#  Check to see that the right families are specified:
sapply(famlist, as.character)[fam]
sapply(famlist, as.character)[famF]

# with(NCPopLong3, NCPopLong3[Final == 1 | Final == 2,])
# levels(NCPopLong3$FamFixed)

#  And, at last, the "reduced" model (WORKS!!):
RepOut1 <- aster(resp ~ varb + Rep, pred, fam, varb, id, root,
  data=NCPopLong3, famlist=famlist)
summary(RepOut1, show.graph = TRUE)

#  Model with Pop effects on reprod. output:
RepOut2 <- aster(resp ~ varb + Rep + Pop*ReprodSqrt - Pop,
  pred, fam, varb, id, root, data=NCPopLong3, famlist=famlist)
summary(RepOut2, show.graph = TRUE)

#  Model with Pop effects on reprod. output (with ROF residuals):
RepOut2a <- aster(resp ~ varb + Rep + Pop*ReprodSqrt - Pop,
  pred, famF, varb, id, root, data=NCPopLong3, famlist=famlist)
summary(RepOut2a, show.graph = TRUE)

#  Model with separate F2 reciprocals:
RepOut3 <- aster(resp ~ varb + Rep + FamFixed*ReprodSqrt - FamFixed,
  pred, famF, varb, id, root, data=NCPopLong3, famlist=famlist)
summary(RepOut3, show.graph = TRUE)

#  Model with S and N combined:
RepOut4 <- aster(resp ~ varb + Rep + PopSN*ReprodSqrt - PopSN,
  pred, fam, varb, id, root, data=NCPopLong3, famlist=famlist)
summary(RepOut4, show.graph = TRUE)

#  Model with F2 and N combined:
RepOut5 <- aster(resp ~ varb + Rep + PopF2N*ReprodSqrt - PopF2N,
  pred, fam, varb, id, root, data=NCPopLong3, famlist=famlist)
summary(RepOut3, show.graph = TRUE)

anova(RepOut1, RepOut2) # Test overall Pop effect.
anova(RepOut2a, RepOut3) # Test reciprocal effect.
anova(RepOut4, RepOut2) # Test N vs. S.
anova(RepOut5, RepOut2) # Test N vs. F2.

# ---------------------------------------------------------------------------

# VERSION FOR TRAIT REGRESSIONS ON FITNESS:
# NOTE:  Similar to previous model, but using untransformed reprod. output
#  Do separately by population:

# Optional:  check format:
# AsterDataNCPop[,c(1:6)]
# Change FlowerDate and Diam1_306 NA values to zeroes:
AsterDataNCPop$FlowerDate <- replace(AsterDataNCPop$FlowerDate,
  is.na(AsterDataNCPop$FlowerDate), 0)
AsterDataNCPop$Diam1_306 <- replace(AsterDataNCPop$Diam1_306,
  is.na(AsterDataNCPop$Diam1_306), 0)

# Create long form data frame from scratch:
vars <- c("S06Flowered","ReprodOutputZ")
NCPopLongR <- reshape(AsterDataNCPop, varying = list(vars), direction = "long",
  timevar = "varb", times = as.factor(vars), v.names = "resp")
# Need to replace NA in non-flowering plants with zeroes (not needed here!):
# NCPopLong2$resp <-replace(NCPopLong2$resp, is.na(NCPopLong2$resp), 0)
names(NCPopLongR) # Check field names, then modify next statements if needed
# OPTIONAL tests to check formatting:
NCPopLongR[c(1:5),c(1:5,10,12,24,25)]
NCPopLongR[c(1:5),]
NCPopLongR[,c(1,3,4,25,28:31)]

#  Add root:
NCPopLongR <- data.frame(NCPopLongR, root=1)
#  Add the desired fitness measures as pseudo-covariates:
ReprodOutputZ <- grep("ReprodOutputZ", as.character(NCPopLongR$varb))
ReprodOutputZ <- is.element(seq(along = NCPopLongR$varb), ReprodOutputZ)
NCPopLongR <- data.frame(NCPopLongR, ReprodOutputZ = as.integer(ReprodOutputZ))

# Make indicator for ReprodOutputZ, and create conditional fields
#  for FlowerDate, Diam1_306, and ShootHt306:
# First remove record with ReprodOutput, no FlowerDate:
NCPopLongR <- with(NCPopLongR, NCPopLongR[Final != 764,])
ReOutInd <- as.numeric(as.character(NCPopLongR$varb)=="ReprodOutputZ")
NCPopLongR$FlowerDateRO <- NCPopLongR$FlowerDate*ReOutInd
NCPopLongR$Diam1_306RO <- NCPopLongR$Diam1_306*ReOutInd
NCPopLongR$ShootHt306RO <- NCPopLongR$ShootHt306*ReOutInd

# Create long form versions for each population separately:
NCPopLongRN <- with(NCPopLongR, NCPopLongR[Pop == "N",])
NCPopLongRS <- with(NCPopLongR, NCPopLongR[Pop == "S",])
NCPopLongRF2 <- with(NCPopLongR, NCPopLongR[Pop == "F2",])
names(NCPopLongRF2) # Check field names, modify next statement if needed
NCPopLongRF2[,c(1:5,24,25)] # Check for correct records

# Parameter estimates for reprod. output normal dist:
ROnN <- lm(ReprodOutputZ ~ Rep, data=AsterDataNCPop,
  subset=(Pop == "N"))
ROnN.sd <- sd(resid(summary(ROnN)))
ROnS <- lm(ReprodOutputZ ~ Rep, data=AsterDataNCPop,
  subset=(Pop == "S"))
ROnS.sd <- sd(resid(summary(ROnS)))
ROnF2 <- lm(ReprodOutputZ ~ Rep, data=AsterDataNCPop,
  subset=(Pop == "F2"))
ROnF2.sd <- sd(resid(summary(ROnF2)))

# Generate list of probability distribution families:
famlist <- list(fam.bernoulli(), fam.normal.location(ROnN.sd),
  fam.normal.location(ROnS.sd), fam.normal.location(ROnF2.sd))

#  Establish pred and families vectors:
vars  # as a reminder of the order
pred <- c(0, 1)
famN <- c(1, 2) # For NC regressions
famS <- c(1, 3) # For Spiterstulen regressions
famF2 <- c(1, 4) # For F2 regressions
#  Check to see that the right families are specified:
sapply(famlist, as.character)[famN]
sapply(famlist, as.character)[famS]
sapply(famlist, as.character)[famF2]

# Digression - examine normality of fitness and prospective correlates:
#  Reproductive output:
with(AsterDataNCPop,qqnorm(AsterDataNCPop$ReprodOutput[Pop == "S"]))
with(AsterDataNCPop,qqline(AsterDataNCPop$ReprodOutput[Pop == "S"], lty=2))
with(AsterDataNCPop,qqnorm(AsterDataNCPop$ReprodOutput[Pop == "N"]))
with(AsterDataNCPop,qqline(AsterDataNCPop$ReprodOutput[Pop == "N"], lty=2))
with(AsterDataNCPop,qqnorm(AsterDataNCPop$ReprodOutput[Pop == "F2"]))
with(AsterDataNCPop,qqline(AsterDataNCPop$ReprodOutput[Pop == "F2"], lty=2))
#  Shoot height:
with(AsterDataNCPop,qqnorm(AsterDataNCPop$ShootHt306[Pop == "S"]))
with(AsterDataNCPop,qqline(AsterDataNCPop$ShootHt306[Pop == "S"], lty=2))
with(AsterDataNCPop,qqnorm(AsterDataNCPop$ShootHt306[Pop == "N"]))
with(AsterDataNCPop,qqline(AsterDataNCPop$ShootHt306[Pop == "N"], lty=2))
with(AsterDataNCPop,qqnorm(AsterDataNCPop$ShootHt306[Pop == "F2"]))
with(AsterDataNCPop,qqline(AsterDataNCPop$ShootHt306[Pop == "F2"], lty=2))
#  Spring diameter:
with(AsterDataNCPop,qqnorm(AsterDataNCPop$Diam1_306[Pop == "S"]))
with(AsterDataNCPop,qqline(AsterDataNCPop$Diam1_306[Pop == "S"], lty=2))
with(AsterDataNCPop,qqnorm(AsterDataNCPop$Diam1_306[Pop == "N"]))
with(AsterDataNCPop,qqline(AsterDataNCPop$Diam1_306[Pop == "N"], lty=2))
with(AsterDataNCPop,qqnorm(AsterDataNCPop$Diam1_306[Pop == "F2"]))
with(AsterDataNCPop,qqline(AsterDataNCPop$Diam1_306[Pop == "F2"], lty=2))
#  Flowering date:
with(AsterDataNCPop,qqnorm(AsterDataNCPop$FlowerDate[Pop == "S" 
  & FlowerDate != 0]))
with(AsterDataNCPop,qqline(AsterDataNCPop$FlowerDate[Pop == "S" 
  & FlowerDate != 0], lty=2))
with(AsterDataNCPop,qqnorm(AsterDataNCPop$FlowerDate[Pop == "N" 
  & FlowerDate != 0]))
with(AsterDataNCPop,qqline(AsterDataNCPop$FlowerDate[Pop == "N" 
  & FlowerDate != 0], lty=2))
with(AsterDataNCPop,qqnorm(AsterDataNCPop$FlowerDate[Pop == "F2" 
  & FlowerDate != 0]))
with(AsterDataNCPop,qqline(AsterDataNCPop$FlowerDate[Pop == "F2" 
  & FlowerDate != 0], lty=2))

# ASTER ANALYSES FOR N:

# Reduced model:  (NOTE:  null eigenvectors of information)
NRed <- aster(resp ~ varb + Rep, pred, famN, varb, id, root,
  data=NCPopLongRN, famlist=famlist)
summary(NRed, show.graph = TRUE)
feig <- eigen(NRed$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(NRed, show.graph = TRUE, info.tol=1e-21)


# Full linear model:  (NOTE:  null eigenvectors of information)
NFullL <- aster(resp ~ varb + Rep + FlowerDateRO + Diam1_306RO + ShootHt306RO,
  pred, famN, varb, id, root, data=NCPopLongRN, famlist=famlist)
summary(NFullL, show.graph = TRUE)

# ASTER ANALYSES FOR S:

# Reduced model:
SRed <- aster(resp ~ varb + Rep, pred, famS, varb, id, root,
  data=NCPopLongRS, famlist=famlist)
summary(SRed, show.graph = TRUE)
SRed$deviance

# Full linear model:)
SFullL <- aster(resp ~ varb + Rep + FlowerDateRO + Diam1_306RO + ShootHt306RO,
  pred, famS, varb, id, root, data=NCPopLongRS, famlist=famlist)
summary(SFullL, show.graph = TRUE)
feig <- eigen(SFullL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(SFullL, show.graph = TRUE, info.tol=1e-15)
SFullL$deviance

anova(SRed,SFullL)

# Linear model with FlowerDate and Diam1_306 only:
SFDL <- aster(resp ~ varb + Rep + FlowerDateRO + Diam1_306RO,
  pred, famS, varb, id, root, data=NCPopLongRS, famlist=famlist)
summary(SFDL, show.graph = TRUE)
feig <- eigen(SFDL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(SFDL, show.graph = TRUE, info.tol=1e-15)
SFDL$deviance

anova(SFDL,SFullL)

# Linear model with FlowerDate and ShootHt306 only:
SFSL <- aster(resp ~ varb + Rep + FlowerDateRO + ShootHt306RO,
  pred, famS, varb, id, root, data=NCPopLongRS, famlist=famlist)
summary(SFSL, show.graph = TRUE)
feig <- eigen(SFSL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(SFSL, show.graph = TRUE, info.tol=1e-15)
SFSL$deviance

anova(SFSL,SFullL)

# Linear model with Diam1_306 and ShootHt306 only:
SDSL <- aster(resp ~ varb + Rep + Diam1_306RO + ShootHt306RO,
  pred, famS, varb, id, root, data=NCPopLongRS, famlist=famlist)
summary(SDSL, show.graph = TRUE)
feig <- eigen(SDSL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(SDSL, show.graph = TRUE, info.tol=1e-10)
SDSL$deviance

anova(SDSL,SFullL)

# Linear model with ShootHt306 only:
SSL <- aster(resp ~ varb + Rep + ShootHt306RO,
  pred, famS, varb, id, root, data=NCPopLongRS, famlist=famlist)
summary(SSL, show.graph = TRUE)
feig <- eigen(SSL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(SSL, show.graph = TRUE, info.tol=1e-9)
SSL$deviance

anova(SSL,SFullL)
anova(SSL,SDSL,SFullL)
anova(SRed,SSL,SDSL,SFullL)
anova(SRed,SSL,SDSL,SFullL)
anova(SRed,SSL,SFSL,SFullL)

# Quadratic DS model:
SDSQ <- aster(resp ~ varb + Rep + Diam1_306RO + ShootHt306RO
  + I(Diam1_306RO^2) + I(ShootHt306RO^2) + I(2 * Diam1_306RO * ShootHt306RO),
  pred, famS, varb, id, root, data=NCPopLongRS, famlist=famlist)
summary(SDSQ, show.graph = TRUE)
feig <- eigen(SDSQ$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(SDSQ, show.graph = TRUE, info.tol=1e-13)
SDSQ$deviance

anova(SDSL, SDSQ)

# Quadratic FS model:  # NOTE:  model doesn't converge
SFSQ <- aster(resp ~ varb + Rep + FlowerDateRO + ShootHt306RO
  + I(FlowerDateRO^2) + I(ShootHt306RO^2) + I(2 * FlowerDateRO * FlowerDateRO),
  pred, famS, varb, id, root, data=NCPopLongRS, famlist=famlist)
summary(SFSQ, show.graph = TRUE)
feig <- eigen(SFSQ$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(SFSQ, show.graph = TRUE, info.tol=1e-13)
SFSQ$deviance

anova(SFSL, SFSQ)

# Quadratic S model:
SSQ <- aster(resp ~ varb + Rep + ShootHt306RO + ShootHt306RO^2,
  pred, famS, varb, id, root, data=NCPopLongRS, famlist=famlist)
summary(SSQ, show.graph = TRUE)
feig <- eigen(SSQ$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(SSQ, show.graph = TRUE, info.tol=1e-9)
SSQ$deviance

anova(SSL, SSQ)

# PLOT RESULTS OF QUADRATIC DS MODEL:
# First create wide form of data:
data(NCPopLongRS)
NCPopWideRS <- with(NCPopLongRS, reshape(NCPopLongRS, direction = "wide",
  timevar = "varb",
  v.names = "resp", varying = list(levels(NCPopLongRS$varb))))
names(NCPopWideRS)
NCPopWideRS[c(1:5),]

# Make A matrix:
a1 <- SDSQ$coefficients["Diam1_306RO"]
a2 <- SDSQ$coefficients["ShootHt306RO"]
a <- c(a1, a2)
A11 <- SDSQ$coefficients["I(Diam1_306RO^2)"]
A22 <- SDSQ$coefficients["I(ShootHt306RO^2)"]
A12 <- SDSQ$coefficients["I(2 * Diam1_306RO * ShootHt306RO)"]
A <- matrix(c(A11, A12, A12, A22), 2, 2)

# Test shape:
eigen(A, symmetric = TRUE, only.values = TRUE)$values

# Find maxima, if possible:  NOTE: Eigen of A is positive,
#  so this might be a minimum
maxSDSQ <- (-solve(A, a)/2)
print(maxSDSQ)

# Generate graph:
#  NOTE:  Use original fields, as RO fields are corrupted in conversion
plot(NCPopWideRS$Diam1_306, NCPopWideRS$ShootHt306, xlab = "Diam",
  ylab = "ShootHt")
ufoo <- par("usr")
nx <- 101
ny <- 101
z <- matrix(NA, nx, ny)
x <- seq(ufoo[1], ufoo[2], length = nx)
y <- seq(ufoo[3], ufoo[4], length = ny)
points(maxSDSQ[1], maxSDSQ[2], pch = 19)
for (i in 1:nx) {
for (j in 1:ny) {
b <- c(x[i], y[j])
z[i, j] <- sum(a * b) + as.numeric(t(b) %*% A %*%
b)
}
}
b <- as.numeric(maxSDSQ)
contour(x, y, z, add = TRUE)
contour(x, y, z, levels = c(0.325), add = TRUE)

# ASTER ANALYSES FOR F2:

# Reduced model:
F2Red <- aster(resp ~ varb + Rep, pred, famF2, varb, id, root,
  data=NCPopLongRF2, famlist=famlist)
summary(F2Red, show.graph = TRUE)
feig <- eigen(F2Red$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(F2Red, show.graph = TRUE, info.tol=1e-10)
F2Red$deviance

# Full linear model:  NOTE:  Doesn't converge
F2FullL <- aster(resp ~ varb + Rep + FlowerDateRO + Diam1_306RO + ShootHt306RO,
  pred, famF2, varb, id, root, data=NCPopLongRF2, famlist=famlist)
summary(F2FullL, show.graph = TRUE)
feig <- eigen(F2FullL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(F2FullL, show.graph = TRUE, info.tol=1e-15)
F2FullL$deviance

anova(F2Red,F2FullL)

# Linear model with FlowerDate and Diam1_306 only:
F2FDL <- aster(resp ~ varb + Rep + FlowerDateRO + Diam1_306RO,
  pred, famF2, varb, id, root, data=NCPopLongRF2, famlist=famlist)
summary(F2FDL, show.graph = TRUE)
feig <- eigen(F2FDL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(F2FDL, show.graph = TRUE, info.tol=1e-17)
F2FDL$deviance

anova(F2FDL,F2FullL)

# Linear model with FlowerDate and ShootHt306 only:
#  NOTE:  Doesn't converge
F2FSL <- aster(resp ~ varb + Rep + FlowerDateRO + ShootHt306RO,
  pred, famF2, varb, id, root, data=NCPopLongRF2, famlist=famlist)
summary(F2FSL, show.graph = TRUE)
feig <- eigen(F2FSL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(F2FSL, show.graph = TRUE, info.tol=1e-15)
F2FSL$deviance

anova(F2FSL,F2FullL)

# Linear model with Diam1_306 and ShootHt306 only:
F2DSL <- aster(resp ~ varb + Rep + Diam1_306RO + ShootHt306RO,
  pred, famF2, varb, id, root, data=NCPopLongRF2, famlist=famlist)
summary(F2DSL, show.graph = TRUE)
feig <- eigen(F2DSL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(F2DSL, show.graph = TRUE, info.tol=1e-13)
F2DSL$deviance

# Linear model with Diam1_306 only:
F2DL <- aster(resp ~ varb + Rep + Diam1_306RO,
  pred, famF2, varb, id, root, data=NCPopLongRF2, famlist=famlist)
summary(F2DL, show.graph = TRUE)
feig <- eigen(F2DL$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(F2DL, show.graph = TRUE, info.tol=1e-13)
F2DL$deviance

anova(F2Red,F2DL,F2DSL)

# Quadratic DS model:  NOTE:  Does not converge
F2DSQ <- aster(resp ~ varb + Rep + Diam1_306RO + ShootHt306RO
  + I(Diam1_306RO^2) + I(ShootHt306RO^2) + I(2 * Diam1_306RO * ShootHt306RO),
  pred, famF2, varb, id, root, data=NCPopLongRF2, famlist=famlist)
summary(F2DSQ, show.graph = TRUE)
feig <- eigen(F2DSQ$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(F2DSQ, show.graph = TRUE, info.tol=1e-18)
F2DSQ$deviance

anova(F2DSL, F2DSQ)

# Quadratic FS model:  # NOTE:  model doesn't converge
F2FSQ <- aster(resp ~ varb + Rep + FlowerDateRO + ShootHt306RO
  + I(FlowerDateRO^2) + I(ShootHt306RO^2) + I(2 * FlowerDateRO * FlowerDateRO),
  pred, famF2, varb, id, root, data=NCPopLongRF2, famlist=famlist)
summary(F2FSQ, show.graph = TRUE)
feig <- eigen(F2FSQ$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(F2FSQ, show.graph = TRUE, info.tol=1e-13)
F2FSQ$deviance

anova(F2FSL, F2FSQ)

# Quadratic S model:
F2SQ <- aster(resp ~ varb + Rep + ShootHt306RO + ShootHt306RO^2,
  pred, famF2, varb, id, root, data=NCPopLongRF2, famlist=famlist)
summary(F2SQ, show.graph = TRUE)
feig <- eigen(F2SQ$fisher, symmetric = TRUE, only.values = TRUE)$values
range(feig)
summary(F2SQ, show.graph = TRUE, info.tol=1e-9)
F2SQ$deviance

anova(F2SL, F2SQ)

# PLOT RESULTS OF QUADRATIC DS MODEL:
# First create wide form of data:
NCPopWideRF2 <- with(NCPopLongRF2, reshape(NCPopLongRF2, direction = "wide",
  timevar = "varb",
  v.names = "resp", varying = list(levels(NCPopLongRF2$varb))))
names(NCPopWideRF2)
NCPopWideRF2[c(1:5),]

# Make A matrix:
a1 <- F2DSQ$coefficients["Diam1_306RO"]
a2 <- F2DSQ$coefficients["ShootHt306RO"]
a <- c(a1, a2)
A11 <- F2DSQ$coefficients["I(Diam1_306RO^2)"]
A22 <- F2DSQ$coefficients["I(ShootHt306RO^2)"]
A12 <- F2DSQ$coefficients["I(2 * Diam1_306RO * ShootHt306RO)"]
A <- matrix(c(A11, A12, A12, A22), 2, 2)

# Test shape:
eigen(A, symmetric = TRUE, only.values = TRUE)$values

# Find maxima, if possible:  NOTE: Eigen of A is positive,
#  so this might be a minimum
maxF2DSQ <- (-solve(A, a)/2)
print(maxF2DSQ)

# Generate graph:
#  NOTE:  Use original fields, as RO fields are corrupted in conversion
plot(NCPopWideRF2$Diam1_306, NCPopWideRF2$ShootHt306, xlab = "Diam",
  ylab = "ShootHt")
ufoo <- par("usr")
nx <- 101
ny <- 101
z <- matrix(NA, nx, ny)
x <- seq(ufoo[1], ufoo[2], length = nx)
y <- seq(ufoo[3], ufoo[4], length = ny)
points(maxF2DSQ[1], maxF2DSQ[2], pch = 19)
for (i in 1:nx) {
for (j in 1:ny) {
b <- c(x[i], y[j])
z[i, j] <- sum(a * b) + as.numeric(t(b) %*% A %*%
b)
}
}
b <- as.numeric(maxF2DSQ)
contour(x, y, z, add = TRUE)
contour(x, y, z, levels = c(0.325), add = TRUE)


# ---------------------------------------------------------------------------


# DO TEST MODEL THAT INCLUDES ONLY FLOWERING AND SEEDS/SIL:

# Create alternate long form data frame without survival, shoots or siliques:
vars <- c("S06Flowered","S06SeedPerSil")
NCPopLong5 <- reshape(AsterDataNCPop, varying = list(vars), direction = "long",
  timevar = "varb", times = as.factor(vars), v.names = "resp")
# Need to replace NA in non-flowering plants with zeroes:
NCPopLong5$resp <-replace(NCPopLong5$resp, is.na(NCPopLong5$resp), 0)
NCPopLong5[,c(1:5,18:20)] # Check format

# Now we can set up the model:
# NOTE: Re-run calculations of size parameters and famlist above if not already done.
#  First look at field names for both "wide" and "long" versions of data:
names(AsterDataNCPop)
names(NCPopLong3)
#  Second, add root:
NCPopLong5 <- data.frame(NCPopLong5, root=1)
#  Third, add the desired fitness measures as pseudo-covariates:
S06SeedPerSil <- grep("S06SeedPerSil", as.character(NCPopLong5$varb))
S06SeedPerSil <- is.element(seq(along = NCPopLong5$varb), S06SeedPerSil)
NCPopLong5 <- data.frame(NCPopLong5, S06SeedPerSil = as.integer(S06SeedPerSil))
#  Fourth, establish pred and families vectors:
vars  # as a reminder of the order
pred <- c(0,1)
fam <- c(1,4)
#  Check to see that the right families are specified:
sapply(famlist, as.character)[fam]

#  And, at last, the "reduced" model (NOTE:  this version actually runs):
RepOut1 <- aster(resp ~ varb + Rep, pred, fam, varb, id, root,
  data=NCPopLong5, famlist=famlist)
summary(RepOut1, show.graph = TRUE)

#  Model with Pop effects on siliques (Also runs):
RepOut2 <- aster(resp ~ varb + Rep + Pop*S06SeedPerSil - Pop,
  pred, fam, varb, id, root, data=NCPopLong5, famlist=famlist)
summary(RepOut2, show.graph = TRUE)

anova(RepOut1, RepOut2)


# ----------------------------------------------

# TESTS OF MODEL FIT:
#   NOTE:  Used this for revised version of Evolution paper in order to
#      test the relative fit of zero-truncated binomial distributions
#      vs. normal distribution w/transformed values for life-history stages.

# Negative binomial:
# NOTE:  Use NCPopLong2 dataset, RepOut2.2 model
#  Need to make sure famlist is the negative binomial version!

xi.hat <- predict(RepOut2.2, model.type = "cond", parm.type = "mean")
xi.hat <- matrix(xi.hat, nrow=nrow(RepOut2.2$x), ncol=ncol(RepOut2.2$x))

theta <- predict(RepOut2.2, model.type = "cond", parm.type = "canonical")
theta <- matrix(theta, nrow=nrow(RepOut2.2$x), ncol=ncol(RepOut2.2$x))

repout <- AsterDataNCPop$ReprodOutputZ
repoutCond <- AsterDataNCPop$ReprodOutputZ[AsterDataNCPop$S06Flowered == 1]
lrepoutCond <- length(repoutCond)

theta1 <- theta[AsterDataNCPop$S06Flowered == 1,2]
xi.hat1 <- xi.hat[AsterDataNCPop$S06Flowered == 1,2]
repgrad <- double(lrepoutCond)
repinfo <- double(lrepoutCond)
for(i in 1:lrepoutCond) {
  repgrad[i] <- famfun(famlist[[2]], deriv=1, theta1[i])
  repinfo[i] <- famfun(famlist[[2]], deriv=2, theta1[i])
}
all.equal(xi.hat1,repgrad)
pearson <- (repoutCond - xi.hat1)/sqrt(repinfo)
summary(pearson)

plot(xi.hat1,pearson, xlab="Fitted Values", ylab="Standardized residuals")
qqnorm(pearson)
qqline(pearson, lty=2)

# Transformed Normal:
# NOTE:    Use NCPopLong3 dataset, RepOut2 model
# CAUTION:  Re-uses variable names from neg. binomial fit test!
#  Need to make sure famlist is transformed normal version!

xi.hat <- predict(RepOut2, model.type = "cond", parm.type = "mean")
xi.hat <- matrix(xi.hat, nrow=nrow(RepOut2$x), ncol=ncol(RepOut2$x))

theta <- predict(RepOut2, model.type = "cond", parm.type = "canonical")
theta <- matrix(theta, nrow=nrow(RepOut2$x), ncol=ncol(RepOut2$x))

repout <- AsterDataNCPop$ReprodSqrt
repoutCond <- AsterDataNCPop$ReprodSqrt[AsterDataNCPop$S06Flowered == 1]
lrepoutCond <- length(repoutCond)

theta1 <- theta[AsterDataNCPop$S06Flowered == 1,2]
xi.hat1 <- xi.hat[AsterDataNCPop$S06Flowered == 1,2]
repgrad <- double(lrepoutCond)
repinfo <- double(lrepoutCond)
for(i in 1:lrepoutCond) {
  repgrad[i] <- famfun(famlist[[2]], deriv=1, theta1[i])
  repinfo[i] <- famfun(famlist[[2]], deriv=2, theta1[i])
}
all.equal(xi.hat1,repgrad)
pearson <- (repoutCond - xi.hat1)/sqrt(repinfo)
summary(pearson)

plot(xi.hat1,pearson, xlab="Fitted Values", ylab="Standardized residuals")
qqnorm(pearson)
qqline(pearson, lty=2)
# I think this should give percent variance explained by the conditional model (?):
(var(repoutCond)-var(xi.hat1))/var(repoutCond)

# ----------------------------------------------

# MODEL PREDICTIONS AND SE:
#  Using transformed normal results, based on results of fit comparisons
#    per above.

# Generate dummy data set:
newdata <- data.frame(Pop = levels(AsterDataNCPop$Pop))
for (v in vars) newdata[[v]] <- 1
newdata$root <- 1
# NOTE:  Assign all dummy individuals to Rep R3, which is more-or-less
#  "average" in aster rep effect.
newdata$Rep <- as.factor("R3")
levels(newdata$Rep)
newdata <- with(newdata, newdata[Pop == "N" | Pop == "S" | Pop == "F2",])

# reshape dummy data set:
renewdata <- (reshape(newdata, varying = list(vars),
		direction="long",timevar="varb",
		times=as.factor(vars),v.names="resp"))
ReprodSqrt <- grep("ReprodSqrt", as.character(renewdata$varb))
ReprodSqrt <- is.element(seq(along = renewdata$varb), ReprodSqrt)
renewdata <- data.frame(renewdata, ReprodSqrt = as.integer(ReprodSqrt))
# set up A-matrix for predictions (ReprodSqrt):
nind <- nrow(newdata)
nnode <- length(vars)
amat <- array(0,c(nind,nnode,nind))
for (i in 1:nind) amat[i, grep("ReprodSqrt", vars), i] <- 1
# Predictions (for RepOut2):
pred4 <- predict(RepOut2, varvar=varb, idvar=id, root=root,
  newdata=renewdata, se.fit=TRUE, amat=amat)
Pop <- as.factor(as.character(newdata$Pop))
Estimate <- as.numeric(pred4$fit)
Std.Error <- as.numeric(pred4$se.fit)
pred4out <- data.frame(cbind(Pop,Estimate,Std.Error))
rm(Pop,Estimate,Std.Error)
# Generate 95% confidence intervals and add to prediction data frame:
crit <- qnorm(0.975)
pred4out$Lower_Limit <- pred4out$Estimate - crit*pred4out$Std.Error
pred4out$Upper_Limit <- pred4out$Estimate + crit*pred4out$Std.Error
pred4out

# Reverse transform predictions:
pred4outRT <- data.frame(cbind(pred4out$Pop,pred4out$Estimate^2))
names(pred4outRT) <- c("Pop","Estimate")
pred4outRT$SE.lower <- (pred4out$Estimate - pred4out$Std.Error)^2
pred4outRT$SE.upper <- (pred4out$Estimate + pred4out$Std.Error)^2
pred4outRT$CI.lower <- (pred4out$Lower_Limit)^2
pred4outRT$CI.upper <- (pred4out$Upper_Limit)^2

# Plot predictions and SE:
barplot(pred4outRT$Estimate, ylim=c(0,20000), xlab="Population",
  ylab="Seed per plant, NC", names.arg=c("F2","Ma","Sp"))
errbar(c(0.7,1.9,3.1), pred4outRT$Estimate, pred4outRT$SE.upper,
  pred4outRT$SE.lower, cap=.08, add=TRUE)



# FOR SEPARATE F2 RECIPROCALS:
# NOTE:  Used for Evolution paper, Fig. 3B.

# Generate dummy data set:
newdata <- data.frame(FamFixed = levels(AsterDataNCPop$FamFixed))
for (v in vars) newdata[[v]] <- 1
newdata$root <- 1
# NOTE:  Assign all dummy individuals to Rep R3, which is more-or-less
#  "average" in aster rep effect.
newdata$Rep <- as.factor("R3")
levels(newdata$Rep)
newdata <- with(newdata, newdata[FamFixed == "S" | FamFixed == "N"
  | FamFixed == "A" | FamFixed == "B",])

# reshape dummy data set:
renewdata <- (reshape(newdata, varying = list(vars),
		direction="long",timevar="varb",
		times=as.factor(vars),v.names="resp"))
ReprodSqrt <- grep("ReprodSqrt", as.character(renewdata$varb))
ReprodSqrt <- is.element(seq(along = renewdata$varb), ReprodSqrt)
renewdata <- data.frame(renewdata, ReprodSqrt = as.integer(ReprodSqrt))
# set up A-matrix for predictions (ReprodSqrt):
nind <- nrow(newdata)
nnode <- length(vars)
amat <- array(0,c(nind,nnode,nind))
for (i in 1:nind) amat[i, grep("ReprodSqrt", vars), i] <- 1
# Predictions (for RepOut3):
pred4 <- predict(RepOut3, varvar=varb, idvar=id, root=root,
  newdata=renewdata, se.fit=TRUE, amat=amat)
FamFixed <- as.factor(as.character(newdata$FamFixed))
Estimate <- as.numeric(pred4$fit)
Std.Error <- as.numeric(pred4$se.fit)
pred4out <- data.frame(cbind(FamFixed,Estimate,Std.Error))
rm(FamFixed,Estimate,Std.Error)
# Generate 95% confidence intervals and add to prediction data frame:
crit <- qnorm(0.975)
pred4out$Lower_Limit <- pred4out$Estimate - crit*pred4out$Std.Error
pred4out$Upper_Limit <- pred4out$Estimate + crit*pred4out$Std.Error
pred4out

# Reorder pred4out:
pred4out[c(3,4,2,1),] <- pred4out[1:4,]

# Inverse transform predictions:
pred4outRT <- data.frame(cbind(pred4out$FamFixed,pred4out$Estimate^2))
names(pred4outRT) <- c("Pop","Estimate")
pred4outRT$SE.lower <- (pred4out$Estimate - pred4out$Std.Error)^2
pred4outRT$SE.upper <- (pred4out$Estimate + pred4out$Std.Error)^2
pred4outRT$CI.lower <- (pred4out$Lower_Limit)^2
pred4outRT$CI.upper <- (pred4out$Upper_Limit)^2

# Plot predictions and SE:
barplot(pred4outRT$Estimate, ylim=c(0,20000), xlab="Population",
  ylab="Seed per plant, NC", names.arg=c("Sp","Ma","SpMaF2","MaSpF2"))
errbar(c(0.7,1.9,3.1,4.3), pred4outRT$Estimate, pred4outRT$SE.upper,
  pred4outRT$SE.lower, cap=.08, add=TRUE)

