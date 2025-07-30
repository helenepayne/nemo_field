library(aster)

inb <- read.csv("inb1D.csv")
inb$row <- as.factor(inb$row)
inb$yearcross <- as.factor(inb$yearcross)
inb$flat <- as.factor(inb$flat)
str(inb)

sapply(inb, function(x) sum(is.na(x)))
goofies <- is.na(inb$cgplaid)
all(goofies == is.na(inb$row))
all(goofies == is.na(inb$posi))
sum(inb$lds3 * as.numeric(goofies))
### so these are all individuals who died before transplanting


inb12 <- read.csv("InbreedingCoreData04_21_2014.csv")
str(inb12)

inb12$row <- as.factor(inb12$row)

mean(inb12$ld2002)
mean(inb12$ld2003[inb12$ld2002 > 0])
mean(inb12$ld2004[inb12$ld2003 > 0])
mean(inb12$ld2005[inb12$ld2004 > 0])
mean(inb12$ld2006[inb12$ld2005 > 0])
mean(inb12$ld2007[inb12$ld2006 > 0])
mean(inb12$ld2008[inb12$ld2007 > 0])
mean(inb12$ld2009[inb12$ld2008 > 0])
mean(inb12$ld2010[inb12$ld2009 > 0])
mean(inb12$ld2011[inb12$ld2010 > 0])
mean(inb12$ld2012[inb12$ld2011 > 0])
mean(inb12$fl2002[inb12$ld2002 > 0])
mean(inb12$fl2003[inb12$ld2003 > 0])
mean(inb12$fl2004[inb12$ld2004 > 0])
#first flowering in 2004
mean(inb12$fl2005[inb12$ld2005 > 0])
mean(inb12$fl2006[inb12$ld2006 > 0])
mean(inb12$fl2007[inb12$ld2007 > 0])
mean(inb12$fl2008[inb12$ld2008 > 0])
mean(inb12$fl2009[inb12$ld2009 > 0])
mean(inb12$fl2010[inb12$ld2010 > 0])
mean(inb12$fl2011[inb12$ld2011 > 0])
mean(inb12$fl2012[inb12$ld2012 > 0])

mean(inb12$hdCt2004[inb12$fl2004 > 0])
#only single heads in 2004
mean(inb12$hdCt2005[inb12$fl2005 > 0])
mean(inb12$hdCt2006[inb12$fl2006 > 0])
mean(inb12$hdCt2007[inb12$fl2007 > 0])
mean(inb12$hdCt2008[inb12$fl2008 > 0])
mean(inb12$hdCt2009[inb12$fl2009 > 0])
mean(inb12$hdCt2010[inb12$fl2010 > 0])
mean(inb12$hdCt2011[inb12$fl2011 > 0])
mean(inb12$hdCt2012[inb12$fl2012 > 0])

with(inb12, any(ld2003 == 0 & ld2004 != 0))
with(inb12, any(ld2004 == 0 & ld2005 != 0))
with(inb12, any(ld2005 == 0 & ld2006 != 0))
with(inb12, any(ld2006 == 0 & ld2007 != 0))
with(inb12, any(ld2007 == 0 & ld2008 != 0))
with(inb12, any(ld2008 == 0 & ld2009 != 0))
with(inb12, any(ld2009 == 0 & ld2010 != 0))
with(inb12, any(ld2010 == 0 & ld2011 != 0))
with(inb12, any(ld2011 == 0 & ld2012 != 0))
with(inb12, any(ld2005 == 0 & fl2005 != 0))
with(inb12, any(ld2006 == 0 & fl2006 != 0))
with(inb12, any(ld2007 == 0 & fl2007 != 0))
with(inb12, any(ld2008 == 0 & fl2008 != 0))
with(inb12, any(ld2009 == 0 & fl2009 != 0))
with(inb12, any(ld2010 == 0 & fl2010 != 0))
with(inb12, any(ld2011 == 0 & fl2011 != 0))
with(inb12, any(ld2012 == 0 & fl2012 != 0))

with(inb12, any(fl2005 == 0 & hdCt2005 != 0))
with(inb12, any(fl2006 == 0 & hdCt2006 != 0))
with(inb12, any(fl2007 == 0 & hdCt2007 != 0))
with(inb12, any(fl2008 == 0 & hdCt2008 != 0))
with(inb12, any(fl2009 == 0 & hdCt2009 != 0))
with(inb12, any(fl2010 == 0 & hdCt2010 != 0))
with(inb12, any(fl2011 == 0 & hdCt2011 != 0))
with(inb12, any(fl2012 == 0 & hdCt2012 != 0))

with(inb12, any(hdCt2005 == 0 & achCt2005 != 0))
with(inb12, any(hdCt2006 == 0 & achCt2006 != 0))
with(inb12, any(hdCt2007 == 0 & achCt2007 != 0))
with(inb12, any(hdCt2008 == 0 & achCt2008 != 0))
with(inb12, any(hdCt2009 == 0 & achCt2009 != 0))
with(inb12, any(hdCt2010 == 0 & achCt2010 != 0))
with(inb12, any(hdCt2011 == 0 & achCt2011 != 0))
with(inb12, any(hdCt2012 == 0 & achCt2012 != 0))

with(inb12, any(achCt2004 < 0))
with(inb12, any(achCt2005 < 0))
with(inb12, any(achCt2006 < 0))
with(inb12, any(achCt2007 < 0))
with(inb12, any(achCt2008 < 0))
with(inb12, any(achCt2009 < 0))
with(inb12, any(achCt2010 < 0))
with(inb12, any(achCt2011 < 0))
with(inb12, any(achCt2012 < 0))

# fix up hdCt == 0 & fl > 0
inb12$fl2005[inb12$hdCt2005 == 0] <- 0
inb12$fl2006[inb12$hdCt2006 == 0] <- 0
inb12$fl2007[inb12$hdCt2007 == 0] <- 0
inb12$fl2008[inb12$hdCt2008 == 0] <- 0
inb12$fl2009[inb12$hdCt2009 == 0] <- 0
inb12$fl2010[inb12$hdCt2010 == 0] <- 0
inb12$fl2011[inb12$hdCt2011 == 0] <- 0
inb12$fl2012[inb12$hdCt2012 == 0] <- 0

#-9999 is the code for “missing data” in the achCt field for each head. If the plant had multiple heads & data were missing for one of the heads, -9999 was summed together with the good achene count(s). So far, we don’t have any instances where there is more than one head with missing achene count data per plant. So if hdCt > 1 & acheneCt < 0 adding 9999 to acheneCt will give you the acheneCt for the heads that are not missing data.

inb12$achCt2008[inb12$achCt2008<0] <- inb12$achCt2008[inb12$achCt2008<0] + 9999
with(inb12, any(achCt2008 < 0))

inb12$cgplaid <- inb12$cgPlaId
inball <- merge(inb, inb12, by =c("cgplaid"), all.x=TRUE)
dim(inball)

goofies <- is.na(inball$cgplaid)

#deal with missing values for the ones that died before transplanting

inball$ld2002[is.na(inball$cgplaid)]<-0
inball$ld2003[is.na(inball$cgplaid)]<-0
inball$ld2004[is.na(inball$cgplaid)]<-0
inball$ld2005[is.na(inball$cgplaid)]<-0
inball$ld2006[is.na(inball$cgplaid)]<-0
inball$ld2007[is.na(inball$cgplaid)]<-0
inball$ld2008[is.na(inball$cgplaid)]<-0
inball$ld2009[is.na(inball$cgplaid)]<-0
inball$ld2010[is.na(inball$cgplaid)]<-0
inball$ld2011[is.na(inball$cgplaid)]<-0
inball$ld2012[is.na(inball$cgplaid)]<-0
inball$fl2005[is.na(inball$cgplaid)]<-0
inball$fl2006[is.na(inball$cgplaid)]<-0
inball$fl2007[is.na(inball$cgplaid)]<-0
inball$fl2008[is.na(inball$cgplaid)]<-0
inball$fl2009[is.na(inball$cgplaid)]<-0
inball$fl2010[is.na(inball$cgplaid)]<-0
inball$fl2011[is.na(inball$cgplaid)]<-0
inball$fl2012[is.na(inball$cgplaid)]<-0
inball$hdCt2004[is.na(inball$cgplaid)]<-0
inball$hdCt2005[is.na(inball$cgplaid)]<-0
inball$hdCt2006[is.na(inball$cgplaid)]<-0
inball$hdCt2007[is.na(inball$cgplaid)]<-0
inball$hdCt2008[is.na(inball$cgplaid)]<-0
inball$hdCt2009[is.na(inball$cgplaid)]<-0
inball$hdCt2010[is.na(inball$cgplaid)]<-0
inball$hdCt2011[is.na(inball$cgplaid)]<-0
inball$hdCt2012[is.na(inball$cgplaid)]<-0
inball$achCt2004[is.na(inball$cgplaid)]<-0
inball$achCt2005[is.na(inball$cgplaid)]<-0
inball$achCt2006[is.na(inball$cgplaid)]<-0
inball$achCt2007[is.na(inball$cgplaid)]<-0
inball$achCt2008[is.na(inball$cgplaid)]<-0
inball$achCt2009[is.na(inball$cgplaid)]<-0
inball$achCt2010[is.na(inball$cgplaid)]<-0
inball$achCt2011[is.na(inball$cgplaid)]<-0
inball$achCt2012[is.na(inball$cgplaid)]<-0

levels(inball$row.y)
levels(inball$row.y) <- c(levels(inball$row.y),"xx")
inball$row.y[is.na(inball$cgplaid)] <- "xx"

inball$row.y[is.na(inball$cgplaid)]<-"xx"
inball$posi[is.na(inball$cgplaid)]<-0

dim(inball)
inball <- inball[inball$ld2004>0, ]
dim(inball)


#vars <- c("ld2002", "ld2003", "ld2004",  "hdCt2004", "achCt2004", "ld2005", "fl2005", "hdCt2005", "achCt2005",
#vars <- c("ld2002", "ld2003", "ld2004",                "ld2005", "fl2005", "hdCt2005", "achCt2005",
vars <- c(                                             "ld2005", "fl2005", "hdCt2005", "achCt2005",
          "ld2006", "fl2006", "hdCt2006", "achCt2006", "ld2007", "fl2007", "hdCt2007", "achCt2007",
          "ld2008", "fl2008", "hdCt2008", "achCt2008", "ld2009", "fl2009", "hdCt2009", "achCt2009",
          "ld2010", "fl2010", "hdCt2010", "achCt2010", "ld2011", "fl2011", "hdCt2011", "achCt2011",
          "ld2012", "fl2012", "hdCt2012", "achCt2012")
#vars <- c("ld2002", "ld2003", "ld2004",                "ld2005", "fl2005", "hdCt2005", "achCt2005")

redata <- reshape(inball, varying = list(vars), direction = "long",
    timevar = "varb", times = as.factor(vars), v.names = "resp")

fitsurv <- grepl("ld", as.character(redata$varb))
fitsurv <- as.numeric(fitsurv)
redata$fitsurv <- fitsurv
fitflower <- grepl("fl", as.character(redata$varb))
fitflower <- as.numeric(fitflower)
redata$fitflower <- fitflower
fithdCt <- grepl("hdCt", as.character(redata$varb))
fithdCt <- as.numeric(fithdCt)
redata$fithdCt <- fithdCt
fit <- grepl("achCt", as.character(redata$varb))
fit <- as.numeric(fit)
redata$fit <- fit
fit2005 <- grepl("achCt2005", as.character(redata$varb))
fit2005 <- as.numeric(fit2005)
redata$fit2005 <- fit2005
redata$root <- 1
levels(redata$varb)
pred <- c(0,1,2,3,1,5,6,7,5,9,10,11,9,13,14,15,13,17,18,19,17,21,22,23,21,25,26,27,25,29,30,31)
# pred <- c(0,1,2,3,4,5,6,4,8,9,10,8,12,13,14,12,16,17,18,16,20,21,22,20,24,25,26,24,28,29,30,28,32,33,34)
# pred <- c(0,1,2,3,4,5,6)

foo <- cbind(vars, c("initial", vars)[pred + 1])
colnames(foo) <- c("successor", "predecessor")
foo

fam <- rep(1, length(vars))
fam[grep("hdCt", vars)] <- 3
fam[grep("hdCt2004", vars)] <- 1
fam[grep("achCt", vars)] <- 2
foo <- cbind(foo, fam)
colnames(foo) <- c("successor", "predecessor", "family")
foo

inb1.aphids <- read.csv("echinacea.c1.inb1.aphids.csv")
inb1.aphids$aph.bf1 <- factor(inb1.aphids$aph.bf1, ordered = TRUE)
str(inb1.aphids)
ls(inb1.aphids)
year <- gsub("[a-zA-Z]", "", as.character(redata$varb))
year <- as.numeric(year)
aphids <- rep("0", nrow(redata))
! any(is.na(inb1.aphids))
for (i in 1:nrow(redata)) {
    id <- redata$cgPlaId[i]
    if (!is.na(id)) {
        yr <- year[i]
        j <- (inb1.aphids$cgPlaId == id) & (inb1.aphids$year == (yr - 1))
        if (! any(is.na(j))) {
            if (sum(j) == 1) {
                aphids[i] <- as.character(inb1.aphids$aph.bf1)[j]
            } else if (sum(j) > 1) {
                stop("more than one j")
            }
        }
    }
}
# something like
# numaphids <- double(length(aphids))
# numaphids[aphids == "bf2"] <- 56
# and the like

unique(aphids)
redata$aphids <- as.factor(aphids)

# despite the name, "wasalive" means was alive in the year preceding the current year & current year >= 2005
wasalive <- rep(FALSE, nrow(redata))
for (i in 1:nrow(redata)) {
    id <- redata$cgPlaId[i]
    if(!is.na(id)) {
        yr <- year[i]
        if (yr >= 2005) {
            j <- redata$cgPlaId == id
            j[is.na(j)] <- FALSE
            j <- j & year==(yr-1) & grepl("ld",as.character(redata$varb))
            if (sum(j) == 1) {
                wasalive[i] <- redata$resp[j] == 1
            } else if (sum(j) > 1) {
                stop("more than one j")
            }
        }
    }
}
identical(wasalive,as.character(redata$aphids)!="0")
unique(as.character(redata$aphids)[wasalive])
unique(as.character(redata$aphids)[!wasalive])
foo <- unique(redata$cgPlaId[wasalive & as.character(redata$aphids) == "0"])
foo
bar <- with(redata, data.frame(cgPlaId, varb, resp, aphids))
mycgPlaId <- redata$cgPlaId
mycgPlaId[is.na(mycgPlaId)] <- 42
bar[mycgPlaId %in% foo, ]
# so the problem is alive in 2004 but no aphid obs.
#if alive for many years after: e.g. cgPlaId = 3312, then insert a value (bf0) for aphids
redata$aphids[1475] <- "bf0"
redata$aphids[1552] <- "bf0"
redata$aphids[1600] <- "bf0"
#recheck
foo <- unique(redata$cgPlaId[wasalive & as.character(redata$aphids) == "0"])
foo
bar <- with(redata, data.frame(cgPlaId, varb, resp, aphids))
mycgPlaId <- redata$cgPlaId
mycgPlaId[is.na(mycgPlaId)] <- 42
bar[mycgPlaId %in% foo, ]

# kill the ones that are dead by 2005 anyway (ugly hack)
#redata$resp[as.character(redata$varb) == "ld2004" & mycgPlaId %in% foo] <- 0
redata$resp[1323] <- 0
redata$resp[1573] <- 0
bar <- with(redata, data.frame(cgPlaId, varb, resp, aphids))
bar[mycgPlaId %in% foo, ]



# we are good
redata$aphidobs <- as.numeric(redata$aphids != "0")
# fixup aphids
aphids <- as.character(redata$aphids)
unique(aphids)
aphids[aphids == "0"] <- "bf2"
redata$aphids <- as.factor(aphids)
# never ever use aphids except as aphidobs : aphids

# make layer
layer <- gsub("[0-9]", "", as.character(redata$varb))
unique(layer)
redata$layer <- as.factor(layer)

#make aphid loads ordered categorical
redata$bf123 <- as.numeric(aphids == "bf1" | aphids == "bf2" | aphids == "bf3")
redata$bf23 <-  as.numeric(aphids == "bf2" | aphids == "bf3")
redata$bf3 <-   as.numeric(aphids == "bf3")

#redata$newpos <- with(redata, (pos- median(pos)))
#redata$newpos[redata$newpos == is.na] <- 0.0

aout.subspace <- aster(resp ~ varb + layer : posi,
    pred, fam, varb, id, root, data = redata, method="nlm", type = "conditional")
summary(aout.subspace, info.tol = 1e-9)

#aout.space <- aster(resp ~ varb + fit : (row.y + posi),
#    pred, fam, varb, id, root, data = redata)
#summary(aout.space, info.tol = 1e-12)

#aout.subspaceyr <- aster(resp ~ varb + fit : (posi + yearcross),
#    pred, fam, varb, id, root, data = redata, method="nlm")
#summary(aout.subspaceyr, info.tol = 1e-9)
#anova(aout.subspace, aout.subspaceyr)

#aout.subspaceyr.crosstype <- aster(resp ~ varb + fit : (posi + yearcross + crosstype),
#    pred, fam, varb, id, root, data = redata, method="nlm")
#anova(aout.subspace, aout.subspaceyr, aout.subspaceyr.crosstype)
#try(summary(aout.spaceyr.crosstype, info.tol = 1e-10))

aout.subspace.crosstype <- aster(resp ~ varb + layer : (posi + crosstype),
    pred, fam, varb, id, root, data = redata, method="nlm", type = "conditional")
anova(aout.subspace, aout.subspace.crosstype)
try(summary(aout.subspace.crosstype, info.tol = 1e-10))

aout.subspace.crosstype.aphids <- aster(resp ~ varb + layer : (posi + crosstype + bf123 + bf23 + bf3),
    pred, fam, varb, id, root, data = redata, method="nlm", maxiter=5000, type = "conditional")
anova(aout.subspace, aout.subspace.crosstype, aout.subspace.crosstype.aphids)
try(summary(aout.subspace.crosstype.aphids, info.tol = 1e-10))

aout.subspace.crosstypebyaphids <- aster(resp ~ varb + layer : (posi + crosstype*(bf123 + bf23 + bf3)),
    pred, fam, varb, id, root, data = redata, method="nlm", maxiter=5000, type = "conditional")
anova(aout.subspace, aout.subspace.crosstype, aout.subspace.crosstype.aphids, aout.subspace.crosstypebyaphids)
try(summary(aout.subspace.crosstypebyaphids, info.tol = 1e-10))

newcrosstype <- with(redata, rep(levels(crosstype), times = nlevels(aphids)))
newaphids <- with(redata, rep(levels(aphids), each = nlevels(crosstype)))
newbf123 <- as.numeric(grepl("[1-3]", newaphids))
newbf23 <- as.numeric(grepl("[2-3]", newaphids))
newbf3 <- as.numeric(grepl("[3]", newaphids))

newdata <- data.frame(crosstype = newcrosstype, aphids = newaphids,
    root = 1, posi = 0, bf123 = newbf123, bf23 = newbf23, bf3 = newbf3)
for (v in vars)
    newdata[[v]] <- 1
dim(newdata)

renewdata <- reshape(newdata, varying = list(vars),
    direction = "long", timevar = "varb", times = as.factor(vars),
    v.names = "resp")
dim(renewdata)
names(renewdata)

achct <- grepl("achCt", as.character(renewdata$varb))
renewdata <- data.frame(renewdata, achct = as.integer(achct))
# it's layer not level
layer <- gsub("[0-9]", "", as.character(renewdata$varb))
renewdata <- data.frame(renewdata, layer = as.factor(layer))
# would skip amat
names(renewdata)

poutc <- predict(aout.subspace.crosstypebyaphids, varvar = varb, idvar = id,
    root = root, newdata = renewdata, model.type = "conditional",
    se.fit = TRUE, info.tol = 1e-10)
names(poutc)

nind <- nrow(newdata)
nnode <- length(vars)
length(poutc$fit) == nind * nnode

barf <- matrix(poutc$fit, nind, nnode)
bars <- matrix(poutc$se.fit, nind, nnode)

indnames <- with(newdata, paste(as.character(crosstype), as.character(aphids),
   sep = "."))
dimnames(barf) <- list(indnames, vars)
dimnames(bars) <- list(indnames, vars)

barf
bars

save(barf, file="barf.RData")
save(bars, file="bars.RData")

amat <- array(0, dim = c(nind, nnode, nind))
dim(amat)

foo <- grepl("achCt", vars)
for (k in 1:nind)
   amat[k, foo, k] <- 1

poutu <- predict(aout.subspace.crosstypebyaphids, varvar = varb, idvar = id,
    root = root, newdata = renewdata, amat = amat, se.fit = TRUE, info.tol = 1e-10)

fred <- cbind(poutu$fit, poutu$se.fit)
rownames(fred) <- indnames
colnames(fred) <- c("mu", "std. err.")
round(fred, 4)

save(fred, file="fred.RData")
