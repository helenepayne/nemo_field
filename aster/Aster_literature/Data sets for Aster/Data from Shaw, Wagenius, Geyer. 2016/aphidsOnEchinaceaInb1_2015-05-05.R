# Stuart prepared this script for Dryad repository on 5 May 2015 by combining 
# 6 scripts:

# makePlantDataFiles.R
# makeHeadDataFiles.R
# makeUsableFiles.R
# echinacea.inb1.herbivory.analysisX.R
# makeFig-a3.R
# makeTable-a.R

# The original scripts read and wrote csv files. I edited this script so it
# doesn't read and write csv files internally, but couldn't get the combined
# script to run properly. So, three temp csv files are written and then read
# around line 365 (ugh).

# The last two sections of this script read the temp R image file "fig-a.RData,"
# which is saved around line 930 of this script.

# the datasets (inbreedingsFitnessInsectsDamageTo2013.csv,
# inbreedingsHdInsectsDamage.csv, and inbreedingsPedigreeCrossLevel.csv) were
# copied from here: 
# \Dropbox\CGData\040_prepareProjectData\ruthStuartInvitedPaperData2014\
# and then I removed all records from INB2.

# the output files are two pdfs (fig 2 and fig S1) and 
# tableA.txt (see lines 1959 - 1961).


# Let me know if you have any questions! 
# Stuart Wagenius 
# stuart.wagenius@gmail.com
# http://echinaceaproject.org/


# makePlantDataFiles.R #####
#
# read plant file ####
# p <- read.csv("https://dl.dropboxusercontent.com/s/qlh0mvw0e40hmqz/inbreedingsFitnessInsectsDamageTo2013.csv")
p <- read.csv("inbreedingsFitnessInsectsDamageTo2013.csv")

# str(p)
# names(p)
# summary(p)
# table(p$measureYr)

# table(p[,  6], p$measureYr, useNA = "always") # measureDt
# table(p[,  7], p$measureYr, useNA = "always") # plaStatusDesc
# table(p[,  8], p$measureYr, useNA = "always") # basalRosetteCt     # no info '01, '02
# table(p[,  9], p$measureYr, useNA = "always") # floweringRosetteCt # no info '01, '02 (0 and NA mixed up)
# table(p[, 10], p$measureYr, useNA = "always") # basalLfCt # -999 in 2006, no data 2001 (0 and NA mixed up)
# table(p[, 11], p$measureYr, useNA = "always") # longestBasalLf some zero many NA
# table(p[, 12], p$measureYr, useNA = "always") # longestCaulineLf no data pre-2010, zeroes in 2011 & 2013 only
# table(p[, 13], p$measureYr, useNA = "always") # insects--lots of info, except 01
# table(p[, 14], p$measureYr, useNA = "always") # ant1 0, 1 all zero in 2001
# table(p[, 15], p$measureYr, useNA = "always") # ant2to10 0, 1 all zero in 2001
# table(p[, 16], p$measureYr, useNA = "always") # antGT10 0, 1 all zero in 2001
# table(p[, 17], p$measureYr, useNA = "always") # aphid1 0, 1 all zero in 2001 - 2003 & '13
# table(p[, 18], p$measureYr, useNA = "always") # aphid2to10  0, 1 all zero in 2001 - 2003
# table(p[, 19], p$measureYr, useNA = "always") # aphid11to80  0, 1 all zero in 2001 - 2003
# table(p[, 20], p$measureYr, useNA = "always") # aphidGT80  0, 1 all zero in 2001 - 2003
# table(p[, 21], p$measureYr, useNA = "always") # antNest  0, 1 all zero in 2001
# table(p[, 22], p$measureYr, useNA = "always") # insectsNote--lots of info, except 01, 02
# table(p[, 23], p$measureYr, useNA = "always") # nibbles only 2005 - '09
# table(p[, 24], p$measureYr, useNA = "always") # holes only 2005 - '09
# table(p[, 25], p$measureYr, useNA = "always") # wrinkles only 2005 - '09
# table(p[, 26], p$measureYr, useNA = "always") # rolledLf all zeroes or NAs (zeroes 2005 - '09)
# table(p[, 27], p$measureYr, useNA = "always") # totalMinorInsDam  only 2005 - '09
# table(p[, 28], p$measureYr, useNA = "always") # totalHalfGoneInsDam only 2005 - '09
# table(p[, 29], p$measureYr, useNA = "always") # totalGoneInsDam  only 2005 - '09
# table(p[, 30], p$measureYr, useNA = "always") # totalCrisp no info 01 - 04

# fix NAs ####
# unique(p[p$measureYr %in% 2003:2004, "floweringRosetteCt"])
p[p$measureYr %in% 2003:2004, "floweringRosetteCt"] <- NA

p$b.lf <- NA # define basal lf present (at least one)
p[p$plaStatusDesc %in% c("basal", "basal based on subsequent yrs data"), "b.lf"] <- TRUE
p[p$plaStatusDesc %in% c("fl based on dud & vd (includeForHdCt=0)", "flowering"), "b.lf"] <- FALSE
# p[p$basalRosetteCt %in% 1:20 & p$b.lf %in% NA, ]
# table(p$basalRosetteCt) # 
# table(p$basalLfCt) # 
p[p$basalLfCt %in% 1:180, "b.lf"] <- TRUE
# table(p$b.lf, useNA="always")

# change names in df ####

changeName <- function(old, new) names(p)[names(p) == old] <<- new 
# names(p)
changeName(old= "measureYr", new= "year")
changeName(old= "ant1", new= "b.ant.01")
changeName(old= "ant2to10", new= "b.ant.02")
changeName(old= "antGT10", new= "b.ant.11")
changeName(old= "aphid1", new= "b.aph.01")
changeName(old= "aphid2to10", new= "b.aph.02")
changeName(old= "aphid11to80", new= "b.aph.11")
changeName(old= "aphidGT80", new= "b.aph.81")
changeName(old= "antNest", new= "b.domatia")
changeName(old= "nibbles", new= "b.nibbles")
changeName(old= "holes", new= "b.holes")
changeName(old= "wrinkles", new= "b.wrinkles")
changeName(old= "rolledLf", new= "b.rolledLf")
changeName(old= "totalMinorInsDam", new= "b.insDamCt.1")
changeName(old= "totalHalfGoneInsDam", new= "b.insDamCt.2")
changeName(old= "totalGoneInsDam", new= "b.insDamCt.3")
changeName(old= "basalLfCt", new= "b.lfCt")
changeName(old= "longestBasalLf", new= "b.lfLen")

# names(p)

# plant info, no year
d1 <- unique(p[, c("cgPlaId", "row", "pos", "expNm")]                              )
              
# size info
s.cols <- c("cgPlaId", "year", 
            "plaStatusDesc", "basalRosetteCt", "floweringRosetteCt", 
            "b.lfCt", "b.lfLen", "longestCaulineLf", "b.lf")   
ds <- p[, s.cols]

#herbivory
h.cols <- c("cgPlaId", "year", "measureDt", 
            "insects", "insectsNote", 
            "b.ant.01", "b.ant.02", "b.ant.11", "b.domatia", 
            "b.aph.01", "b.aph.02", "b.aph.11", "b.aph.81", 
            "b.nibbles", "b.holes", "b.wrinkles", "b.rolledLf", 
            "b.insDamCt.1", "b.insDamCt.2", "b.insDamCt.3", "totalCrisp")
dh <- p[, h.cols]

# write.csv(d1, "echinacea.C1.inb.location.plant.csv", row.names = FALSE)
# write.csv(dh, "echinacea.C1.inb.herbivory.plant.csv", row.names = FALSE)
# write.csv(ds, "echinacea.C1.inb.size.plant.csv", row.names = FALSE)

d1.keep <- d1
dh.keep <- dh
ds.keep <- ds

rm("changeName", "d1", "dh", "ds", "h.cols", "p", "s.cols")


# makeHeadDataFiles.R #####

# read head file #####

# read dataset ####
# f <- read.csv("https://dl.dropboxusercontent.com/s/h9qqt07s9voijc4/inbreedingsHdInsectsDamage.csv")
f <- read.csv("inbreedingsHdInsectsDamage.csv")
# str(f)
# names(f)
# summary(f)
# table(f$hdmeasureYr)

# table(f[,  5], f$hdmeasureYr, useNA = "always") # no cauline lf counts in 2010, 11, 12, 13
# table(f[,  6], f$hdmeasureYr, useNA = "always") # no cauline lf measurements in 2010, 11, 12, 13
# table(f[,  7], f$hdmeasureYr, useNA = "always") # hdInsects   text
# table(f[,  8], f$hdmeasureYr, useNA = "always") # 0, 1 ant1
# table(f[,  9], f$hdmeasureYr, useNA = "always") # 0, 1 
# table(f[, 10], f$hdmeasureYr, useNA = "always") # 0, 1
# table(f[, 11], f$hdmeasureYr, useNA = "always") # 0, 1
# table(f[, 12], f$hdmeasureYr, useNA = "always") # 0, 1
# table(f[, 13], f$hdmeasureYr, useNA = "always") # 0, 1
# table(f[, 14], f$hdmeasureYr, useNA = "always") # 0, 1
# table(f[, 15], f$hdmeasureYr, useNA = "always") # 0, 1
# table(f[, 16], f$hdmeasureYr, useNA = "always") # hdInsectDamageTxt
# table(f[, 17], f$hdmeasureYr, useNA = "always") # 0, 1; NA - no nibble data 2010, 11, 12, 13 nor 2004
# table(f[, 18], f$hdmeasureYr, useNA = "always") # 0, 1; NA - no hole data 2010, 11, 12, 13 nor 2004
# table(f[, 19], f$hdmeasureYr, useNA = "always") # 0, 1; NA - no wrinkle data 2010, 11, 12, 13 nor 2004
# table(f[, 20], f$hdmeasureYr, useNA = "always") # 0, 1; NA - no rolledLf data 2010, 11, 12, 13 nor 2004
# table(f[, 21], f$hdmeasureYr, useNA = "always") # 0, 1; NA - no minor count data 2010, 11, 12, 13 nor 2004
# table(f[, 22], f$hdmeasureYr, useNA = "always") # 0, 1; NA - no major count data 2010, 11, 12, 13 nor 2004
# table(f[, 23], f$hdmeasureYr, useNA = "always") # 0, 1; NA - no gone data 2010, 11, 12, 13 nor 2004

# fix insect note columns ####
# f$hdInsectDamageTxt # no extra info (all in data columns)
# f$hdInsects # this has good info
f$hi <- as.character(f$hdInsects)
f$hi <- gsub("ant nest", "", f$hi)
f$hi <- gsub("1 ant", "", f$hi)
f$hi <- gsub("2-10 ants", "", f$hi)
f$hi <- gsub(">10 ants", "", f$hi)
f$hi <- gsub("1 aphid", "", f$hi)
f$hi <- gsub("2-10 aphids", "", f$hi)
f$hi <- gsub("11-80 aphids", "", f$hi)
f$hi <- gsub(">80 aphids", "", f$hi)
f$hi <- gsub("^;", "", f$hi)
f$hi <- gsub("^;", "", f$hi)

# unique(f$hi[f$hi != ""])
# table(f$hi, f$hdmeasureYr, useNA = "always") # all years '04 - '13, except 2010

# make six data frames ####

# 1 hh: headCount in all years ####
hh <- aggregate(cbind(cgHdId) ~ cgPlaId + hdmeasureYr , f, length)
# str(hh)  
names(hh)[2:3] <- c("year", "f.hdCt")
# table(hh$f.hdCt, hh$year)

# 2 aa: ant & aphid fields all years ####
aa <- aggregate(cbind(ant1, ant2to10, antGT10, aphid1, aphid2to10, aphid11to80, aphidGT80, antNest) ~ cgPlaId + hdmeasureYr , f, sum)
# str(aa)
# summary(aa)
# aa[aa$aphidGT80 == 2, ]
# aa[aa$cgPlaId == 2936, ]
# f[f$cgPlaId == 2936, ]
# hh[hh$cgPlaId == 2936, ]

names(aa)[2] <- "year"
names(aa)[3:5] <- c("f.ant.01", "f.ant.02", "f.ant.11") 
names(aa)[6:9] <- c("f.aph.01", "f.aph.02", "f.aph.11", "f.aph.81")
names(aa)[10] <- "f.domatia"
# names(aa)

# 3 ii: insect dam fields 2005 - 2009 ####
# names(f[17:23])
tmp <- f[f$hdmeasureYr %in% 2005:2009, ]

ii <- aggregate(cbind(nibbles, holes, wrinkles, rolledLf, totalMinorInsDam, totalHalfGoneInsDam, totalGoneInsDam) ~ cgPlaId + hdmeasureYr, tmp, sum)
# str(ii)
# summary(ii)

names(ii)[2] <- "year"
names(ii)[3:6] <- paste("f.", names(ii)[3:6], sep = "")
names(ii)[7:9] <- c("f.insDamCt.1", "f.insDamCt.2", "f.insDamCt.3")

# table(ii$f.insDamCt.1, ii$year)
# table(ii$f.insDamCt.2, ii$year)
# table(ii$f.insDamCt.3, ii$year)

# table(ii$f.nibbles, ii$year)
# table(ii$f.holes, ii$year)
# table(ii$f.wrinkles, ii$year)
# table(ii$f.rolledLf, ii$year)

rm(tmp)

# 4 cc.h: cauline lf counts (by hd) 2004 - 2009 ####
#names(f[1:6])
cc.h <- f[f$hdmeasureYr %in% 2004:2009, c(2, 3, 1, 5, 6)]
names(cc.h)[2] <- "year"
names(cc.h)[4:5] <- c("f.lfCt", "f.lfLength")

# table(cc.h$f.lfCt, cc.h$year)
# table(cc.h$f.lfLength, cc.h$year)

# 5 cc.p: cauline lf counts (by pla) 2004 - 2009 ####

foo <- aggregate(cbind(f.lfCt, f.lfLength)~ cgPlaId + year, cc.h, sum)
names(foo)[3:4] <- c("f.lfCt.sum", "f.lfLen.sum")

bah <- aggregate(f.lfLength ~ cgPlaId + year, cc.h, mean)
names(bah)[3] <- "f.lfLen.mean"

cc.p <- merge(foo, bah)
# names(cc.p)
rm(foo, bah)

# 6 otherHeadInsects ###
# names(f)
h.i <- f[, c(2, 3, 1, 24)]
names(h.i)[2] <- "year"
names(h.i)[4] <- "f.otherInsects"


# merge hh, aa, cc.p, and ii ####

# names(hh)
# names(aa)
# names(ii)   # 2005 - 2009
# names(cc.p) # 2004 - 2009

fp <- merge(hh, aa)
fp <- merge(fp, cc.p, all.x = TRUE)
fp <- merge(fp, ii, all.x = TRUE)

# str(fp)
# str(cc.h)

# save csv files

# write.csv(fp, "echinacea.C1.inb.herbivory.flRosettes.csv", row.names = FALSE)
# write.csv(cc.h, "echinacea.C1.inb.size.flRosettes.csv", row.names = FALSE)
# write.csv(h.i, "echinacea.C1.inb.otherInsects.flRosettes.csv", row.names = FALSE)

# write metadata

fp.keep <- fp
cc.h.keep <- cc.h
h.i.keep <- h.i

rm("aa", "cc.h", "cc.p", "f", "fp", "h.i", "hh", "ii")

# makeUsableFiles.R #####


##########################
# read files ####
# p <- read.csv("echinacea.C1.inb.herbivory.plant.csv")
# q <- read.csv("echinacea.C1.inb.location.plant.csv")
# r <- read.csv("echinacea.C1.inb.size.plant.csv")
# 
# s <- read.csv("echinacea.C1.inb.herbivory.flRosettes.csv")
# t <- read.csv("echinacea.C1.inb.otherInsects.flRosettes.csv")
# u <- read.csv("echinacea.C1.inb.size.flRosettes.csv")


p <- dh.keep
q <- d1.keep
r <- ds.keep

s <- fp.keep
t <- h.i.keep
u <- cc.h.keep

rm(list = ls(pattern = ".keep"))

# read pedigree data file #######

# bb <- read.csv("https://dl.dropboxusercontent.com/s/ytbgt2cpzlitq5p/inbreedingsPedigreeCrossLevel.csv")
bb <- read.csv("inbreedingsPedigreeCrossLevel.csv")

# merge files with 1 record per plant per year ####
# names(p); names(s); names(r)
mm <- merge(p, s, all.x = TRUE)
mm <- merge(mm, r)

# merge files with 1 record per head per year ####
nn <- merge(t, u, all= TRUE)

# merge location and pedigree files ####
# names(bb); names(q)
zz <- merge(bb, q[, 1:3])

# subsets for inb1 & inb2 ####
goodCgPlaIds.inb1 <- 
  bb[bb$crossLevel %in% c("B", "I", "W") &
     bb$expNm == "Inbreeding", "cgPlaId"]

goodCgPlaIds.inb2 <- 
  bb[bb$crossLevel %in% c("B", "I", "W") &
     bb$expNm != "Inbreeding", "cgPlaId"]

# output csv files for inb1 ####

d <- mm[mm$cgPlaId %in% goodCgPlaIds.inb1, ]
e <- nn[nn$cgPlaId %in% goodCgPlaIds.inb1, ]
f <- zz[zz$cgPlaId %in% goodCgPlaIds.inb1, ]
# str(d)
# str(e)
# str(f)
write.csv(d, "tempXXXechinacea.inb1.herbivory.plant.csv", row.names = FALSE)
write.csv(e, "tempXXXechinacea.inb1.herbivory.flRosette.csv", row.names = FALSE)
write.csv(f, "tempXXXechinacea.inb1.herbivory.location.csv", row.names = FALSE)


# write.csv(d, "echinacea.inb1.herbivory.plant.csv", row.names = FALSE)
# write.csv(e, "echinacea.inb1.herbivory.flRosette.csv", row.names = FALSE)
# write.csv(f, "echinacea.inb1.herbivory.location.csv", row.names = FALSE)

# rm("bb", "d", "e", "f", "goodCgPlaIds.inb1", "p", "q", "r", "s", "t", "u")

rm("bb",                "goodCgPlaIds.inb1", "p", "q", "r", "s", "t", "u")


# output csv files for inb2 ####

# g <- mm[mm$cgPlaId %in% goodCgPlaIds.inb2, ]
# h <- nn[nn$cgPlaId %in% goodCgPlaIds.inb2, ]
# i <- zz[zz$cgPlaId %in% goodCgPlaIds.inb2, ]
# 
# str(g)
# str(h)
# str(i)
# 
# write.csv(g, "echinacea.inb2.herbivory.plant.csv", row.names = FALSE)
# write.csv(h, "echinacea.inb2.herbivory.flRosette.csv", row.names = FALSE)
# write.csv(i, "echinacea.inb2.herbivory.location.csv", row.names = FALSE)
# 
# 
# rm("g", "h", "i", "mm", "nn", "zz", "goodCgPlaIds.inb2")
rm("mm", "nn", "zz", "goodCgPlaIds.inb2")


# echinacea.inb1.herbivory.analysisX.R ####
# load necessary library ####
library(MASS)

# read files ####
# bb <- read.csv("echinacea.inb1.herbivory.plant.csv")
# ff <- read.csv("echinacea.inb1.herbivory.flRosette.csv")
# pp <- read.csv("echinacea.inb1.herbivory.location.csv")

# bb <- d
# ff <- e
# pp <- f

bb <- read.csv("tempXXXechinacea.inb1.herbivory.plant.csv")
ff <- read.csv("tempXXXechinacea.inb1.herbivory.flRosette.csv")
pp <- read.csv("tempXXXechinacea.inb1.herbivory.location.csv")


# names(bb)
# names(ff)
# names(pp)

rm("d", "e", "f")

# fix
bb[bb$cgPlaId %in% c(3441, 3443), "b.lf"] <- TRUE
# problems with status "died this year"

# goal: borf analysis ####
# aph.bf ~ pi + bwi + row + pos + borf

# 1 make aph.bf: aphid abundance category for f or b

# make aph.b categories ####

# names(bb)
# table(bb$b.lfCt, bb$b.lf, useNA = "always")
# bb[bb$b.lfCt %in% "0" & bb$b.lf %in% TRUE, ]
# bb[bb$b.lfCt %in% "-999" & bb$b.lf %in% TRUE, ]

# table(bb$b.aph.01, useNA = "always")
# table(bb$b.aph.02, useNA = "always")
# table(bb$b.aph.11, useNA = "always")
# table(bb$b.aph.81, useNA = "always")

bb$aph.b <- "0"
bb[bb$b.lf %in% TRUE,  "aph.b"] <- "b00"
bb[bb$b.aph.01 %in% 1, "aph.b"] <- "b01"
bb[bb$b.aph.02 %in% 1, "aph.b"] <- "b02"
bb[bb$b.aph.11 %in% 1, "aph.b"] <- "b11"
bb[bb$b.aph.81 %in% 1, "aph.b"] <- "b81"

table(bb$aph.b, useNA = "always")

# flowering plants with no basal lvs, but aphids on basal--no records in 2005
# table(bb[bb$aph.b %in% "b01" & bb$b.lf %in% FALSE, "year"])
# table(bb[bb$aph.b %in% "b02" & bb$b.lf %in% FALSE, "year"])
# table(bb[bb$aph.b %in% "b11" & bb$b.lf %in% FALSE, "year"])
# table(bb[bb$aph.b %in% "b81" & bb$b.lf %in% FALSE, "year"])

# make aph.f categories ####

# investigate aphid abundance in fl plants

# table(bb[ , c("floweringRosetteCt", "f.hdCt")], useNA = "always")
# bb[is.na(bb$floweringRosetteCt) & bb$f.hdCt %in% 1, ] # from 2004
# bb[bb$year %in% 2004, "floweringRosetteCt"]

# names(bb)
# table(bb$floweringRosetteCt, bb$f.lfCt.sum, useNA = "always")    #ugh
# table(bb$plaStatusDesc, bb$f.lfCt.sum, useNA = "always")         #ugh
# table(bb$floweringRosetteCt, bb$plaStatusDesc, useNA = "always") #ugh
bb$aph.f0 <- "0"
bb[bb$plaStatusDesc %in% c("flowering", "fl based on dud & vd (includeForHdCt=0)"), "aph.f0"] <- "f00"
# table(bb$aph.f0, bb$plaStatusDesc, useNA = "always")      #ok
# table(bb$aph.f0, bb$f.lfCt.sum, useNA = "always")         #ok
# table(bb$aph.f0, bb$floweringRosetteCt, useNA = "always") #ok

# table(bb$f.aph.01, useNA = "always")
# table(bb$f.aph.02, useNA = "always")
# table(bb$f.aph.11, useNA = "always")
# table(bb$f.aph.81, useNA = "always")

bb[bb$f.aph.01 %in% 1, "aph.f0"] <- "f01"

bb[bb$f.aph.02 %in% 1, "aph.f0"] <- paste(bb[bb$f.aph.02 %in% 1, "aph.f0"], "f02", sep = "-")
bb[bb$f.aph.02 %in% 2, "aph.f0"] <- paste(bb[bb$f.aph.02 %in% 2, "aph.f0"], "f02f02", sep = "-")
bb[bb$f.aph.02 %in% 3, "aph.f0"] <- paste(bb[bb$f.aph.02 %in% 3, "aph.f0"], "f02f02f02", sep = "-")
bb[bb$f.aph.02 %in% 4, "aph.f0"] <- paste(bb[bb$f.aph.02 %in% 4, "aph.f0"], "f02f02f02f02", sep = "-")

bb[bb$f.aph.11 %in% 1, "aph.f0"] <- paste(bb[bb$f.aph.11 %in% 1, "aph.f0"], "f11", sep = "-")
bb[bb$f.aph.11 %in% 2, "aph.f0"] <- paste(bb[bb$f.aph.11 %in% 2, "aph.f0"], "f11f11", sep = "-")
bb[bb$f.aph.11 %in% 3, "aph.f0"] <- paste(bb[bb$f.aph.11 %in% 3, "aph.f0"], "f11f11f11", sep = "-")
bb[bb$f.aph.11 %in% 4, "aph.f0"] <- paste(bb[bb$f.aph.11 %in% 4, "aph.f0"], "f11f11f11f11", sep = "-")

bb[bb$f.aph.81 %in% 1, "aph.f0"] <- paste(bb[bb$f.aph.81 %in% 1, "aph.f0"], "f81", sep = "-")
bb[bb$f.aph.81 %in% 2, "aph.f0"] <- paste(bb[bb$f.aph.81 %in% 2, "aph.f0"], "f81f81", sep = "-")

bb$aph.f0 <- gsub("f00-", "", bb$aph.f0)


# table(bb$aph.f0, useNA = "always")
aa <- aggregate(cgPlaId ~ aph.f0, bb, length)
names(aa) <- c("old", "count")
# write.csv(aa, "reassignAphidLevels.f.csv", row.names = FALSE)

# a <- read.csv("reassignAphidLevels.f.1.csv", stringsAsFactors = FALSE)
# plot(a$max, type = "n", log = "y")
# segments(x0=1:28, y0=a$min, x1=1:28, y1=a$max)
# # a

bb$aph.f1 <- as.factor(bb$aph.f0)
bb$aph.f2 <- as.factor(bb$aph.f0)

# for(i in 2:28){
#   levels(bb$aph.f1)[levels(bb$aph.f1) == a[i, "old"]] <- a[i, "new1"]
#   levels(bb$aph.f2)[levels(bb$aph.f2) == a[i, "old"]] <- a[i, "new2"]
# }


aa$new1 <- c("", "ff0", "ff1", "ff2", "ff2", "ff2", "ff2", "ff3", "ff3", 
             "ff3", "ff3", "ff3", "ff3", "ff3", "ff3", "ff3", "ff3", "ff3", 
             "ff3", "ff3", "ff3", "ff4", "ff4", "ff4", "ff4", "ff4", "ff4", 
             "ff4")

aa$new2 <- c("", "fff0", "fff1", "fff1", "fff1", "fff1", "fff1", "fff2", 
             "fff2", "fff2", "fff2", "fff2", "fff2", "fff2", "fff2", "fff2", 
             "fff2", "fff2", "fff2", "fff2", "fff2", "fff2", "fff2", "fff2", 
             "fff2", "fff2", "fff2", "fff2")

str(aa)

for(i in 2:28){
  levels(bb$aph.f1)[levels(bb$aph.f1) == aa[i, "old"]] <- aa[i, "new1"]
  levels(bb$aph.f2)[levels(bb$aph.f2) == aa[i, "old"]] <- aa[i, "new2"]
}


# table(bb$aph.f0, bb$aph.f1)
# table(bb$aph.f0, bb$aph.f2)

# table(bb$aph.f1)
# table(bb$aph.f2)

# make aph.bf1 category based on both basal & fl aphid infestation ####

bb$aph.bf0 <- paste(bb$aph.b, bb$aph.f0)

# table(bb$aph.bf0, useNA = "always")
qq <- aggregate(cgPlaId ~ aph.bf0, bb, length)
names(qq) <- c("old", "count")
# write.csv(qq, "reassignAphidLevels.bf.csv", row.names = FALSE)

# q <- read.csv("reassignAphidLevels.bf.1.csv", stringsAsFactors = FALSE)
# plot(q$max, type = "n", log = "y")
# segments(x0=1:60, y0=q$min, x1=1:60, y1=q$max)
# # q

bb$aph.bf1 <- as.factor(bb$aph.bf0)

# for(i in 1:60){
#   levels(bb$aph.bf1)[levels(bb$aph.bf1) == q[i, "old"]] <- q[i, "new1"]
# }
# rm("i", "aa", "qq")
# dput(q[, c("old", "count", "new1")])

qq <- structure(list(
  old = c("0 0", "0 f00", "b00 0", "b00 f00", "0 f01", 
   "b00 f01", "b01 0", "b01 f00", "0 f02", "b00 f02", "b02 0", "b02 f00", 
   "b01 f02", "b02 f01", "b00 f02f02", "b02 f02", "b02 f02f02", 
   "0 f11", "b00 f11", "b11 0", "b11 f00", "b11 f01", "b02 f11", 
   "b11 f02", "b11 f01-f02", "b11 f02f02", "b11 f02f02f02f02", "b00 f11f11", 
   "b11 f11", "b11 f02-f11", "b11 f01-f02-f11", "b00 f02f02-f11f11", 
   "b11 f02f02-f11", "b11 f02f02f02-f11", "0 f11f11f11", "b11 f11f11", 
   "b11 f02-f11f11", "b00 f01-f02f02f02f02-f11f11f11", "b11 f02-f11f11f11f11", 
   "0 f81", "b81 0", "b81 f00", "b02 f81", "b81 f02", "b11 f81", 
   "b81 f11", "b81 f02-f11", "b81 f01-f02-f11", "b81 f02f02-f11", 
   "b81 f02f02f02f02-f11", "b81 f11f11", "b11 f02f02f02f02-f11-f81", 
   "b81 f02-f11f11f11", "b81 f11f11f11f11", "b81 f81", "b81 f11-f81", 
   "b11 f11-f81f81", "b81 f11f11-f81", "b81 f02f02-f11f11-f81", 
   "b81 f02f02-f11f11f11f11-f81"), 
  count = c(2225L, 81L, 2748L, 
   177L, 1L, 4L, 29L, 5L, 3L, 12L, 217L, 27L, 2L, 3L, 1L, 6L, 1L, 
   6L, 3L, 713L, 102L, 1L, 4L, 23L, 1L, 4L, 1L, 2L, 31L, 5L, 1L, 
   1L, 2L, 1L, 1L, 3L, 3L, 1L, 1L, 1L, 182L, 66L, 1L, 10L, 4L, 24L, 
   4L, 1L, 1L, 1L, 2L, 1L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
  new1 = c("0", 
    "bf0", "bf0", "bf0", "bf1", "bf1", "bf1", "bf1", "bf1", "bf1", 
    "bf1", "bf1", "bf1", "bf1", "bf1", "bf1", "bf1", "bf2", "bf2", 
    "bf2", "bf2", "bf2", "bf2", "bf2", "bf2", "bf2", "bf2", "bf2", 
    "bf2", "bf2", "bf2", "bf2", "bf2", "bf2", "bf2", "bf2", "bf2", 
    "bf2", "bf2", "bf3", "bf3", "bf3", "bf3", "bf3", "bf3", "bf3", 
    "bf3", "bf3", "bf3", "bf3", "bf3", "bf3", "bf3", "bf3", "bf3", 
    "bf3", "bf3", "bf3", "bf3", "bf3")), .Names = c("old", "count", 
    "new1"), class = "data.frame", row.names = c(NA, -60L))


for(i in 1:60){
  levels(bb$aph.bf1)[levels(bb$aph.bf1) == qq[i, "old"]] <- qq[i, "new1"]
}

rm("i", "aa", "qq")



# table(bb$aph.bf0, bb$aph.bf1)

# table(bb$aph.bf1)

# make pi predictor - infestation in previous year ####

# names(bb)

xx <- bb[, c("cgPlaId", "year", "aph.bf1")]
xx$nextYear <- xx$year + 1
names(xx)[2:4] <- c("delete", "pi", "year")
xx <- xx[, -2]
# head(xx)

bb <- merge(bb, xx, all.x = TRUE)
# names(bb)
# table(bb$pi, useNA = "always")

# bb[1:160, c("cgPlaId", "year", "aph.bf1", "pi")]
rm("xx")

# add bwi, row & pos predictors, crosstype ####
# head(pp)
pp$rowf <- as.factor(pp$row)

dd <- merge(pp, bb, all.y = TRUE)
names(dd)[names(dd) == "crossLevel"] <- "bwi"

# make borf predictor basal or flowering ####
# names(dd)
# table(dd$plaStatusDesc, dd$b.lf, useNA = "always")

# dd[dd$plaStatusDesc %in% "staple based on subsequent yrs data" & dd$b.lf %in% TRUE, ]
# dd[dd$cgPlaId %in% 3006, ]

dd$borf <- dd$b.lf
dd[dd$cgPlaId %in% 3006 & dd$year %in% 2013, "borf"] <- NA

dd$borf <- as.factor(dd$borf)
levels(dd$borf) <- c("f", "b")

dd[dd$borf %in% "b" & dd$plaStatusDesc %in% "flowering", "borf"] <- "f"
dd[dd$borf %in% "b" & dd$plaStatusDesc %in% "fl based on dud & vd (includeForHdCt=0)", "borf"] <- "f"

# table(dd$plaStatusDesc, dd$borf, useNA = "always")
# levels(dd$bwi)
dd$bwi <- relevel(dd$bwi, ref = "W")
dd$bwi <- relevel(dd$bwi, ref = "B")
levels(dd$bwi)

# trial polr model ####


# cc <- dd[dd$year %in% 2005:2012 & !is.na(dd$borf), ]
# # levels(cc$pi) <- c(levels(cc$pi), "unk") # for 2004
# # cc[cc$year %in% 2004, "pi"] <- "unk" # for 2004
# cc$pi <- cc$pi[, drop = TRUE]
# cc$aph.bf1 <- cc$aph.bf1[, drop = TRUE]
# cc$aph.bf1 <- factor(cc$aph.bf1, ordered = TRUE)
# median(cc$pos)
# cc$newPos <- cc$pos - median(cc$pos)
# # table(cc$aph.bf1, useNA = 'always')
# # table(cc$pi, cc$aph.bf1, useNA = 'always')
# # str(cc)
# # focus on 2-way interactions because with 3-way terms,
# # fitted probabilities numerically 0 or 1 occurred 
# m0 <- polr(aph.bf1 ~ (pi + bwi + rowf + newPos + borf)^2, data = cc)
# # (terms <- attributes(m0$model)$names[-1])
# # cc[, terms]
# # cc[cc$pi == "0", ]
# # bb[bb$cgPlaId %in% 3441, ]
# 
# dropterm(m0, test = "Chi")
# sig terms for different datasets
# 2005-09 -- pi:bwi
# 2005-10 -- pi:bwi, rowf:newPos
# 2005-11 -- pi:newPos
# 2005-12 -- pi:newPos, pi:borf, bwi:borf
# 2005-13 -- pi:newPos

# for the following, pi in 2004 was bf0 
# 2004-09 -- pi:bwi, pi:borf
# 2004-10 -- pi:bwi
# 2004-11 -- pi:borf
# 2004-12 -- pi:borf, bwi:borf
# 2004-13 -- newPos:borf

# for the following, pi in 2004 was unk 
# 2004-09 -- pi:bwi, pi:newPos, pi:borf, newPos:borf
# 2004-10 -- pi:bwi, pi:newPos, pi:borf, newPos:borf
# 2004-11 -- pi:newPos, pi:borf
# 2004-12 -- pi:newPos, pi:borf, bwi:borf
# 2004-13 -- pi:newPos, pi:borf

# focus on 2005-2010 dataset
cc <- dd[dd$year %in% 2005:2010 & !is.na(dd$borf), ]
cc$pi <- cc$pi[, drop = TRUE]
cc$aph.bf1 <- cc$aph.bf1[, drop = TRUE]
cc$aph.bf1 <- factor(cc$aph.bf1, ordered = TRUE)
median(cc$pos)
cc$newPos <- cc$pos - median(cc$pos)
# focus on 2-way interactions because with 3-way terms,
# fitted probabilities numerically 0 or 1 occurred 
m0 <- polr(aph.bf1 ~ (pi + bwi + rowf + newPos + borf)^2, data = cc)
dropterm(m0, test = "Chi")

m1 <- polr(aph.bf1 ~ pi * bwi + rowf * newPos + borf, data = cc, Hess = TRUE)
dropterm(m1, test = "Chi")

# summary(m1)

# 1 - pchisq(deviance(m1), df.residual(m1))

# m1.H <- update(m1, Hess = TRUE)
# m1.p <- profile(m1.H)
# summary(m1.H)
# confint(m1.p)
# plot(m1.p)
# pairs(m1.p)

# (terms <- attributes(m1$model)$names[-1])
newd <- unique(cc[, c("pi", "bwi", "rowf", "borf")])
newd$newPos <- 0
newd <- newd[newd$rowf == "11", ]

p1 <- predict(m1, newd, type = "probs")
zz <- cbind(newd, p1)
zz


# make data frame for Ruth ####
# str(dd)
inb1.aphids <- dd[dd$year %in% 2004:2013, c("cgPlaId", "year", "aph.bf1")]
table(inb1.aphids$aph.bf1, inb1.aphids$year, useNA = "always")
inb1.aphids$aph.bf1 <- inb1.aphids$aph.bf1[, drop = TRUE]
inb1.aphids$aph.bf1 <- factor(inb1.aphids$aph.bf1, ordered = TRUE)
str(inb1.aphids)
# save(inb1.aphids, file= "echinacea.c1.inb1.aphids.RData")
write.csv(inb1.aphids, file= "echinacea.c1.inb1.aphids.csv", row.names = FALSE)

# add year to previous analysis ###
library(MASS)

# try one big model ####
cc <- dd[dd$year %in% 2005:2009 & !is.na(dd$borf), ]
cc$pi <- cc$pi[, drop = TRUE]
cc$aph.bf1 <- cc$aph.bf1[, drop = TRUE]
cc$aph.bf1 <- factor(cc$aph.bf1, ordered = TRUE)
cc$newPos <- cc$pos - median(cc$pos)
cc$yearf <- as.factor(cc$year)
m0 <- polr(aph.bf1 ~ (pi + bwi + rowf + newPos + borf + yearf)^2, data = cc)
x <- dropterm(m0, test = "Chi")
z <- attr(x, "row.names")[x$"Pr(Chi)" < 0.05]
paste(z[-1], collapse = ", ")
# sig terms for different datasets
# 2005-09 -- "pi:bwi, pi:borf,             rowf:yearf, newPos:borf, newPos:yearf, borf:yearf"
# 2005-10 -- "pi:bwi,         rowf:newPos, rowf:yearf, newPos:borf, newPos:yearf, borf:yearf"
# 2005-11 -- fitted probabilities numerically 0 or 1 occurred 
# 2005-12 -- fitted probabilities numerically 0 or 1 occurred 

# try one model per year #### 
cc <- dd[dd$year %in% 2006 & !is.na(dd$borf), ]
cc$pi <- cc$pi[, drop = TRUE]
cc$aph.bf1 <- cc$aph.bf1[, drop = TRUE]
cc$aph.bf1 <- factor(cc$aph.bf1, ordered = TRUE)
cc$newPos <- cc$pos - median(cc$pos)
m0 <- polr(aph.bf1 ~ (pi + bwi + rowf + newPos + borf)^2, data = cc)

x <- dropterm(m0, test = "Chi")
z <- attr(x, "row.names")[x$"Pr(Chi)" < 0.05]
paste(z[-1], collapse = ", ")
# sig terms for different datasets
# 2005 -- fitted probabilities numerically 0 or 1 occurred 
# 2006 -- "           rowf:newPos"
# 2007 -- "           rowf:newPos"
# 2008 -- "pi:newPos, rowf:newPos, rowf:borf"
# 2009 -- fitted probabilities numerically 0 or 1 occurred 
# 2010 -- fitted probabilities numerically 0 or 1 occurred 
# 2011 -- fitted probabilities numerically 0 or 1 occurred 
# 2012 -- fitted probabilities numerically 0 or 1 occurred 
# 2013 -- fitted probabilities numerically 0 or 1 occurred 

# 2005 -- fitted probabilities numerically 0 or 1 occurred 
m0 <- polr(aph.bf1 ~ (pi + bwi + rowf + newPos)^2, data = cc)
# table(cc$borf, cc$bwi)
# table(cc$borf, cc$pi)
x <- dropterm(m0, test = "Chi")
z <- attr(x, "row.names")[x$"Pr(Chi)" < 0.05]
paste(z[-1], collapse = ", ")
# 2005 -- "rowf:newPos"

# 2009 -- fitted probabilities numerically 0 or 1 occurred 
m0 <- polr(aph.bf1 ~ (pi + bwi + rowf + newPos)^2, data = cc)
# table(cc$borf, cc$bwi)
# table(cc$borf, cc$pi)
# table(cc$pi, cc$bwi)
x <- dropterm(m0, test = "Chi")
z <- attr(x, "row.names")[x$"Pr(Chi)" < 0.05]
paste(z[-1], collapse = ", ")
# 2009 -- "pi:newPos, rowf:newPos"

# 2010 -- fitted probabilities numerically 0 or 1 occurred 
m0 <- polr(aph.bf1 ~ (pi + bwi + rowf + newPos)^2, data = cc)
# 2010 -- fitted probabilities numerically 0 or 1 occurred 
# table(cc$borf, cc$bwi)
# table(cc$borf, cc$pi)
# table(cc$pi, cc$bwi)
# table(cc$rowf, cc$bwi)
x <- dropterm(m0, test = "Chi")
z <- attr(x, "row.names")[x$"Pr(Chi)" < 0.05]
paste(z[-1], collapse = ", ")
# 2010 -- "pi:newPos, rowf:newPos"

# 2011 -- fitted probabilities numerically 0 or 1 occurred 
m0 <- polr(aph.bf1 ~ (pi + bwi + rowf + newPos)^2, data = cc)
# 2011 -- fitted probabilities numerically 0 or 1 occurred 
# table(cc$borf, cc$bwi)
# table(cc$borf, cc$pi)
# table(cc$pi, cc$bwi)
# table(cc$rowf, cc$bwi)

# select models for 2006 - 2008, 2005, 2009, 2010 ####

cc <- dd[dd$year %in% 2006 & !is.na(dd$borf), ]
cc$pi <- cc$pi[, drop = TRUE]
cc$aph.bf1 <- cc$aph.bf1[, drop = TRUE]
cc$aph.bf1 <- factor(cc$aph.bf1, ordered = TRUE)
cc$newPos <- cc$pos - median(cc$pos)
m0 <- polr(aph.bf1 ~ (pi + bwi + rowf + newPos + borf)^2, data = cc)
dropterm(m0, test = "Chi")
m1 <- polr(aph.bf1 ~ pi + bwi + rowf + newPos + borf + rowf:newPos, data = cc)
dropterm(m1, test = "Chi") # drop bwi
m2 <- polr(aph.bf1 ~ pi + rowf + newPos + borf + rowf:newPos, data = cc)
dropterm(m2, test = "Chi") # m2 is best
m.best.2006 <- m2
m.a.2006 <- polr(aph.bf1 ~ pi + rowf*newPos + borf + bwi, data = cc)
m.2.2006 <- polr(aph.bf1 ~ (pi + rowf + newPos + borf + bwi)^2, data = cc)

cc <- dd[dd$year %in% 2007 & !is.na(dd$borf), ]
cc$pi <- cc$pi[, drop = TRUE]
cc$aph.bf1 <- cc$aph.bf1[, drop = TRUE]
cc$aph.bf1 <- factor(cc$aph.bf1, ordered = TRUE)
cc$newPos <- cc$pos - median(cc$pos)
m0 <- polr(aph.bf1 ~ (pi + bwi + rowf + newPos + borf)^2, data = cc)
dropterm(m0, test = "Chi")
m1 <- polr(aph.bf1 ~ pi + bwi + rowf + newPos + borf + rowf:newPos, data = cc)
dropterm(m1, test = "Chi") # drop bwi
m2 <- polr(aph.bf1 ~ pi +  rowf + newPos + borf + rowf:newPos, data = cc)
dropterm(m2, test = "Chi") # drop borf
m3 <- polr(aph.bf1 ~ pi +  rowf + newPos + rowf:newPos, data = cc)
dropterm(m3, test = "Chi") # m3 is best
m.best.2007 <- m3
m.a.2007 <- polr(aph.bf1 ~ pi + rowf*newPos + borf + bwi, data = cc)
m.2.2007 <- polr(aph.bf1 ~ (pi + rowf + newPos + borf + bwi)^2, data = cc)



cc <- dd[dd$year %in% 2008 & !is.na(dd$borf), ]
cc$pi <- cc$pi[, drop = TRUE]
cc$aph.bf1 <- cc$aph.bf1[, drop = TRUE]
cc$aph.bf1 <- factor(cc$aph.bf1, ordered = TRUE)
cc$newPos <- cc$pos - median(cc$pos)
m0 <- polr(aph.bf1 ~ (pi + bwi + rowf + newPos + borf)^2, data = cc)
dropterm(m0, test = "Chi")
m1 <- polr(aph.bf1 ~ pi + bwi + rowf + newPos + borf + rowf:newPos + pi:newPos + rowf:borf, data = cc)
dropterm(m1, test = "Chi") # drop bwi
m2 <- polr(aph.bf1 ~ pi + rowf + newPos + borf + rowf:newPos + pi:newPos + rowf:borf, data = cc)
dropterm(m2, test = "Chi") # m2 is best
m.best.2008 <- m2
m.a.2008 <- polr(aph.bf1 ~ pi + rowf*newPos + borf + bwi, data = cc)
m.2.2008 <- polr(aph.bf1 ~ (pi + rowf + newPos + borf + bwi)^2, data = cc)

cc <- dd[dd$year %in% 2005 & !is.na(dd$borf), ]
cc$pi <- cc$pi[, drop = TRUE]
cc$aph.bf1 <- cc$aph.bf1[, drop = TRUE]
cc$aph.bf1 <- factor(cc$aph.bf1, ordered = TRUE)
cc$newPos <- cc$pos - median(cc$pos)
m0 <- polr(aph.bf1 ~ pi + bwi + rowf * newPos + borf, data = cc)
dropterm(m0, test = "Chi") # drop bwi
m1 <- polr(aph.bf1 ~ pi + rowf * newPos + borf, data = cc)
dropterm(m1, test = "Chi") #m1 is best
m.best.2005 <- m1
m.a.2005 <- polr(aph.bf1 ~ pi + rowf*newPos + borf + bwi, data = cc)
# m.2.2005 <- polr(aph.bf1 ~ (pi + rowf + newPos + borf + bwi)^2, data = cc)
m.2.2005 <- polr(aph.bf1 ~ rowf * newPos + borf * bwi +
                           pi * rowf + newPos * borf + bwi * rowf, data = cc)

cc <- dd[dd$year %in% 2009 & !is.na(dd$borf), ]
cc$pi <- cc$pi[, drop = TRUE]
cc$aph.bf1 <- cc$aph.bf1[, drop = TRUE]
cc$aph.bf1 <- factor(cc$aph.bf1, ordered = TRUE)
cc$newPos <- cc$pos - median(cc$pos)
m0 <- polr(aph.bf1 ~ pi + bwi + rowf + newPos + borf + rowf:newPos, data = cc)
dropterm(m0, test = "Chi") # keep bwi
m1 <- polr(aph.bf1 ~ pi + bwi + rowf + newPos + borf, data = cc)
dropterm(m1, test = "Chi") # drop rowf
m2 <- polr(aph.bf1 ~ pi + bwi + newPos + borf, data = cc)
dropterm(m2, test = "Chi") # m2 is best
m.best.2009 <- m2
m.a.2009 <- polr(aph.bf1 ~ pi + rowf*newPos + borf + bwi, data = cc)
m.2.2009 <- polr(aph.bf1 ~ (pi + rowf + newPos + borf + bwi)^2, data = cc) # warning msg
polr(aph.bf1 ~ (pi + rowf + newPos + borf + bwi)^2 - borf:pi, data = cc) # ok
aggregate(aph.bf1 ~ borf + pi, cc, length)

cc <- dd[dd$year %in% 2010 & !is.na(dd$borf), ]
cc$pi <- cc$pi[, drop = TRUE]
cc$aph.bf1 <- cc$aph.bf1[, drop = TRUE]
cc$aph.bf1 <- factor(cc$aph.bf1, ordered = TRUE)
cc$newPos <- cc$pos - median(cc$pos)
m0 <- polr(aph.bf1 ~ pi + bwi + rowf + newPos + borf + rowf:newPos, data = cc)
dropterm(m0, test = "Chi") # drop bwi
m1 <- polr(aph.bf1 ~ pi + rowf + newPos + borf + rowf:newPos, data = cc)
dropterm(m1, test = "Chi") # drop rowf:newPos
m2 <- polr(aph.bf1 ~ pi + rowf + newPos + borf, data = cc)
dropterm(m2, test = "Chi") # drop pi
m3 <- polr(aph.bf1 ~ rowf + newPos + borf, data = cc)
dropterm(m3, test = "Chi") # m3 is best
m.best.2010 <- m3
m.a.2010 <- polr(aph.bf1 ~ pi + rowf*newPos + borf + bwi, data = cc)
m.2.2010 <- polr(aph.bf1 ~ (pi + rowf + newPos + borf + bwi)^2, data = cc) # warning msg
polr(aph.bf1 ~ (pi + rowf + newPos + borf + bwi)^2 - pi:rowf, data = cc) # ok

cc <- dd[dd$year %in% 2004 & !is.na(dd$borf), ]
cc$pi <- cc$pi[, drop = TRUE]
cc$aph.bf1 <- cc$aph.bf1[, drop = TRUE]
cc$aph.bf1 <- factor(cc$aph.bf1, ordered = TRUE)
cc$newPos <- cc$pos - median(cc$pos)
m0 <- polr(aph.bf1 ~ bwi + rowf + newPos + borf + rowf:newPos, data = cc)
dropterm(m0, test = "Chi") # drop r:p
m1 <- polr(aph.bf1 ~ bwi + rowf + newPos + borf, data = cc)
dropterm(m1, test = "Chi") # drop bwi
m2 <- polr(aph.bf1 ~ rowf + newPos + borf, data = cc)
dropterm(m2, test = "Chi") # drop rowf
m3 <- polr(aph.bf1 ~ newPos + borf, data = cc)
dropterm(m3, test = "Chi") # m3 is best
m.best.2004 <- m3
m.a.2004 <- polr(aph.bf1 ~ rowf*newPos + borf + bwi, data = cc)
# m.2.2004 <- polr(aph.bf1 ~ (rowf + newPos + borf + bwi)^2, data = cc)


# now work on visualizing bwi effects ####

# make a good newd for predict ####
newd <- unique(dd[, c("pi", "bwi", "rowf", "borf")])
newd <- newd[!is.na(newd$borf), ]
newd[is.na(newd$pi), "borf"] <- "f"
newd[is.na(newd$pi) & newd$bwi == "I" & newd$borf == "f", "pi"] <- "bf1"
newd <- newd[!is.na(newd$pi), ]
# newd <- newd[!newd$pi %in% "0", ]
newd$pi <- newd$pi[, drop = TRUE]
newd <- newd[newd$rowf == "11", ]
newd$newPos <- 0
newd <- newd[order(newd$borf, newd$pi, newd$bwi), ]
newd <- newd[c(13:24, 1:12), ]
# newd
# str(newd)

ls()

save(newd,
  m.2.2005,
  m.2.2006, 
  m.2.2007, 
  m.2.2008, 
  m.2.2009, 
  m.2.2010, 
  m.a.2004,    
  m.a.2005,    
  m.a.2006,   
  m.a.2007,    
  m.a.2008,
  m.a.2009,
  m.a.2010,    
  m.best.2004,
  m.best.2005,
  m.best.2006,
  m.best.2007,
  m.best.2008,
  m.best.2009, 
  m.best.2010,
  zz,
  p1,
  dd, 
  file = "fig-a.RData")



#"a"           "bb"          "cc"                "ff"          
# "inb1.aphids"            "pp"          "q"           
#"x"           "z"


# do a glm with any incidence as binomial response ####

str(dd)
dd$aa1 <- 0
dd[dd$aph.bf1 %in% c("bf1", "bf2", "bf3"), "aa1"] <- 1

dd$pi.aa1 <- "none"
dd[dd$pi %in% c("bf1", "bf2", "bf3"), "pi.aa1"] <- ">0"
dd$pi.aa1 <- as.factor(dd$pi.aa1)


# try one year with pi ####
cc <- dd[dd$year %in% 2008 & !is.na(dd$borf), ]
cc$pi <- cc$pi[, drop = TRUE]
cc$newPos <- cc$pos - median(cc$pos)
m0 <- glm(aa1 ~ (pi + bwi + rowf + newPos + borf)^2, data = cc, binomial)
dropterm(m0, test = "Chi") # keep three
m1 <- glm(aa1 ~ pi + bwi + rowf + newPos + borf + pi:newPos + rowf:newPos + rowf:borf, data = cc, binomial)
dropterm(m1, test = "Chi") # drop bwi
m2 <- glm(aa1 ~ pi + rowf + newPos + borf + pi:newPos + rowf:newPos + rowf:borf, data = cc, binomial)
dropterm(m2, test = "Chi") # drop row:borf
m3 <- glm(aa1 ~ pi + rowf + newPos + borf + pi:newPos + rowf:newPos , data = cc, binomial)
dropterm(m3, test = "Chi") # m3 is best

# all other years yield warning...2005, 6, 7, 9, 10, 12
# Warning message: glm.fit: fitted probabilities numerically 0 or 1 occurred 

# try one year with pi.aa1 ####
cc <- dd[dd$year %in% 2008 & !is.na(dd$borf), ]
cc$pi.aa1 <- cc$pi.aa1[, drop = TRUE]
cc$newPos <- cc$pos - median(cc$pos)
m0 <- glm(aa1 ~ (pi.aa1 + bwi + rowf + newPos + borf)^2, data = cc, binomial)
dropterm(m0, test = "Chi") # draop all but rowf:borf & pi.aa1:newPos
m1 <- glm(aa1 ~ pi.aa1 + bwi + rowf + newPos + borf + pi.aa1:newPos + rowf:borf, data = cc, binomial)
dropterm(m1, test = "Chi") # drop bwi
m2 <- glm(aa1 ~ pi.aa1 + rowf + newPos + borf + pi.aa1:newPos + rowf:borf, data = cc, binomial)
dropterm(m2, test = "Chi") # drop pi.aa1:newPos
m3 <- glm(aa1 ~ pi.aa1 + rowf + newPos + borf + rowf:borf, data = cc, binomial)
dropterm(m3, test = "Chi") # drop newPos
m4 <- glm(aa1 ~ pi.aa1 + rowf + borf + rowf:borf, data = cc, binomial)
dropterm(m4, test = "Chi") # drop p1.aa1
m5 <- glm(aa1 ~ rowf + borf + rowf:borf, data = cc, binomial)
dropterm(m5, test = "Chi") # drop rowf:borf
m6 <- glm(aa1 ~ rowf + borf , data = cc, binomial)
dropterm(m6, test = "Chi") # drop rowf
m7 <- glm(aa1 ~ borf , data = cc, binomial)
dropterm(m7, test = "Chi") # m7 is best
glm.best.2008 <- m7
glm.best.2008.plus <- glm(aa1 ~ borf + bwi, data = cc, binomial)


cc <- dd[dd$year %in% 2007 & !is.na(dd$borf), ]
cc$pi.aa1 <- cc$pi.aa1[, drop = TRUE]
cc$newPos <- cc$pos - median(cc$pos)
m0 <- glm(aa1 ~ (pi.aa1 + bwi + rowf + newPos + borf)^2, data = cc, binomial)
dropterm(m0, test = "Chi") # drop all but rowf:newPos & pi.aa1:newPos
m1 <- glm(aa1 ~ pi.aa1 + bwi + rowf + newPos + borf + pi.aa1:newPos + rowf:newPos, data = cc, binomial)
dropterm(m1, test = "Chi") # drop borf
m2 <- glm(aa1 ~ pi.aa1 + bwi + rowf + newPos + pi.aa1:newPos + rowf:newPos, data = cc, binomial)
dropterm(m2, test = "Chi") # drop bwi
m3 <- glm(aa1 ~ pi.aa1 + rowf + newPos + pi.aa1:newPos + rowf:newPos, data = cc, binomial)
dropterm(m3, test = "Chi") # m3 is best
glm.best.2007 <- m3
glm.best.2007.plus <- update(m3, .~ . + bwi) 

cc <- dd[dd$year %in% 2006 & !is.na(dd$borf), ]
cc$pi.aa1 <- cc$pi.aa1[, drop = TRUE]
cc$newPos <- cc$pos - median(cc$pos)
m0 <- glm(aa1 ~ (pi.aa1 + bwi + rowf + newPos + borf)^2, data = cc, binomial)
dropterm(m0, test = "Chi") # warning

# years yielding warning: 2006, 9, 10, 12
# Warning message: glm.fit: fitted probabilities numerically 0 or 1 occurred 

table(cc$bwi, cc$aa1)
table(cc$bwi, cc$borf)
table(cc$bwi, cc$rowf)
table(cc$bwi, cc$pi.aa1)
table(cc$aa1, cc$bwi)
table(cc$aa1, cc$borf) ##
table(cc$aa1, cc$rowf)
table(cc$aa1, cc$pi.aa1)
table(cc$pi.aa1, cc$aa1)
table(cc$pi.aa1, cc$borf)
table(cc$pi.aa1, cc$rowf)
table(cc$borf, cc$aa1)
table(cc$borf, cc$rowf)
table(cc$borf, cc$pi.aa1)
table(cc$rowf, cc$aa1)
table(cc$rowf, cc$borf)
table(cc$rowf, cc$pi.aa1)

m0 <- glm(aa1 ~ (pi.aa1 + bwi + rowf + newPos)^2 + borf + borf:bwi , data = cc, binomial)
dropterm(m0, test = "Chi") # drop all int
m0 <- glm(aa1 ~ (pi.aa1 + bwi + rowf + newPos)^2 + borf + borf:pi.aa1 , data = cc, binomial)
dropterm(m0, test = "Chi") # drop all
m0 <- glm(aa1 ~ (pi.aa1 + bwi + rowf + newPos)^2 + borf + borf:rowf , data = cc, binomial)
dropterm(m0, test = "Chi") # keep borf:rowf
m1 <- glm(aa1 ~ pi.aa1 + bwi + rowf + newPos + borf + borf:rowf, data = cc, binomial)
dropterm(m1, test = "Chi") # drop bwi
m2 <- glm(aa1 ~ pi.aa1  + rowf + newPos + borf + borf:rowf, data = cc, binomial)
dropterm(m2, test = "Chi") # drop borf:rowf
m3 <- glm(aa1 ~ pi.aa1  + rowf + newPos + borf, data = cc, binomial)
dropterm(m3, test = "Chi") # drop rowf
m4 <- glm(aa1 ~ pi.aa1 + newPos + borf, data = cc, binomial)
dropterm(m4, test = "Chi") # m4 is best
glm.best.2006 <- m4
glm.best.2006.plus <- update(m4, .~ . + bwi) 

cc <- dd[dd$year %in% 2009 & !is.na(dd$borf), ]
cc$pi.aa1 <- cc$pi.aa1[, drop = TRUE]
cc$newPos <- cc$pos - median(cc$pos)
m0 <- glm(aa1 ~ (pi.aa1 + bwi + rowf + newPos + borf)^2, data = cc, binomial)
dropterm(m0, test = "Chi") # warning

# years yielding warning: 2006, 9, 10, 12
# Warning message: glm.fit: fitted probabilities numerically 0 or 1 occurred 

table(cc$bwi, cc$aa1)
table(cc$bwi, cc$borf)
table(cc$bwi, cc$rowf)
table(cc$bwi, cc$pi.aa1)
table(cc$aa1, cc$bwi)
table(cc$aa1, cc$borf) #####
table(cc$aa1, cc$rowf)
table(cc$aa1, cc$pi.aa1)
table(cc$pi.aa1, cc$aa1)
table(cc$pi.aa1, cc$borf)
table(cc$pi.aa1, cc$rowf)
table(cc$borf, cc$aa1)
table(cc$borf, cc$rowf)
table(cc$borf, cc$pi.aa1)
table(cc$rowf, cc$aa1)
table(cc$rowf, cc$borf)
table(cc$rowf, cc$pi.aa1)

m0 <- glm(aa1 ~ (pi.aa1 + bwi + rowf + newPos)^2 + borf + borf:bwi , data = cc, binomial)
dropterm(m0, test = "Chi") # keep pi.aa1:newPos
m0 <- glm(aa1 ~ (pi.aa1 + bwi + rowf + newPos)^2 + borf + borf:pi.aa1 , data = cc, binomial)
dropterm(m0, test = "Chi") # keep pi.aa1:newPos
m0 <- glm(aa1 ~ (pi.aa1 + bwi + rowf + newPos)^2 + borf + borf:rowf , data = cc, binomial) # warning
m0 <- glm(aa1 ~ pi.aa1 + bwi + rowf + newPos + borf + borf:pi.aa1 + pi.aa1:newPos, data = cc, binomial)
dropterm(m0, test = "Chi") # drop pi.aa1:borf
m1 <- glm(aa1 ~ pi.aa1 + bwi + rowf + newPos + borf + pi.aa1:newPos, data = cc, binomial)
dropterm(m1, test = "Chi") # drop rowf
m2 <- glm(aa1 ~ pi.aa1 + bwi + newPos + borf + pi.aa1:newPos, data = cc, binomial)
dropterm(m2, test = "Chi") # drop pi.aa1:borf
m3 <- glm(aa1 ~ pi.aa1 + bwi + newPos + borf, data = cc, binomial)
dropterm(m3, test = "Chi")  # m3 is best
glm.best.2009 <- m3
glm.best.2009.plus <- update(m3, .~ . + bwi) 

cc <- dd[dd$year %in% 2010 & !is.na(dd$borf), ]
cc$pi.aa1 <- cc$pi.aa1[, drop = TRUE]
cc$newPos <- cc$pos - median(cc$pos)
m0 <- glm(aa1 ~ (pi.aa1 + bwi + rowf + newPos + borf)^2, data = cc, binomial)
dropterm(m0, test = "Chi") # warning

# years yielding warning: 2006, 9, 10, 12
# Warning message: glm.fit: fitted probabilities numerically 0 or 1 occurred 

table(cc$bwi, cc$aa1)
table(cc$bwi, cc$borf) ####
table(cc$bwi, cc$rowf) ####
table(cc$bwi, cc$pi.aa1)
table(cc$aa1, cc$bwi)
table(cc$aa1, cc$borf) #####
table(cc$aa1, cc$rowf)
table(cc$aa1, cc$pi.aa1)
table(cc$pi.aa1, cc$aa1)
table(cc$pi.aa1, cc$borf)
table(cc$pi.aa1, cc$rowf)
table(cc$borf, cc$aa1) ####
table(cc$borf, cc$rowf)
table(cc$borf, cc$pi.aa1)
table(cc$rowf, cc$aa1)
table(cc$rowf, cc$borf)
table(cc$rowf, cc$pi.aa1)

m0 <- glm(aa1 ~ (pi.aa1 + bwi + rowf + newPos)^2 + borf + borf:bwi , data = cc, binomial)
dropterm(m0, test = "Chi") # keep pi.aa1:bwi
m0 <- glm(aa1 ~ (pi.aa1 + bwi + rowf + newPos)^2 + borf + borf:pi.aa1 , data = cc, binomial)
dropterm(m0, test = "Chi") # keep pi.aa1:bwi
m0 <- glm(aa1 ~ (pi.aa1 + bwi + rowf + newPos)^2 + borf + borf:rowf , data = cc, binomial)
dropterm(m0, test = "Chi") # keep pi.aa1:bwi
m0 <- glm(aa1 ~ (pi.aa1 + borf + rowf + newPos)^2 + bwi + borf:bwi , data = cc, binomial) # warning
m0 <- glm(aa1 ~ (pi.aa1 + borf + rowf + newPos)^2 + bwi + bwi:pi.aa1 , data = cc, binomial)
dropterm(m0, test = "Chi") # keep pi.aa1:bwi
m0 <- glm(aa1 ~ (pi.aa1 + borf + rowf + newPos)^2 + bwi + bwi:rowf , data = cc, binomial)
dropterm(m0, test = "Chi") # drop all

m0 <- glm(aa1 ~ pi.aa1 + bwi + rowf + newPos + borf + bwi:pi.aa1, data = cc, binomial)
dropterm(m0, test = "Chi") # drop newPos
m1 <- glm(aa1 ~ pi.aa1 + bwi + rowf + borf + bwi:pi.aa1, data = cc, binomial)
dropterm(m1, test = "Chi") # m1 is best
glm.best.2010 <- m1
glm.best.2010.plus <- m1 

# could try 2012

cc <- dd[dd$year %in% 2005 & !is.na(dd$borf), ]
cc$pi.aa1 <- cc$pi.aa1[, drop = TRUE]
cc$newPos <- cc$pos - median(cc$pos)
m0 <- glm(aa1 ~ (pi.aa1 + bwi + rowf + newPos + borf)^2, data = cc, binomial)
dropterm(m0, test = "Chi") # drop all but rowf:newPos & pi.aa1:newPos & rowf:borf
m1 <- glm(aa1 ~ pi.aa1 + bwi + rowf + newPos + borf + pi.aa1:newPos + rowf:newPos + rowf:borf, data = cc, binomial)
dropterm(m1, test = "Chi") # drop rowf:borf
m2 <- glm(aa1 ~ pi.aa1 + bwi + rowf + newPos + borf + pi.aa1:newPos + rowf:newPos, 
data = cc, binomial)
dropterm(m2, test = "Chi") # drop bwi
m3 <- glm(aa1 ~ pi.aa1 + rowf + newPos + borf + pi.aa1:newPos + rowf:newPos, 
data = cc, binomial)
dropterm(m3, test = "Chi") # m3 is best
glm.best.2005 <- m3
glm.best.2005.plus <- update(m3, .~ . + bwi) 

# best models ####

glm.best.2005$call
glm.best.2006$call
glm.best.2007$call
glm.best.2008$call
glm.best.2009$call
glm.best.2010$call


# make newd ####
newd.g <- newd
levels(newd.g$pi)[1] <- "none"
levels(newd.g$pi)[2:4] <- ">0"
names(newd.g)[1] <- "pi.aa1"

colz <- c("green", "green", "pink", "pink")
linz <- c(1, 2, 1, 2)
xss <- c(-0.014, -0.007, 0.007, 0.014)
  
vg <- function(model){
  yrz <- paste(deparse(substitute(model)), model$call[2], sep = "\n") 
  p1 <- predict(model, newd.g, type = "response", se.fit = TRUE)
  zz <- cbind(newd.g, p1)
  zz <- unique(zz)
  piz <- as.character(zz$pi.aa1)
  namz <- zz$bwi
  zz$bp <- as.factor(paste(zz$borf, zz$pi.aa1))
  interaction.plot(zz$bwi, zz$bp, zz$fit, ylim = 0:1, 
                   xlab = "crosslevel", ylab = "proportion infested",
                   main = yrz, cex.main = 0.75, col = colz, 
                   lwd = 2, lty = linz)
  with(zz[zz$bwi %in% "B", ], 
       segments(1 + xss, fit - se.fit, 1 + xss, fit + se.fit, 
                col = colz, lwd = 2))
  with(zz[zz$bwi %in% "W", ], 
       segments(2 + xss, fit - se.fit, 2 + xss, fit + se.fit, 
                col = colz, lwd = 2))
  with(zz[zz$bwi %in% "I", ], 
       segments(3 + xss, fit - se.fit, 3 + xss, fit + se.fit, 
                col = colz, lwd = 2))
}

# make figs #####

vg(glm.best.2005)
vg(glm.best.2006)
vg(glm.best.2007)
vg(glm.best.2008)
vg(glm.best.2009)
vg(glm.best.2010)

vg(glm.best.2005.plus)
vg(glm.best.2006.plus)
vg(glm.best.2007.plus)
vg(glm.best.2008.plus)
vg(glm.best.2009.plus)
vg(glm.best.2010.plus)



par("mar")

# postscript("viewPredicted.glm.eps", horizontal = FALSE)
# par(mfrow = c(2, 2))
# par(mar = c(5, 1, 4, 1) + 0.1)
# par(oma = c(8, 0, 8, 0))
# vg(glm.best.2005)
# vg(glm.best.2005.plus)
# vg(glm.best.2006)
# vg(glm.best.2006.plus)
# vg(glm.best.2007)
# vg(glm.best.2007.plus)
# vg(glm.best.2008)
# vg(glm.best.2008.plus)
# vg(glm.best.2009)
# vg(glm.best.2009.plus)
# vg(glm.best.2010)
# vg(glm.best.2010.plus)
# dev.off()

par(oma = c(0, 0, 0, 0))
par(mar = c(5, 4, 4, 2) + 0.1)

# one big model with year ####
cc <- dd[dd$year %in% 2005:2009 & !is.na(dd$borf), ]
cc$pi.aa1 <- cc$pi.aa1[, drop = TRUE]
cc$newPos <- cc$pos - median(cc$pos)
m0 <- glm(aa1 ~ (year + pi.aa1 + bwi + rowf + newPos + borf)^2, data = cc, binomial)
(x <- dropterm(m0, test = "Chi")) # drop all but year:pi.aa1, year:borf

m1 <- glm(aa1 ~ year + pi.aa1 + bwi + rowf + newPos + borf + year:pi.aa1 + year:borf, data = cc, binomial)
(x <- dropterm(m1, test = "Chi")) # drop bwi

m2 <- glm(aa1 ~ year + pi.aa1 + rowf + newPos + borf + year:pi.aa1 + year:borf, data = cc, binomial)
(x <- dropterm(m2, test = "Chi")) # drop rowf

m3 <- glm(aa1 ~ year + pi.aa1 + newPos + borf + year:pi.aa1 + year:borf, data = cc, binomial)
(x <- dropterm(m3, test = "Chi")) # m3 is best

# handy line of code...
z <- attr(x, "row.names")[x$"Pr(Chi)" < 0.05]; paste(z[-1], collapse = ", ") 


# one big model with year as factor ####
cc <- dd[dd$year %in% 2005:2009 & !is.na(dd$borf), ]
cc$pi.aa1 <- cc$pi.aa1[, drop = TRUE]
cc$newPos <- cc$pos - median(cc$pos)
cc$yearf <- as.factor(cc$year)

m0 <- glm(aa1 ~ (yearf + pi.aa1 + bwi + rowf + newPos + borf)^2, data = cc, binomial)
(x <- dropterm(m0, test = "Chi")) # drop all but yearf:rowf, yearf:newPos, yearf:borf, yearf:bwi

m1 <- glm(aa1 ~ yearf + pi.aa1 + bwi + rowf + newPos + borf + yearf:rowf + yearf:newPos + yearf:borf + yearf:bwi, data = cc, binomial)
(x <- dropterm(m1, test = "Chi")) # drop yearf:bwi

m2 <- glm(aa1 ~ yearf + pi.aa1 + bwi + rowf + newPos + borf + yearf:rowf + yearf:newPos + yearf:borf, data = cc, binomial)
(x <- dropterm(m2, test = "Chi")) # drop bwi

m3 <- glm(aa1 ~ yearf + pi.aa1 + rowf + newPos + borf + yearf:rowf + yearf:newPos + yearf:borf, data = cc, binomial)
(x <- dropterm(m3, test = "Chi")) # m3 is best

big.glm <- m3
big.glm.plus <- m1

big.glm$call
big.glm.plus$call

m4 <- glm(aa1 ~ (yearf + pi.aa1 + bwi)^2, data = cc, binomial)
(x <- dropterm(m4, test = "Chi")) # no evidence of bwi effect



newd.big <- newd.g
newd.big$year <- 2005
bn6 <- newd.big; bn6$year <- 2006
bn7 <- newd.big; bn7$year <- 2007
bn8 <- newd.big; bn8$year <- 2008
bn9 <- newd.big; bn9$year <- 2009
newd.big <- rbind(newd.big, bn6, bn7, bn8, bn9)
newd.big$yearf <- as.factor(newd.big$year)
levels(newd.big$yearf)

colz <- c("green", "green", "pink", "pink")
linz <- c(1, 2, 1, 2)
xss <- c(-0.014, -0.007, 0.007, 0.014)
  
model <- big.glm

  yrz <- paste(deparse(substitute(model)), model$call[2], sep = "\n") 
  p1 <- predict(model, newd.big, type = "response", se.fit = TRUE)
  zz <- cbind(newd.big, p1)
  zz <- unique(zz)
  piz <- as.character(zz$pi.aa1)
  namz <- zz$bwi
  zz$bp <- as.factor(paste(zz$borf, zz$pi.aa1))
  
  plot(fit ~ year, zz, ylim = 0:1, 
                   xlab = "year", ylab = "proportion infested",
                   main = yrz, cex.main = 0.75)
  points(fit ~ year, zz[zz$bp %in% "f >0", ], type = "b", lwd = 2,
         lty = linz[3], col = colz[3])
  points(fit ~ year, zz[zz$bp %in% "f none", ], type = "b", lwd = 2,
         lty = linz[4], col = colz[4])
  points(fit ~ year, zz[zz$bp %in% "b >0", ], type = "b", lwd = 2,
         lty = linz[1], col = colz[1])
  points(fit ~ year, zz[zz$bp %in% "b none", ], type = "b", lwd = 2,
         lty = linz[2], col = colz[2])
  

model <- big.glm.plus

  yrz <- paste(deparse(substitute(model)), model$call[2], sep = "\n") 
  p1 <- predict(model, newd.big, type = "response", se.fit = TRUE)
  zz <- cbind(newd.big, p1)
  zz <- unique(zz)
  piz <- as.character(zz$pi.aa1)
  namz <- zz$bwi
  zz$bp <- as.factor(paste(zz$borf, zz$pi.aa1))
  
  plot(fit ~ year, zz, ylim = 0:1, 
                   xlab = "year", ylab = "proportion infested",
                   main = yrz, cex.main = 0.75)

zz.i <- zz[zz$bwi %in% "I", ]

  points(fit ~ year, zz.i[zz.i$bp %in% "f >0", ], type = "b", lwd = 2,
         lty = linz[3], col = "red")
  points(fit ~ year, zz.i[zz.i$bp %in% "f none", ], type = "b", lwd = 2,
         lty = linz[4], col = "red")
  points(fit ~ year, zz.i[zz.i$bp %in% "b >0", ], type = "b", lwd = 2,
         lty = linz[1], col = "red")
  points(fit ~ year, zz.i[zz.i$bp %in% "b none", ], type = "b", lwd = 2,
         lty = linz[2], col = "red")

zz.b <- zz[zz$bwi %in% "B", ]

points(fit ~ year, zz.b[zz.b$bp %in% "f >0", ], type = "b", lwd = 2,
       lty = linz[3], col = "black")
points(fit ~ year, zz.b[zz.b$bp %in% "f none", ], type = "b", lwd = 2,
       lty = linz[4], col = "black")
points(fit ~ year, zz.b[zz.b$bp %in% "b >0", ], type = "b", lwd = 2,
       lty = linz[1], col = "black")
points(fit ~ year, zz.b[zz.b$bp %in% "b none", ], type = "b", lwd = 2,
       lty = linz[2], col = "black")

zz.w <- zz[zz$bwi %in% "W", ]

points(fit ~ year, zz.w[zz.w$bp %in% "f >0", ], type = "b", lwd = 2,
       lty = linz[3], col = "gray")
points(fit ~ year, zz.w[zz.w$bp %in% "f none", ], type = "b", lwd = 2,
       lty = linz[4], col = "gray")
points(fit ~ year, zz.w[zz.w$bp %in% "b >0", ], type = "b", lwd = 2,
       lty = linz[1], col = "gray")
points(fit ~ year, zz.w[zz.w$bp %in% "b none", ], type = "b", lwd = 2,
       lty = linz[2], col = "gray")

# save.image("echinacea.inb1.herbivory.analysis2014-05-02.RData")

# makeFig-a3.R ####
#############################
rm(list=ls(all=TRUE))

load("fig-a.RData") # objects saved from line 513 in 
                    # echinacea.inb1.herbivory.analysis2014-07-17.R
library(MASS)

# review best models & models for comparing years ####
# str(m.best.2005)

# m.best.2005[["call"]]
m.best.2005$call
m.best.2006$call
m.best.2007$call
m.best.2008$call # pi X pos, row X flowering
m.best.2009$call
m.best.2010$call
m.best.2004$call

m.a.2005$call
m.a.2006$call
m.a.2007$call
m.a.2008$call
m.a.2009$call
m.a.2010$call

m.2.2005
m.2.2006
m.2.2007
m.2.2008
m.2.2009 # prob with borf:pi
m.2.2010 # prob with pi:rowf

# infestation the previous year, row, pos, row X pos, flowering status, cross type. 

# make model for comparing among years ####

makeComp <- function(year){
  cc <- dd[dd$year %in% year & !is.na(dd$borf), ]
  cc$pi <- cc$pi[, drop = TRUE]
  cc$aph.bf1 <- cc$aph.bf1[, drop = TRUE]
  cc$aph.bf1 <- factor(cc$aph.bf1, ordered = TRUE)
  cc$newPos <- cc$pos - median(cc$pos)
  polr(aph.bf1 ~ pi + bwi + rowf + newPos + borf + 
         pi:newPos + borf:rowf + rowf:newPos + newPos:borf, data = cc)
}

m.comp.2005 <- makeComp(2005)
m.comp.2006 <- makeComp(2006)
m.comp.2007 <- makeComp(2007)
m.comp.2008 <- makeComp(2008)
m.comp.2009 <- makeComp(2009)
m.comp.2010 <- makeComp(2010)

# sample sizes #### 
table(m.comp.2005$model$bwi)
table(m.comp.2010$model$bwi)

rm(zz, p1)

b05 <- c("2005", "pl + fs*x + x*y")
b06 <- c("2006", "pl + fs + x*y")
b07 <- c("2007", "        pl + x*y")
b08 <- c("2008", "pl + pl*y + fs*y + x*y")
b09 <- c("2009", "gc + pl*y + fs*y + x*y")
b10 <- c("2010", "fs + x + y")
b04 <- c("2004", "fs + y")


colz <- c("#BAE4B3", "#74C476", "#31A354", "#006D2C")[4:1]
spaz <- rep(c(0.5, 0.22, 0.22), 8)

# revise function ####
vv <- function(model, incLegend = TRUE, subb = TRUE, bpax = TRUE,
               mtcex = 0.8, yrt = c("2005", "bm")){
  p1 <- predict(model, newd, type = "probs")
  zz <- cbind(newd, p1)
  bz <- t(as.matrix(zz[, 9:6]))
#   infz <- rownames(bz)[4:1]
  infz <- c("none", "low", "mid", "high")
  piz <- as.character(zz$pi)
  namz <- zz$bwi
  lt <- if(incLegend) infz[4:1] else NULL  
#  if(bpax) 
  bp <- barplot(bz, col = colz, space = spaz, names.arg = rep("", 24),
                axes = bpax, cex.axis = 1.2, legend.text = lt, 
                args.legend = list(x = 9, y = 0.9, title = "Aphid-load",
                                   bg = "white", cex = 1),
                border = FALSE)
  text(3, 0.95, yrt[1], cex = 1.2) 
  title(main = yrt[2], cex.main = 1, font.main = 3, line = -1.2)
  if(subb){
    mtext(side = 1, at = bp, line = 0, text = namz, cex = mtcex)
    mtext(side = 1, at = tapply(bp, rep(1:8, each = 3), mean), line = 1.25,
          text = rep(infz, 2), cex = mtcex)
    mtext(side = 1, at = c(mean(bp[1:12]), mean(bp[13:24])), line = 2.5,
          text = c("Basal", "Flowering"), cex = mtcex)
  }
}

par(mfcol = c(1, 1))
vv(m.a.2008, incLegend = TRUE, subb = FALSE)
vv(m.a.2008, incLegend = TRUE, subb = TRUE, bpax = FALSE)

# pdf("fig.a.comp.pdf", width = 7.5, height = 5)
fname <- paste("fig.a.comp", format(Sys.time(), "%H-%M-%S"), "-AM.pdf", sep = "")
pdf(fname, width = 7.5, height = 5)
par(mfrow = c(2, 3))
par(las = 1)
par(oma = c(5.3, 3.7, 0.25, 0.25))
par(mar = c(0, 0.4, 0.7, 0))
vv(m.comp.2005, yrt = b05, incLegend = FALSE, subb = FALSE, bpax = TRUE)
vv(m.comp.2006, yrt = b06, incLegend = FALSE, subb = FALSE, bpax = FALSE)
vv(m.comp.2007, yrt = b07, incLegend = FALSE, subb = FALSE, bpax = FALSE)
vv(m.comp.2008, yrt = b08, incLegend = TRUE, subb = TRUE, bpax = TRUE, mtcex = 0.7)
vv(m.comp.2009, yrt = b09, incLegend = FALSE, subb = TRUE, bpax = FALSE, mtcex = 0.7)
vv(m.comp.2010, yrt = b10, incLegend = FALSE, subb = TRUE, bpax = FALSE, mtcex = 0.7)
mtext("Proportion of plants with each aphid-load level", side = 2, outer = TRUE, las = 0, line = 2.2)
mtext(text = "Genotypic class, prior load, and flowering status", 
      side = 1, outer = TRUE, line = 4.2)
dev.off()


# pdf("fig.a.best.pdf", width = 7.5, height = 5)
fname2 <- paste("fig.a.best", format(Sys.time(), "%H-%M-%S"), "-AM.pdf", sep = "")
pdf(fname2, width = 7.5, height = 5)
par(mfrow = c(2, 3))
par(las = 1)
par(oma = c(5.3, 3.7, 0.25, 0.25))
par(mar = c(0, 0.4, 0.7, 0))
vv(m.best.2005, yrt = b05, incLegend = FALSE, subb = FALSE, bpax = TRUE)
vv(m.best.2006, yrt = b06, incLegend = FALSE, subb = FALSE, bpax = FALSE)
vv(m.best.2007, yrt = b07, incLegend = FALSE, subb = FALSE, bpax = FALSE)
vv(m.best.2008, yrt = b08, incLegend = TRUE, subb = TRUE, bpax = TRUE, mtcex = 0.7)
vv(m.best.2009, yrt = b09, incLegend = FALSE, subb = TRUE, bpax = FALSE, mtcex = 0.7)
vv(m.best.2010, yrt = b10, incLegend = FALSE, subb = TRUE, bpax = FALSE, mtcex = 0.7)
mtext("Proportion of plants with each aphid-load level", side = 2, outer = TRUE, las = 0, line = 2.2)
mtext(text = "Genotypic class, prior load, and flowering status", 
      side = 1, outer = TRUE, line = 4.2)
dev.off()


# makeTable-a.R #####
rm(list=ls(all=TRUE))

load("fig-a.RData") # objects saved from line 513 in 
# echinacea.inb1.herbivory.analysis2014-07-17.R
library(MASS)

# summarize models by year ####

smriz <- function(year){
  cc <- dd[dd$year %in% year & !is.na(dd$borf), ]
  cc$pi <- cc$pi[, drop = TRUE]
  cc$aph.bf1 <- cc$aph.bf1[, drop = TRUE]
  cc$aph.bf1 <- factor(cc$aph.bf1, ordered = TRUE)
  cc$newPos <- cc$pos - median(cc$pos)
  m0 <- polr(aph.bf1 ~ pi + bwi + rowf + newPos + borf + 
                       pi:newPos + borf:rowf + row:newPos + newPos:borf, data = cc)
  dropterm(m0, test = "Chi")
}

round(smriz(2005)$"Pr(Chi)", 3)

mat <- matrix(0, nrow = 6, ncol = 6)
for(i in 2005:2010){
  mat[i-2004, ] <- round(smriz(i)$"Pr(Chi)", 3)
}
rownames(mat) <- 2005:2010
colnames(mat) <- row.names(smriz(2010))
mat

# quick exam of crosstype main effect w/o plant size ####

justXT <- function(year){
  cc <- dd[dd$year %in% year & !is.na(dd$borf), ]
  cc$pi <- cc$pi[, drop = TRUE]
  cc$aph.bf1 <- cc$aph.bf1[, drop = TRUE]
  cc$aph.bf1 <- factor(cc$aph.bf1, ordered = TRUE)
  cc$newPos <- cc$pos - median(cc$pos)
  m0 <- polr(aph.bf1 ~ bwi + rowf * newPos, data = cc)
#  m0 <- polr(aph.bf1 ~ bwi , data = cc)
  dropterm(m0, test = "Chi")
}

justXT(2004)
justXT(2005)
justXT(2006)
justXT(2007)
justXT(2008)
justXT(2009)
justXT(2010)

# year <- 2009
# cc <- dd[dd$year %in% year & !is.na(dd$borf), ]
# cc$pi <- cc$pi[, drop = TRUE]
# cc$aph.bf1 <- cc$aph.bf1[, drop = TRUE]
# cc$aph.bf1 <- factor(cc$aph.bf1, ordered = TRUE)
# cc$newPos <- cc$pos - median(cc$pos)
# polr(aph.bf1 ~ bwi + rowf * newPos, data = cc)

# simplify factor names ####

names(dd)[names(dd) == "pi"] <- "pl"
names(dd)[names(dd) == "rowf"] <- "x"
names(dd)[names(dd) == "borf"] <- "fs"
names(dd)[names(dd) == "newPos"] <- "y"
names(dd)[names(dd) == "bwi"] <- "gc"

#  model selex function ####


startM <- function(year){
  cc <- dd[dd$year %in% year & !is.na(dd$fs), ]
  cc$pl <- cc$pl[, drop = TRUE]
  cc$aph.bf1 <- cc$aph.bf1[, drop = TRUE]
  cc$aph.bf1 <- factor(cc$aph.bf1, ordered = TRUE)
  cc$y <- cc$pos - median(cc$pos)
  model <- polr(aph.bf1 ~ pl + gc + x + y + fs + 
                pl:y + fs:x + x:y + y:fs, data = cc)
  ans <- list(m = model, df = cc)
  return(ans)
}

# start model selex here ####
getOption("digits")
options(scipen = 100)
options(digits = 3)

# year 2005 ####
rm(list = ls(pattern = "^mm")); rm(list = letters[1:4])
mm <- startM(2005)
# mm$m
dropterm(mm$m, test = "Chi")
mm.gc <- polr(aph.bf1 ~ pl +  x + y + fs + 
                pl:y + fs:x + x:y + y:fs, data = mm$df)
anova(mm$m, mm.gc, test = "Chi")
dropterm(mm.gc, test = "Chi")

mm.gc.ply <- polr(aph.bf1 ~ pl +  x + y + fs + 
                fs:x + x:y + y:fs, data = mm$df)
anova(mm.gc, mm.gc.ply, test = "Chi")
dropterm(mm.gc.ply, test = "Chi")

mm.gc.ply.yfs <- polr(aph.bf1 ~ pl +  x + y + fs + 
                   fs:x + x:y, data = mm$df) ## best
anova(mm.gc.ply, mm.gc.ply.yfs, test = "Chi")
dropterm(mm.gc.ply.yfs, test = "Chi")

(a <- anova(mm.gc.ply.yfs, 
            mm.gc.ply, 
            mm.gc, 
            mm$m, test = "Chi"))

mm.gc.ply.yfs.xy <- polr(aph.bf1 ~ pl +  x + y + fs + 
                        fs:x , data = mm$df)
mm.gc.ply.yfs.xfs <- polr(aph.bf1 ~ pl +  x + y + fs + 
                          x:y, data = mm$df)
mm.gc.ply.yfs.pl <- polr(aph.bf1 ~  x + y + fs + 
                           fs:x + x:y, data = mm$df)
(b <- anova(mm.gc.ply.yfs.xy , mm.gc.ply.yfs, test = "Chi"))
(c <- anova(mm.gc.ply.yfs.xfs, mm.gc.ply.yfs, test = "Chi"))
(d <- anova(mm.gc.ply.yfs.pl , mm.gc.ply.yfs, test = "Chi"))
(a.2005 <- rbind(a, b, c, d))
mm.gc.ply.yfs$call # best

# year 2006 ####
rm(list = ls(pattern = "^mm")); rm(list = letters[1:4])
mm <- startM(2006)
mm$m
dropterm(mm$m, test = "Chi")
mm.yfs <- polr(aph.bf1 ~ gc + pl +  x + y + fs + pl:y + fs:x + x:y , data = mm$df)
anova(mm$m, mm.yfs, test = "Chi")
dropterm(mm.yfs, test = "Chi")

mm.yfs.gc <- polr(aph.bf1 ~ pl +  x + y + fs + fs:x + x:y + pl:y, data = mm$df)
anova(mm.yfs, mm.yfs.gc, test = "Chi")
dropterm(mm.yfs.gc, test = "Chi")

mm.yfs.gc.ply <- polr(aph.bf1 ~ pl + x + y + fs + fs:x + x:y, data = mm$df)
anova(mm.yfs.gc, mm.yfs.gc.ply, test = "Chi")
dropterm(mm.yfs.gc.ply, test = "Chi")

mm.yfs.gc.ply.xfs <- polr(aph.bf1 ~ pl +  x + y + fs + x:y, data = mm$df) # best
anova(mm.yfs.gc.ply, mm.yfs.gc.ply.xfs, test = "Chi")
dropterm(mm.yfs.gc.ply.xfs, test = "Chi")

(a <- anova(mm.yfs.gc.ply.xfs,
            mm.yfs.gc.ply,
            mm.yfs.gc, 
            mm.yfs, 
            mm$m, test = "Chi"))

mm.yfs.gc.ply.xfs.pl <- polr(aph.bf1 ~ x + y + fs + x:y, data = mm$df)
mm.yfs.gc.ply.xfs.fs <- polr(aph.bf1 ~ pl +  x + y + x:y, data = mm$df)
mm.yfs.gc.ply.xfs.xy <- polr(aph.bf1 ~ pl +  x + y + fs, data = mm$df) 

(b <- anova(mm.yfs.gc.ply.xfs.pl , mm.yfs.gc.ply.xfs, test = "Chi"))
(c <- anova(mm.yfs.gc.ply.xfs.fs , mm.yfs.gc.ply.xfs, test = "Chi"))
(d <- anova(mm.yfs.gc.ply.xfs.xy , mm.yfs.gc.ply.xfs, test = "Chi"))
(a.2006 <- rbind(a, b, c, d))
mm.yfs.gc.ply.xfs$call # best

# year 2007 ####
rm(list = ls(pattern = "^mm")); rm(list = letters[1:4])
mm <- startM(2007)
mm$m
dropterm(mm$m, test = "Chi")

mm.gc <- polr(aph.bf1 ~ pl +  x + y + fs + 
                pl:y + fs:x + x:y + y:fs, data = mm$df)
anova(mm$m, mm.gc, test = "Chi")
dropterm(mm.gc, test = "Chi")

mm.gc.ply <- polr(aph.bf1 ~ pl +  x + y + fs + 
                    fs:x + x:y + y:fs, data = mm$df)
anova(mm.gc, mm.gc.ply, test = "Chi")
dropterm(mm.gc.ply, test = "Chi")

mm.gc.ply.xfs <- polr(aph.bf1 ~ pl +  x + y + fs + 
                        y:fs + x:y, data = mm$df)
anova(mm.gc.ply, mm.gc.ply.xfs, test = "Chi")
dropterm(mm.gc.ply.xfs, test = "Chi")

mm.gc.ply.xfs.yfs <- polr(aph.bf1 ~ pl +  x + y + fs + 
                          x:y, data = mm$df)
anova(mm.gc.ply.xfs, mm.gc.ply.xfs.yfs, test = "Chi")
dropterm(mm.gc.ply.xfs.yfs, test = "Chi")

mm.gc.ply.xfs.yfs.fs <- polr(aph.bf1 ~ pl +  x + y + 
                            x:y, data = mm$df) # best
anova(mm.gc.ply.xfs.yfs, mm.gc.ply.xfs.yfs.fs, test = "Chi")
dropterm(mm.gc.ply.xfs.yfs.fs, test = "Chi")

(a <- anova(mm.gc.ply.xfs.yfs.fs, 
            mm.gc.ply.xfs.yfs, 
            mm.gc.ply.xfs, 
            mm.gc.ply, 
            mm.gc, 
            mm$m, test = "Chi"))

mm.gc.ply.xfs.yfs.fs.pl <- polr(aph.bf1 ~       x + y + x:y, data = mm$df)
mm.gc.ply.xfs.yfs.fs.xy <- polr(aph.bf1 ~ pl +  x + y      , data = mm$df)

(b <- anova(mm.gc.ply.xfs.yfs.fs , mm.gc.ply.xfs.yfs.fs.pl, test = "Chi"))
(c <- anova(mm.gc.ply.xfs.yfs.fs , mm.gc.ply.xfs.yfs.fs.xy, test = "Chi"))
(a.2007 <- rbind(a, b, c))
mm.gc.ply.xfs.yfs.fs$call # best

# year 2008 ####
rm(list = ls(pattern = "^mm")); rm(list = letters[1:4])
mm <- startM(2008)
mm$m
dropterm(mm$m, test = "Chi")

mm.gc <- polr(aph.bf1 ~ pl +  x + y + fs + 
                pl:y + fs:x + x:y + y:fs, data = mm$df)
anova(mm$m, mm.gc, test = "Chi")
dropterm(mm.gc, test = "Chi")

mm.gc.xfs <- polr(aph.bf1 ~ pl +  x + y + pl:y + 
                    fs + x:y + y:fs, data = mm$df) # best
anova(mm.gc, mm.gc.xfs, test = "Chi")
dropterm(mm.gc.xfs, test = "Chi")

(a <- anova(mm.gc.xfs, 
            mm.gc, 
            mm$m, test = "Chi"))

mm.gc.xfs.ply <- polr(aph.bf1 ~ pl +  x + y +  
                        fs + x:y + y:fs, data = mm$df)
mm.gc.xfs.xy <- polr(aph.bf1 ~ pl +  x + y + pl:y + 
                        fs +       y:fs, data = mm$df)
mm.gc.xfs.yfs <- polr(aph.bf1 ~ pl +  x + y + pl:y + 
                        fs + x:y       , data = mm$df)

(b <- anova(mm.gc.xfs, mm.gc.xfs.ply, test = "Chi"))
(c <- anova(mm.gc.xfs, mm.gc.xfs.xy, test = "Chi"))
(d <- anova(mm.gc.xfs, mm.gc.xfs.yfs, test = "Chi"))
(a.2008 <- rbind(a, b, c, d))
mm.gc.xfs$call # best

# year 2009 ####
rm(list = ls(pattern = "^mm")); rm(list = letters[1:5])
mm <- startM(2009)
mm$m
dropterm(mm$m, test = "Chi")
mm.xfs <- polr(aph.bf1 ~ gc + pl +  x + y + fs + 
                pl:y + x:y + y:fs, data = mm$df) # best
anova(mm$m, mm.xfs, test = "Chi")
dropterm(mm.xfs, test = "Chi")

(a <- anova(mm.xfs, 
            mm$m, test = "Chi"))

mm.xfs.gc <- polr(aph.bf1 ~       pl +  x + y + fs + 
                 pl:y + x:y + y:fs, data = mm$df)
mm.xfs.ply <- polr(aph.bf1 ~ gc + pl +  x + y + fs + 
                        x:y + y:fs, data = mm$df)
mm.xfs.xy <- polr(aph.bf1 ~ gc + pl +  x + y + fs + 
                 pl:y +       y:fs, data = mm$df)
mm.xfs.yfs <- polr(aph.bf1 ~ gc + pl +  x + y + fs + 
                 pl:y + x:y       , data = mm$df)
(b <- anova(mm.xfs, mm.xfs.gc, test = "Chi"))
(c <- anova(mm.xfs, mm.xfs.ply, test = "Chi"))
(d <- anova(mm.xfs, mm.xfs.xy, test = "Chi"))
(e <- anova(mm.xfs, mm.xfs.yfs, test = "Chi"))
(a.2009 <- rbind(a, b, c, d, e))
mm.xfs$call # best

# year 2005 ####
rm(list = ls(pattern = "^mm")); rm(list = letters[1:4])
mm <- startM(2010)
mm$m
dropterm(mm$m, test = "Chi")
mm.gc <- polr(aph.bf1 ~ pl +  x + y + fs + 
                pl:y + fs:x + x:y + y:fs, data = mm$df)
anova(mm$m, mm.gc, test = "Chi")
dropterm(mm.gc, test = "Chi")

mm.gc.xfs <- polr(aph.bf1 ~ pl +  x + y + pl:y + 
                    fs + x:y + y:fs, data = mm$df)
anova(mm.gc, mm.gc.xfs, test = "Chi")
dropterm(mm.gc.xfs, test = "Chi")

mm.gc.xfs.yfs <- polr(aph.bf1 ~ pl +  x + y + pl:y + 
                    fs + x:y , data = mm$df)
anova(mm.gc.xfs, mm.gc.xfs.yfs, test = "Chi")
dropterm(mm.gc.xfs.yfs, test = "Chi")

mm.gc.xfs.yfs.ply <- polr(aph.bf1 ~ pl +  x + y + 
                        fs + x:y , data = mm$df)
anova(mm.gc.xfs.yfs, mm.gc.xfs.yfs.ply, test = "Chi")
dropterm(mm.gc.xfs.yfs.ply, test = "Chi")

mm.gc.xfs.yfs.ply.pl <- polr(aph.bf1 ~  x + y + 
                            fs + x:y , data = mm$df)
anova(mm.gc.xfs.yfs.ply, mm.gc.xfs.yfs.ply.pl, test = "Chi")
dropterm(mm.gc.xfs.yfs.ply.pl, test = "Chi")

mm.gc.xfs.yfs.ply.pl.xy <- polr(aph.bf1 ~  x + y + 
                               fs  , data = mm$df) # best
anova(mm.gc.xfs.yfs.ply.pl, mm.gc.xfs.yfs.ply.pl.xy, test = "Chi")
dropterm(mm.gc.xfs.yfs.ply.pl.xy, test = "Chi")

(a <- anova(mm.gc.xfs.yfs.ply.pl.xy, 
            mm.gc.xfs.yfs.ply.pl, 
            mm.gc.xfs.yfs.ply, 
            mm.gc.xfs.yfs, 
            mm.gc.xfs, 
            mm.gc, 
            mm$m, test = "Chi"))

mm.gc.xfs.yfs.ply.pl.xy.x  <- polr(aph.bf1 ~      y + fs, data = mm$df)
mm.gc.xfs.yfs.ply.pl.xy.y  <- polr(aph.bf1 ~  x +     fs, data = mm$df)
mm.gc.xfs.yfs.ply.pl.xy.fs <- polr(aph.bf1 ~  x + y     , data = mm$df)
(b <- anova(mm.gc.xfs.yfs.ply.pl.xy, mm.gc.xfs.yfs.ply.pl.xy.x, test = "Chi"))
(c <- anova(mm.gc.xfs.yfs.ply.pl.xy, mm.gc.xfs.yfs.ply.pl.xy.y, test = "Chi"))
(d <- anova(mm.gc.xfs.yfs.ply.pl.xy, mm.gc.xfs.yfs.ply.pl.xy.fs, test = "Chi"))
(a.2010 <- rbind(a, b, c, d))
mm.gc.xfs.yfs.ply.pl.xy$call # best

# compile years ####
rm(list = ls(pattern = "^mm")); rm(list = letters[1:4])
rm(df)

reorgTable <- function(df, mNo, to, yr){
  df$mNo <- mNo            # model number
  df$to <- to              # table order
  df$year <- yr            # year
  df <- df[order(df$to), ] # sort by table order
  orderForTestCols <- c(2:dim(df)[1],  1)
  df[orderForTestCols, c("Test", "   Df", "LR stat.", "Pr(Chi)")] <- 
    df[                , c("Test", "   Df", "LR stat.", "Pr(Chi)")]
  df
}

#a.2005
a05 <- reorgTable(a.2005,
                 c(4:1, 5, 4, 6, 4, 7, 4),
                 c(4:1, 6:5, 8:7, 10:9),
                 2005)
#a05

#a.2006
a06 <- reorgTable(a.2006,
                 c(5:1, 6, 5, 7, 5, 8, 5),
                 c(5:1, 7:6, 9:8, 11:10),
                 2006)
#a06

#a.2007
a07 <- reorgTable(a.2007,
                 c(6:1, 7, 6, 8, 6),
                 c(6:1, 8:7, 10:9),
                 2007)
#a07

#a.2008
a08 <- reorgTable(a.2008,
                  c(3:1, 4, 3, 5, 3, 6, 3),
                  c(3:1, 5:4, 7:6, 9:8),
                  2008)
#a08

#a.2009
a09 <- reorgTable(a.2009,
                  c(2:1, 3, 2, 4, 2, 5, 2, 6, 2),
                  c(2:1, 4:3, 6:5, 8:7, 10:9),
                  2009)
#a09

#a.2010
a10 <- reorgTable(a.2010,
                  c(7:1, 8, 7, 9, 7, 10, 7),
                  c(7:1, 9:8, 11:10, 13:12),
                  2010)
#a10

a <- rbind(a05, a06, a07, a08, a09, a10)
a <- a[, c("year", "mNo", "Model", "Resid. df", "Resid. Dev", "   Df", "LR stat.", "Pr(Chi)")]
names(a)[names(a) == "Model"] <- "Model terms"
names(a)[names(a) == "mNo"] <- "Model"
names(a)[names(a) == "year"] <- "Year"
names(a)[names(a) == "Pr(Chi)"] <- "p"
a$"LR stat." <- round(a$"LR stat.", 3)
a$p <- round(a$p, 3)
a <- format(a) # asis
a$p <- as.character(a$p)
a[a$p %in% "0.000", "p"] <- "< 0.001"
a[a$p == "   NA", c("   Df", "LR stat.", "p")] <- ""

s <- gsub(" ", "", a[, 3])
ss <- strsplit(s, "[+]")
term <- as.character(rep("", 63))
for(i in 2:63){
  print(i) 
  p <- setdiff(ss[[i - 1]], ss[[i]])
  if(length(p) == 0) term[i] <- "" else term[i] <- p
  if(length(p) > 1) term[i] <- "xxxxxx"
}
term
a$"Test term" <- term
a <- a[, c(1:5, 9, 6:8)]
names(a)[names(a) == "p"] <- "P-value"
a

# sink(file = "tableA.txt")
# print(a, row.names=FALSE, na.print = "") 
# sink()

# nice!
# I added asterisks to indicate best models and added a line between each year.
# Also change ":" to "*".

# to do ; add column term that shows the term tested
# code ideas to compare strings & extract difference
# http://journal.r-project.org/archive/2010-2/RJournal_2010-2_Wickham.pdf

# # use base
# s <- gsub(" ", "", a[, 3])
# ss <- strsplit(s, "[+]")
# term <- as.character(rep("", 63))
# for(i in 2:63){
#   print(i) 
#   p <- setdiff(ss[[i - 1]], ss[[i]])
#   if(length(p) == 0) term[i] <- "" else term[i] <- p
# }
# a$term <- term

