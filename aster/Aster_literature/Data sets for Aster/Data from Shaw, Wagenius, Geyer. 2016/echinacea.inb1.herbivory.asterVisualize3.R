# This script reads the three output files from Ruth & Charlie's analysis and
# makes figures 3 & 4 for the manuscript. SW 2015-05-05

# Let me know if you have any questions! 
# Stuart Wagenius 
# stuart.wagenius@gmail.com
# http://echinaceaproject.org/

load("barf.RData")
load("bars.RData")
load("fred.RData")

# reshape barf data frame ####
str(barf)
ff <- data.frame(barf)
long.barf <- reshape(ff, idvar = "crossAa", ids = row.names(ff),
                times = names(ff), timevar = "layerYr",
                varying = list(names(ff)), direction = "long")
# str(long.barf)
names(long.barf)[2] <- "mu"
long.barf$layer <- gsub("[0-9]", "", long.barf$layerYr)
long.barf$year <- as.integer(gsub("[A-z]", "", long.barf$layerYr))
ca <- strsplit(long.barf$crossAa, "[.]")
df <- data.frame(t(sapply(ca, `[`)))
names(df) <- c("bwi", "aphida")
long.barf <- cbind(long.barf, df)
row.names(long.barf) <- NULL
long.barf <- long.barf[, c("year", "layer", "bwi", "aphida", "mu")]
# long.barf
rm(df, ca, ff)

# reshape bars data frame ####
str(bars)
ff <- data.frame(bars)
long.bars <- reshape(ff, idvar = "crossAa", ids = row.names(ff),
                     times = names(ff), timevar = "layerYr",
                     varying = list(names(ff)), direction = "long")
# str(long.bars)
names(long.bars)[2] <- "se"
long.bars$layer <- gsub("[0-9]", "", long.bars$layerYr)
long.bars$year <- as.integer(gsub("[A-z]", "", long.bars$layerYr))
ca <- strsplit(long.bars$crossAa, "[.]")
df <- data.frame(t(sapply(ca, `[`)))
names(df) <- c("bwi", "aphida")
long.bars <- cbind(long.bars, df)
row.names(long.bars) <- NULL
long.bars <- long.bars[, c("year", "layer", "bwi", "aphida", "se")]
# long.bars
rm(df, ca, ff)

long.barf <- merge(long.barf, long.bars)

# make graphs for each layer: x = year, bwi in three lines, aphida 
# visualize inbreeding depression

unique(long.barf$layer)
long.barf$a.y <- paste(long.barf$aphida, long.barf$year, sep = ".")
long.barf$bwi <- relevel(long.barf$bwi, "Wr")
long.barf$bwi <- relevel(long.barf$bwi, "Br")

# look at fred ####
ff <- fred

ba <- strsplit(rownames(ff), "[.]")
df <- data.frame(t(sapply(ba, '[')))
ff <- cbind(df, ff)
names(ff) <- c("bwi", "aphida", "mu", "se")
ff$bwi <- relevel(ff$bwi, "Wr")
ff$bwi <- relevel(ff$bwi, "Br")

x <- ff[ff$aphida == "bf0",]
calcID <- function(ff) {
  1 - ff[ff$bwi == "Wi", "mu"] / ff[ff$bwi == "Wr", "mu"] 
}
calcID(x)

# now go back to long.barf & add inbr dep ####

inbDep <- ddply(long.barf, .(layer, year), calcID)
names(inbDep)[3:6] <- c("bf0", "bf1", "bf2", "bf3")
inbDep[3:6] <- round(inbDep[3:6], 3)

# now make figBarf2009 (ii) ####

bb <- long.barf[long.barf$year == 2009, ]
bb$layer <- as.factor(bb$layer)
bb$layer <- relevel(bb$layer, "hdCt")
bb$layer <- relevel(bb$layer, "achCt")
levels(bb$layer)

head(bb)
bb$bwii <- as.integer(bb$bwi)
bb <- bb[order(bb$bwii), ]


ja <- 0.03 # jitter amount
bb$bwii <- bb$bwii + (as.integer(bb$aphida) * ja) + (-2.5 * ja)
bb <- bb[order(bb$bwii), ]

# script for April 2015 starts here #######

# require(RColorBrewer)
# colz <- brewer.pal(5, "Greens")[2:5]
colz <- c("#BAE4B3", "#74C476", "#31A354", "#006D2C")
textx = .8


plotLayer <- function(layr = "achCt", 
                      yLim = c(100, 205), 
                      yLab = "Achenes",
                      xa = "n", df = bb, 
                      arro = FALSE){
  dd <- df[df$layer == layr, ]
  plot  (mu ~ bwii, dd[dd$aphida == "bf0", ], type = "l", col = colz[1],
         ylim = yLim, ylab = yLab, xlim = c(.8, 3.2), xaxt = xa, lwd = 2,
         cex.lab = 1.5, bty = "n")
  points(mu ~ bwii, dd[dd$aphida == "bf1", ], type = "l", col = colz[2], lwd = 2)
  points(mu ~ bwii, dd[dd$aphida == "bf2", ], type = "l", col = colz[3], lwd = 2)
  points(mu ~ bwii, dd[dd$aphida == "bf3", ], type = "l", col = colz[4], lwd = 2)
  if (arro){
    with(df[df$layer == layr, ], arrows(bwii, mu - se, bwii, mu + se, lwd = 2, 
                                        col = colz,
                                           code = 3, length = 0.01 , angle = 90))
  }
}

fname <- paste("fig3at", format(Sys.time(), "%H-%M-%S"), "-AM.pdf", sep = "")
pdf(fname, width = 3.5, height = 6)
par(mfcol = c(4, 1))
par(cex = 0.6)
par(mar = c(0, 5, 0, 0), oma = c(4, 0.5, 0.5, 0.5))
par(tcl = -0.25)
par(mgp = c(2.75, 0.6, 0))
par(las = 1)
plotLayer(arro = TRUE)
legend(0.99, 140, legend = c("none", "low", "medium", "high"), 
       lty = 1, col = colz, lwd = 2, bty = "o", 
       title = "Aphid-load", cex = 1,
       ncol = 2, box.col = "gray")
text(textx, 190, "A", cex = 1.4)
plotLayer("hdCt", c(0, 3), "Heads", arro = TRUE)
text(textx, 2.5, "B", cex = 1.4)
plotLayer("fl", c(0, 0.5), "Flowering rate", arro = TRUE)
text(textx, .45, "C", cex = 1.4)
plotLayer("ld", c(0.80, 1), "Survival", arro = TRUE)
text(textx, .97, "D", cex = 1.4)
axis(1, 1:3, c("B", "W", "I"), cex.axis = 1.5)
mtext("Genotypic Class", side = 1, line = 2.5, cex = 1.1)
dev.off()


# now make figFred (iii) ####

head(bb)
ff
ff$bwii <- as.integer(ff$bwi)
ja <- 0.03 # jitter amount
# ff$jit <- (as.integer(ff$aphida) * ja) + (-2.5 * ja)
ff$bwii <- ff$bwii + (as.integer(ff$aphida) * ja) + (-2.5 * ja)
ff <- ff[order(ff$bwii), ]
ff$layer <- "achCt"

fname <- paste("fig4at", format(Sys.time(), "%H-%M-%S"), "-AM.pdf", sep = "")
pdf(fname, width = 5, height = 4)
par(mfcol = c(1, 1))
par(cex = 0.6)
par(mar = c(0, 4, 0, 0), oma = c(4, 0.5, 0.5, 0.5))
par(tcl = -0.25)
par(mgp = c(2.75, 0.6, 0))
par(las = 1)
plotLayer("achCt", c(0, 800), "Total achenes", df = ff)
with(ff, arrows(bwii, mu - se, bwii, mu + se, lwd = 2, col = colz,
                code = 3, length = 0.03 , angle = 90))
legend("topright", legend = c("none", "low", "medium", "high"), 
       col = colz, lwd = 2, bty = "o", 
       title = "Aphid-load", cex = 1.2,
       ncol = 2, box.col = "gray")
axis(1, 1:3, c("B", "W", "I"), cex.axis = 1.5)
mtext("Genotypic Class", side = 1, line = 2.5, cex = 1.2)
dev.off()

