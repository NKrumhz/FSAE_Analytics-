#comparing RCV axles to TRE axles 
X <- read.csv("rcvDATA.txt", header = T)
attach(X)
RCVs <- as.vector(subset(Hardness, Axle == 1))
RCVs <- RCVs + 1 # +0 for calibration, +1 for roundness
RCVl <- as.vector(subset(Hardness, Axle == 2))
RCVl <- RCVl +1  #  +0 for calibration, +1 for roundness 

Y <- read.csv("2016DATA.txt", header = T)
attach(Y)
TREs <- as.vector(TRE.short, mode = 'numeric')
TREs <- TREs + 2 + 1 # +2 for calibration, +1 for roundness
TREl <- as.vector(TRE.long, mode = 'numeric')
TREl <- TREl + 2 + 1 # +2 for calibration, +1 for roundness

#short comparison
cat("\n\n Comparison of short axle\n")
short <- c(TREs, RCVs)
types <- factor(c(rep("TRE", length(TREs)), rep("RCV", length(RCVs))))
sam <- aovp(short ~ types)
summary(sam)
boxplot(short ~ types, horizontal = T, xlab = "Hardness", main = "Short axle")

#long comparison
cat("\n\n Comparison of long axle\n")
long <- c(TREl, RCVl)
typel <- factor(c(rep("TRE", length(TREl)), rep("RCV", length(RCVl))))
lam <- aov(long ~ typel)
summary(lam)
boxplot(long ~ typel, horizontal = T, xlab = "Hardness (HBC)", main = "Long Axle")

#compare all
cat("\n\n Comparison of axle brands\n")
data <- c(TREs, TREl, RCVs, RCVl)
TREa <- c(TREl, TREs)
RCVa <- c(RCVl, RCVs)
brand<- as.factor(c(rep("TRE", length(c(TREl,TREs))), rep("RCV", length(c(RCVl,RCVs)))))
Length<- as.factor(c(rep("short", length(TREs)), rep("long", length(TREl)), rep("short", length(RCVs)), rep("long", length(RCVl))))
fm <- aov(data ~ brand)
summary(fm)

cat("\n\n Comparison of axle brands accounting for length\n")
gm <- aov(data ~ brand * Length)
summary(gm)

# intervals 
cat("\n\n TRE data\n")
summary(TREa)
cat("\n\n RCV data\n")
summary(RCVa)
boxplot(data ~ brand, horizontal = T, xlab = "Hardness (HBC)", varwidth = T, main = "All Data")

# looking into non-normal residuals 
# can use a permutated anova from the lmPerm package to compute the significance of each feature 
library(lmPerm)
mod <- aovp(data ~ brand * Length)
summary(mod)
qqnorm(mod$res)
qqline(mod$res, col = "red")