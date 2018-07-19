### Goal ###
# look at the change in dynamic event times over the same five year period 
# predict the dynamic event times for 2018 
###

library(tidyverse)

# Load data
setwd("E:/ME/PostGrad/FSAE analysis/Points Analysis")
X <- read.csv("Top 10 trends.csv")
tten <- subset(X, X$Year < 2018)
tten8 <- subset(X, X$Year == 2018)

## Placements
tnd_P <- lm(tten8$Accel ~ tten8$Place)
# intercept 4.33
# trend 0.00804

ptp <- ggplot(X) +
    geom_point(aes(x = as.factor(Year), y = Place, col = Team)) +
    geom_boxplot(aes(x = as.factor(Year), y = Place), alpha = 0.25) + 
    ylab("Final Placement") + ggtitle("Placement Trends of Sampled Teams")
ptp

## Acceleration Event 
tnd_A <- lm(tten$Accel ~ tten$Year)
tnd_A
# intercept 11.895
# trends -.0037


pta <- ggplot(X) +
    geom_point(aes(x = Year, y = Accel)) +
    geom_abline(intercept = tnd_A$coefficients[1], slope = tnd_A$coefficients[2]) +
    ylab("Seconds") + ggtitle("Acceleration Event Trends")
pta


## SkidPad Event 
tnd_S <- lm(tten$Skid_pad ~ tten$Year)
tnd_S
# intercept 113.834
# trend -.0539

pts <- ggplot(X) +
    geom_point(aes(x = Year, y = Skid_pad)) +
    geom_abline(intercept = tnd_S$coefficients[1], slope = tnd_S$coefficients[2]) +
    ylab("Seconds") + ggtitle("Skidpad Event Trends")
pts


## AutoCross Event
tnd_C <- lm(tten$Autocross ~ tten$Year)
# intercept 3404.363
# trend -1.664

ptc <- ggplot(X) +
    geom_point(aes(x = Year, y = Autocross)) +
    geom_abline(intercept = tnd_C$coefficients[1], slope = tnd_C$coefficients[2]) +
    ylab("Seconds") + ggtitle("Autocross Event Trends")
ptc


## Endurance Event 
tnd_E <- lm(tten$Endurance ~ tten$Year)
# intercept -10610.405
# trend 5.983

pte <- ggplot(X) +
    geom_point(aes(x = Year, y = Endurance)) +
    geom_abline(intercept = tnd_E$coefficients[1], slope = tnd_E$coefficients[2]) +
    ylab("Seconds") + ggtitle("Endurance Event Trends")
pte


## 2018 predictions
accel <- tnd_A$coefficients[1] + tnd_A$coefficients[2] * 2018
skidpad <- tnd_S$coefficients[1] + tnd_S$coefficients[2] * 2018
autocross <- tnd_C$coefficients[1] + tnd_C$coefficients[2] * 2018
endurance <- tnd_E$coefficients[1] + tnd_E$coefficients[2] * 2018

## 2018 actual 
bpa <- ggplot(tten8) +
    geom_boxplot(aes(y = Accel, x = as.factor(Year))) +
    geom_point(aes(y = accel, x = as.factor(2018)), col = "red", size = 3) +
    xlab("Year") + ylab("seconds") + ggtitle("Actual vs. Prediction for Acceleration Event")
bpa

bps <- ggplot(tten8) +
    geom_boxplot(aes(y = Skid_pad, x = as.factor(Year))) +
    geom_point(aes(y = skidpad, x = as.factor(2018)), col = "red", size = 3) +
    xlab("Year") + ylab("seconds") + ggtitle("Actual vs. Prediction for Skid Pad Event")
bps

bpc <- ggplot(tten8) +
    geom_boxplot(aes(y = Autocross, x = as.factor(Year))) +
    geom_point(aes(y = autocross, x = as.factor(2018)), col = "red", size = 3) +
    xlab("Year") + ylab("seconds") + ggtitle("Actual vs. Prediction for Autocross Event")
bpc

bpe <- ggplot(tten8) +
    geom_boxplot(aes(y = Endurance, x = as.factor(Year))) +
    geom_point(aes(y = endurance, x = as.factor(2018)), col = "red", size = 3) +
    xlab("Year") + ylab("seconds") + ggtitle("Actual vs. Prediction for Endurance Event")
bpe