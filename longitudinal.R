### Goal ###
# look at the change in scores over time
###
library(tidyverse)

res2017 <- read.csv("fsae_mi_2017.csv")
res2017$year <- 2017
res2016 <- read.csv("fsae_mi_2016.csv")
res2016$year <- 2016
res2015 <- read.csv("fsae_mi_2015.csv")
res2015$year <- 2015
res2014 <- read.csv("fsae_mi_2014.csv")
res2014$year <- 2014
res2013 <- read.csv("fsae_mi_2013.csv")
res2013$year <- 2013

restdl <- do.call("rbind", list(res2017,res2016,res2015,res2014,res2013))

pt <- ggplot(restdl) + 
  geom_point(aes(x=year, y = Total))
pt

gatherpairs <- function(data, ..., xkey = '.xkey', xvalue = '.xvalue', ykey = '.ykey', yvalue = '.yvalue', na.rm = FALSE, convert = FALSE, factor_key = FALSE) {
  vars <- quos(...)
  xkey <- enquo(xkey)
  xvalue <- enquo(xvalue)
  ykey <- enquo(ykey)
  yvalue <- enquo(yvalue)
  
  data %>% {
    cbind(gather(., key = !!xkey, value = !!xvalue, !!!vars, na.rm = na.rm, convert = convert, factor_key = factor_key), dplyr::select(., !!!vars))
  }   %>% gather(., key = !!ykey, value = !!yvalue, !!!vars, na.rm = na.rm, convert = convert, factor_key = factor_key)
}

# dynamic events 
restdl %>% gatherpairs( Accel, Skid_pad, Autocross, Endurance, Total) %>%
  ggplot(aes(x = .xvalue, y = .yvalue, color = Place), alpha = 0.2) +
  geom_point() +
  facet_wrap(.xkey ~ .ykey, scales = 'free')

# static events 
restdl %>% gatherpairs( Cost, Presentation, Design, Efficiency, Total) %>%
  ggplot(aes(x = .xvalue, y = .yvalue, color = Place), alpha = 0.2) +
  geom_point() +
  facet_wrap(.xkey ~ .ykey, scales = 'free')


###
# lets look more closely year to year 
normaliz <- function(value) {
  (value - min(value)) / (max(value) - min(value))
}


rt17 <- data.frame(Team = res2017$Team, year = res2017$year,  sapply(res2017[4:13], function(x) normaliz(x)))
rt16 <- data.frame(Team = res2016$Team, year = res2016$year, sapply(res2016[4:13], function(x) normaliz(x)))
rt15 <- data.frame(Team = res2015$Team, year = res2015$year, sapply(res2015[4:13], function(x) normaliz(x)))
rt14 <- data.frame(Team = res2014$Team, year = res2014$year, sapply(res2014[4:13], function(x) normaliz(x)))
rt13 <- data.frame(Team = res2013$Team, year = res2013$year, sapply(res2013[4:13], function(x) normaliz(x)))

restdl <- do.call("rbind", list(rt17, rt16, rt15, rt14, rt13))

pt <- ggplot(restdl) + 
  geom_point(aes(x=year, y = Total))
pt

# look at only teams that are in dataset for all 5 years 
# plot each team's trend 
# 
sbst <- table(restdl$Team)

sbst <- ifelse(sbst<5, FALSE, TRUE)

trnds <- data.frame(matrix(vector(), 0, 12))
names(trnds) <- names(restdl)

for(i in 1:nrow(restdl)){
  id <- restdl$Team[i]
  
    if(sbst[which(names(sbst)==id)] == TRUE){
      trnds <- rbind(trnds, restdl[i,])
      
  }
}

nrow(trnds)/5
trnds$Team <- as.factor(as.character((trnds$Team)))
trnds[is.na(trnds)] <- 0 
tmanes <- names(table(trnds$Team))

trnds.sb <- trnds[which(trnds$Team %in% tmanes[1:7]),]
pt <- ggplot(trnds.sb) + 
  geom_point(aes(x = year, y = Total, col = Team)) + 
  geom_line(aes(x = year, y = Total, col = Team)) + 
  xlim(2013, 2017)
pt

###
# cluster Teams
# Year should have been accounted for in the normalization so it is not 
# taking part in this fit. 
##pca 
pca <- prcomp(trnds[3:12])
topteams <- kmeans(pca$x[,1:2], centers = 4, iter.max = 100, nstart = 1000,  algorithm = "Hartigan-Wong")
trnds$cluster <- as.factor(topteams$cluster)

pt <- ggplot(trnds) + 
  geom_point(aes(x = pca$x[,2], y = pca$x[,1], col = cluster))
pt
clanes <- vector(length = 27)
for(i in 1:length(tmanes)){
  results <- as.numeric(trnds[which(trnds$Team %in% tmanes[i]),]$cluster)
  clanes[i] <- as.numeric(names(which.max(table(results))))
  
}

print(data.frame(tmanes,clanes))


###
# Predict clusters of all other teams 
library(clue)
pca.all <- predict(pca, newdata = restdl[3:12])
teamcl <- cl_predict(topteams, pca.all[,1:2])
tmanes.n <- names(table(restdl$Team))
restdl$cluster <- as.factor(teamcl)


for(i in 1:length(tmanes.n)){
  results <- as.numeric(restdl[which(restdl$Team %in% tmanes.n[i]),]$cluster)
  clanes[i] <- as.numeric(names(which.max(table(results))))
  
}
print(data.frame(team = tmanes.n,cluster = clanes))