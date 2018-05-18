# Includes 
library(tabulizer)
library(data.table)
library(dplyr)


files <- t(read.table("ResultURLs.txt"))
# this file contains the urls of the pdf files of the results 

page <- c(3,3,3,3,3)
# only pull data from the first 3 pages 
# the final 2 pages have too many missing values making more problems than solving


out <- extract_tables(files[5], pages = 1:page[5], 
                      method = "stream" )
str(out)
out[[1]] <- out[[1]][,-8]
out[[2]] <- out[[2]][,-8]
out[[3]] <- out[[3]][,-8]
out <- lapply(out, function(x) {if(any(class(x) =="matrix")) as.data.frame(x) else x})

# pull table out of each page 
df <- rbindlist(out)
# combine the list into 1 data.frame

scores <- sapply(df[,4:13], gsub, pattern = "[^0-9. ]", replacement = "")
scores <- as.data.frame(scores)
# fix some formatting issues 

# save as csv file 
results <- bind_cols(df[,1:3], scores)
names <- c("Place", "Car Num", "Team", "Penalty", "Cost", "Presentation", "Design", "Accel", "Skid_pad", "Autocross", "Endurance", "Efficiency", "Total")
colnames(results) <- names
write.csv(results, file = "fsae_mi_201#.csv")