rm(list = ls())
cat("\f")
getwd()
#setwd("C:/Users/Lawson/Google Drive/Daily Temp/")
setwd("C:/Users/Lawson/Google Drive/Data and Clean Scripts/ZB/Weather Scripts/Data/")
options(max.print = 1000000)
library(gsubfn)
library(plyr)
library(dplyr)
library(reshape2)
library(stringr)
library(tidyr)

dt <- read.csv(file="daily_tmean_1989.txt",header=T,sep = ",")

y <- names(dt[50])

z <- sub("^[A-Z]+_([a-z]+)_.*$","\\1",y)

d <- sub("t","dt_",z)

x <- c("PRISM_","_stable_4kmD1","_bil",paste0(z,"_"))

for(i in 1:length(x)){
  names(dt) <- gsub(x[i], "", names(dt), fixed = TRUE)
}

# Use sub and regular expressions to add a period after year in the
# variable names. 
# Note that sub is used instead of gsub because sub replaces the first match
# whereas gsub would replace all matches and place a period at the end of the
# variable name as well.

dm <- dt
names(dm) <- sub("(\\d{4})","\\1\\.", names(dm))

dm <- dm[c(2,3,6,7,20:length(dm))]

molten <- melt(dm, variable.name = "key",value.names = "value",id.vars = c("STATEFP", "COUNTYFP", "NAME","NAMELSAD"))

tidy <- separate(molten, key, into = c("year", "all"), sep = "\\.")

tidy$all <- sub("(\\d{2})","\\1\\.", tidy$all)

dtm <- separate(tidy, all, into = c("month", "day"), sep = "\\.")
names(dtm) <- sub("value",paste0(d), names(dtm))

write.csv(dtm,file = paste0(d,".csv"), row.names = FALSE)