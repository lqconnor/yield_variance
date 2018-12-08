rm(list = ls())
cat("\f")
getwd()
#setwd("C:/Users/Lawson/Google Drive/Daily Temp/")
#setwd("C:/Users/Lawson/Google Drive/Data and Clean Scripts/ZB/Weather Scripts/Data/")
#setwd("C:/Users/connor.189/Documents/Data/Precipitation/")
setwd("C:/Research/Crop Insurance and yield variance/Data")

options(max.print = 1000000)

#Check if packages installed, install missing packages and load all installed packages
pckgs <- c("tidyverse", "reshape2", "stringr")
lapply(pckgs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

dt <- read.csv(file="td_mean.txt",header=T,sep = ",")

# Grab random column to extract name format of precipitation variables then extract the
# descriptive portion (ppt).
y <- names(dt[20])
z <- sub("^[A-Z]+_([a-z]+)_.*$","\\1",y)
d <- sub("d","d",z)                     # Replace first occurrence of a "p" with "m_p" so that name becomes m_ppt

# Remove non-descriptive portions of variable names and replace with descriptive
# portions (mppt_mnth_yr)
x <- c("PRISM_","_stable_4kmM1","_stable_4kmM2","_bil",paste0(z,"_"))

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

dm <- dm[c(2,3,7,12:length(dm))]

molten <- melt(dm, variable.name = "key",value.names = "value",id.vars = c("STATEFP", "COUNTYFP", "NAME"))

ppt <- separate(molten, key, into = c("year", "month"), sep = "\\.")

# These separate month and day variables, so aren't necessary for the monthly script
#tidy$all <- sub("(\\d{2})","\\1\\.", tidy$all)

#dtm <- separate(tidy, all, into = c("month", "day"), sep = "\\.")
names(ppt) <- sub("value",paste0(d), names(ppt))

write.csv(ppt,file = paste0(d,".csv"), row.names = FALSE)