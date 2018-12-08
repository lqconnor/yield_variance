# Define work space --------------------------------------------------------------------------
rm(list = ls())
cat("\f")
getwd()

library(tidyverse)
library(stringr)
library(magrittr)
library(lubridate)


memory.limit()

memory.limit(size=300000)

setwd("C:/Research/Crop Insurance and yield variance/Data")


# Join Precipitation data ---------------------------------------------------------------------------
ppt <- read_csv("m_ppt.csv")
ppt_1988 <- read_csv("m_ppt_1988.csv")

ppt <- bind_rows(ppt_1988, ppt)

ppt$month <- as.numeric(gsub(",","", ppt$month, fixed = TRUE))

# get indicator for growing season months
c <- 1
ppt$winter <- 0
for(y in 1989:2016){
  ppt$winter[ppt$month > 3 & ppt$year == y | ppt$month < 11 & ppt$year == y] <- c
  c <- c + 1
}

# sum the total precipitation over the growing season
ppt <- group_by(ppt, STATEFP, COUNTYFP, winter) %>%
  mutate(wint_ppt = sum(m_ppt)) %>%
  arrange(STATEFP, COUNTYFP, year, month) %>%
  ungroup()

# replace all non-growing season months with precipiation sums of 0
ppt$wint_ppt[ppt$winter == 0 | ppt$month > 10] <- 0

# spread the growing season sum over current year and reduce to one observation per state / county / year
ppt <- group_by(ppt, STATEFP, COUNTYFP, year) %>%
  mutate(w_ppt = mean(wint_ppt)) %>%
  ungroup() %>%
  select(-month, -m_ppt, -winter, -wint_ppt) %>%
  filter(year >= 1989) %>%
  rename(cnty_name = NAME) %>%
  distinct()

# Join Temperature data -----------------------------------------------------------------------
#dtemp <- read_csv("Weather Scripts/Data/dt_mean.csv")
dtemp <- read_csv("dt_mean.csv")
dtemp$month <- as.numeric(gsub(",","", dtemp$month, fixed = TRUE))

# Calculate Growing Degree Days (GDD)
# get indicator for growing season days
c <- 1
dtemp$grow <- 0
for(y in 1989:2016){
  dtemp$grow[dtemp$month >= 4 & dtemp$month < 11 & dtemp$year == y] <- c       #create unique factor number for the growing season in each year so that we can sum by group next
  c <- c + 1
}

# Calculate growing degree day
temp <- mutate(dtemp, hdd = dt_mean - 29)        # Calculation for number of degrees above 10 degrees celcius in a day.
temp <- mutate(temp, hdd = ifelse(hdd < 0, 0, hdd))          
               

# sum the total growing degree days
temp <- group_by(temp, STATEFP, COUNTYFP, grow) %>%
  mutate(s_hdd = sum(hdd)) %>%
  arrange(STATEFP, COUNTYFP, year, month) %>%
  ungroup()

# replace all non-winter months with precipiation sums of 0
temp$s_hdd[temp$grow == 0] <- 0

# spread the winter sum over current year and reduce to one observation per state / county / year
temp <- group_by(temp, STATEFP, COUNTYFP, year) %>%
  mutate(HDD = mean(s_hdd)) %>%
  ungroup() %>%
  select(STATEFP, COUNTYFP, year, HDD) %>%
  filter(year >= 1989) %>%
  distinct()


# Join vapor pressure deficit data -----------------------------------------------------------------------
#dtemp <- read_csv("Weather Scripts/Data/dt_mean.csv")
vapd <- read_csv("vpdmax.csv")
vapd$month <- as.numeric(gsub(",","", vapd$month, fixed = TRUE))

# Calculate Growing Degree Days (GDD)
# get indicator for growing season days
c <- 1
vapd$grow <- 0
for(y in 1989:2016){
  vapd$grow[vapd$month >= 4 & vapd$month < 11 & vapd$year == y] <- c       #create unique factor number for the growing season in each year so that we can sum by group next
  c <- c + 1
}

# # Calculate growing degree day
# temp <- mutate(dtemp, hdd = dt_mean - 29)        # Calculation for number of degrees above 10 degrees celcius in a day.
# temp <- mutate(temp, hdd = ifelse(hdd < 0, 0, hdd))          


# sum the total growing degree days
mvap <- group_by(vapd, STATEFP, COUNTYFP, grow) %>%
  mutate(s_vpdmax = mean(vpdmax)) %>%
  arrange(STATEFP, COUNTYFP, year, month) %>%
  ungroup()

# replace all non-winter months with precipiation sums of 0
mvap$s_vpdmax[mvap$grow == 0] <- 0

# spread the winter sum over current year and reduce to one observation per state / county / year
vapor <- group_by(mvap, STATEFP, COUNTYFP, year) %>%
  mutate(vpd = mean(s_vpdmax)) %>%
  ungroup() %>%
  select(STATEFP, COUNTYFP, year, vpd) %>%
  filter(year >= 1989) %>%
  distinct() 


# join to NASS2 dataset
weather <- left_join(temp, ppt, by = c("STATEFP" = "STATEFP",
                                       "COUNTYFP" = "COUNTYFP",
                                       "year" = "year")) %>%
  left_join(., vapor, by = c("STATEFP" = "STATEFP",
                                     "COUNTYFP" = "COUNTYFP",
                                     "year" = "year"))

weather$COUNTYFP <- as.character(sprintf("%03d", weather$COUNTYFP))
weather <- unite(weather, st_cnty, STATEFP, COUNTYFP, sep = "")
write_csv(weather, "../Scripts/yld_var_git/weather.csv")
