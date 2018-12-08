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

# replace all non-growing season months with precipiation sums of NA
ppt$wint_ppt[ppt$winter == 0 | ppt$month > 10] <- NA

# spread the growing season sum over current year and reduce to one observation per state / county / year
ppt <- group_by(ppt, STATEFP, COUNTYFP, year) %>%
  mutate(w_ppt = mean(wint_ppt, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-month, -m_ppt, -winter, -wint_ppt) %>%
  filter(year >= 1989) %>%
  rename(cnty_name = NAME) %>%
  distinct()

# Join Temperature data -----------------------------------------------------------------------
#dtemp <- read_csv("Weather Scripts/Data/dt_mean.csv")
dtemp <- read_csv("dt_mean.csv")
td_temp <- read_csv("tdmean.csv")
dtemp$month <- as.numeric(gsub(",","", dtemp$month, fixed = TRUE))
td_temp$month <- as.numeric(gsub(",","", td_temp$month, fixed = TRUE))

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

# replace all non-winter months with precipiation sums of NA
temp$s_hdd[temp$grow == 0] <- NA

# spread the winter sum over current year and reduce to one observation per state / county / year
temp <- group_by(temp, STATEFP, COUNTYFP, year) %>%
  mutate(HDD = mean(s_hdd, na.rm = TRUE)) %>%
  ungroup() %>%
  select(STATEFP, COUNTYFP, year, HDD) %>%
  filter(year >= 1989) %>%
  distinct()


# Join vapor pressure deficit data -----------------------------------------------------------------------
#dtemp <- read_csv("Weather Scripts/Data/dt_mean.csv")
vapmax <- read_csv("vpdmax.csv")
vapmin <- read_csv("vpdmin.csv")
vapmax <- left_join(vapmax, vapmin, by = c("STATEFP" = "STATEFP",
                                           "COUNTYFP" = "COUNTYFP",
                                           "NAME" = "NAME",
                                           "year" = "year",
                                           "month" = "month"))

vapmax$month <- as.numeric(gsub(",","", vapmax$month, fixed = TRUE))
vapmin$month <- as.numeric(gsub(",","", vapmin$month, fixed = TRUE))

# Calculate Growing Degree Days (GDD)
# get indicator for growing season days
c <- 1
vapmax$grow <- 0
for(y in 1989:2016){
  vapmax$grow[vapmax$month >= 4 & vapmax$month < 11 & vapmax$year == y] <- c       #create unique factor number for the growing season in each year so that we can sum by group next
  c <- c + 1
}

# # Calculate growing degree day
# temp <- mutate(dtemp, hdd = dt_mean - 29)        # Calculation for number of degrees above 10 degrees celcius in a day.
# temp <- mutate(temp, hdd = ifelse(hdd < 0, 0, hdd))          

vapmax <- mutate(vapmax, vpd_m = (vpdmin + vpdmax)/2)
# sum the total growing degree days
mvap <- group_by(vapmax, STATEFP, COUNTYFP, grow) %>%
  mutate(m_vpd = mean(vpd_m)) %>%
  arrange(STATEFP, COUNTYFP, year, month) %>%
  ungroup()

# replace all non-winter months with precipiation sums of 0
mvap$m_vpd[mvap$grow == 0] <- NA

# spread the winter sum over current year and reduce to one observation per state / county / year
vapor <- group_by(mvap, STATEFP, COUNTYFP, year) %>%
  mutate(vpd = mean(m_vpd, na.rm = TRUE)) %>%
  ungroup() %>%
  select(STATEFP, COUNTYFP, year, vpd) %>%
  filter(year >= 1989) %>%
  distinct() 

# VPD from dewpoint temperature------------------------------------------
m_temp <- group_by(dtemp, STATEFP, COUNTYFP, year, month) %>%
  mutate(a_temp = mean(dt_mean)) %>%
  select(-c(day, dt_mean)) %>%
  distinct() %>%
  left_join(td_temp, by = c("STATEFP" = "STATEFP",
                            "COUNTYFP" = "COUNTYFP",
                            "NAME" = "NAME",
                            "month" = "month",
                            "year" = "year")) %>%
  mutate(dew_vpd = 6.1094*(exp(17.625*a_temp/(a_temp + 243.04)) - exp(17.625*tdmean/(tdmean + 243.04))))

# sum the total growing degree days
mvpd <- group_by(m_temp, STATEFP, COUNTYFP, grow) %>%
  mutate(dm_vpd = mean(dew_vpd)) %>%
  arrange(STATEFP, COUNTYFP, year, month) %>%
  ungroup()

# replace all non-winter months with precipiation sums of 0
mvpd$dm_vpd[mvpd$grow == 0] <- NA

# spread the winter sum over current year and reduce to one observation per state / county / year
dw_vpd <- group_by(mvpd, STATEFP, COUNTYFP, year) %>%
  mutate(vpd_dew = mean(dm_vpd, na.rm = TRUE)) %>%
  ungroup() %>%
  select(STATEFP, COUNTYFP, year, vpd_dew) %>%
  filter(year >= 1989) %>%
  distinct()

# join datasets ----------------------------------------------------------
weather <- left_join(temp, ppt, by = c("STATEFP" = "STATEFP",
                                       "COUNTYFP" = "COUNTYFP",
                                       "year" = "year")) %>%
  left_join(., vapor, by = c("STATEFP" = "STATEFP",
                            "COUNTYFP" = "COUNTYFP",
                            "year" = "year")) %>%
  left_join(., dw_vpd, by = c("STATEFP" = "STATEFP",
                             "COUNTYFP" = "COUNTYFP",
                             "year" = "year"))

# Convert format to match yield vol data and save ----------------------------
weather$COUNTYFP <- as.character(sprintf("%03d", weather$COUNTYFP))
weather <- unite(weather, st_cnty, STATEFP, COUNTYFP, sep = "")
write_csv(weather, "weather.csv")
