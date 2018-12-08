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
#setwd("C:/Users/connor.189/Documents/Data")
#setwd("C:/Users/Lawson/Documents/Data")


# Load all data files -------------------------------------------------------------------------
ARMS <- read_tsv("qs.crops.txt")
insurance <- read_csv("ins2.csv")
gmo <- read_csv("alltablesGEcrops2.csv")
ppi <- read_csv("Futures Prices/ppi_farm_products.csv")
soil <- read_csv("Soil/county_soil.csv")
fips <- read_csv("fips_codes_website.csv")
economics <- read_tsv("qs.economics.txt")


# Data Parameters -----------------------------------------------------------------------------
beg_year <- 1970

commodities <- c("BARLEY", "WHEAT", "CORN", "SOYBEANS", "RICE", "COTTON", "SORGHUM")

vars <- c("YIELD", "PLANTED", "HARVESTED", "PRODUCTION")

nass_cleaner <- c("NET", "SWEET", "FOLLOWING", "IRRIGATED", "PIMA", "FORAGE", 
                  "EXCL", "SPRING", "WINTER", "PLANTED ACRE")

price_cleaner <- c("SPRING", "EXCL", "FEED", "SWEET", "SILAGE", "MALTING", "WINTER", 
                   "COTTONSEED", "PIMA", "LONG", "MEDIUM", "COTTON -")


# Convert strings -----------------------------------------------------------------------------
# Convert string value column to numeric. Remove comma separator
ARMS$amnt <- as.numeric(gsub(",","", ARMS$VALUE, fixed = TRUE))
ARMS$year <- as.numeric(gsub(",","", ARMS$YEAR, fixed = TRUE))


# Production variables ------------------------------------------------------------------------
# Variables of interest: yield, raw production, acres harvested, acres planted
prep <- filter(ARMS, str_detect(AGG_LEVEL_DESC, "COUNTY"), str_detect(FREQ_DESC, "ANNUAL"),
               str_detect(SOURCE_DESC, "SURVEY")) %>%
  filter(str_detect(SHORT_DESC, paste(vars, collapse = '|'))) %>%       # the paste portion creates a string where the separator is | (meaning or)
  filter(str_detect(SHORT_DESC, paste(commodities, collapse = '|'))) %>%
  filter(!(str_detect(SHORT_DESC, paste(nass_cleaner, collapse = '|')))) %>%
  filter(!(str_detect(UTIL_PRACTICE_DESC, "SILAGE"))) %>%
  filter(COUNTY_CODE < 900, YEAR >= beg_year) %>%
  filter(!str_detect(VALUE, "(D)")) %>%
  select(STATE_FIPS_CODE, COUNTY_CODE, year, SHORT_DESC, amnt)
 

# SHORT_DESC: Clean ---------------------------------------------------------------------------
# Use regex to make all SHORT_DESC observations consistent to convert to tidy form
prep$SHORT_DESC <- sub(",[[:blank:]][A-Z]+", "", prep$SHORT_DESC)
prep$SHORT_DESC <- gsub(",.*", "", prep$SHORT_DESC)
prep$SHORT_DESC <- gsub("- ", "-", prep$SHORT_DESC)
prep$SHORT_DESC <- gsub(" -", "-", prep$SHORT_DESC)
prep$SHORT_DESC <- gsub("YIELD", "YIELD ", prep$SHORT_DESC)
prep$SHORT_DESC <- gsub("////[[:blank:]].*", "", prep$SHORT_DESC)
prep$SHORT_DESC <- gsub(" IN.*", "", prep$SHORT_DESC)


# Convert to tidy form ------------------------------------------------------------------------
nass <- separate(prep, SHORT_DESC, into = c("commodity", "description"), sep = "-") %>%
  spread(key = description, value = amnt) %>%
  rename(yield = `YIELD `) %>% 
  group_by(STATE_FIPS_CODE, COUNTY_CODE, commodity) %>%
  arrange(STATE_FIPS_CODE, COUNTY_CODE, commodity, year) %>%
  mutate(sum = cumsum(yield)) %>%
  mutate(lag = lag(sum, 10)) %>%
  ungroup()

nass$lag[nass$year == 1979] <- 0
nass$avg <- (nass$sum - nass$lag)/10
nass <- select(nass, -lag, -sum)


# Price data ----------------------------------------------------------------------------------
# NASS reports observed prices at the state level which will be joined to the county level production data

price <- filter(ARMS, str_detect(SHORT_DESC, "PRICE"), str_detect(FREQ_DESC, "ANNUAL"), AGG_LEVEL_DESC == "STATE") %>%
  filter(str_detect(COMMODITY_DESC, paste(commodities, collapse = '|'))) %>%
  filter(!(str_detect(UNIT_DESC, paste(c("PCT", "INDEX", "PIMA"), collapse = '|')))) %>%
  filter(!(str_detect(SHORT_DESC, paste(price_cleaner, collapse = '|')))) %>%
  rename(price = amnt) %>%
  filter(YEAR >= beg_year) %>%
  select(STATE_FIPS_CODE, COMMODITY_DESC, year, price) %>%
  arrange(STATE_FIPS_CODE, COMMODITY_DESC, year)

nass <- left_join(nass, price, by = c("STATE_FIPS_CODE" = "STATE_FIPS_CODE",
                                      "commodity" = "COMMODITY_DESC",
                                      "year" = "year")) %>%
  mutate(countyfp = as.numeric(sub("^0+","", COUNTY_CODE))) %>%       # trim leading zeros from county fips
  mutate(statefp = as.numeric(sub("^0+","", STATE_FIPS_CODE)))        # trim leading zers from  state fips


# Add insurance data -------------------------------------------------------------------------
ins <- insurance %>%
  select(-county, -commodity, -st_abbrv, -netrepqty_tp, -prm_p_acr, -max_prm, -comacres, -(polsld:unindm_cnt))

nass$commod_id <- 0

a <- c(11, 18, 21, 41, 51, 81, 91)
b <- c("WHEAT", "RICE", "COTTON", "CORN", "SORGHUM", "SOYBEANS", "BARLEY")

# create commodity ID column in the NASS dataset that matches the commodity IDs used by RMA
# so that joining the datasets is less prone to errors
for (i in 1:length(a)) {
  nass$commod_id[nass$commodity == b[i]] <- a[i]
}

nass <- left_join(nass, ins, by = c("countyfp" = "countyfp", 
                                    "statefp" = "statefp",
                                    "commod_id" = "commod_id",
                                    "year" = "year"))


# Add US level GMO data -----------------------------------------------------------------------
gm <- gmo %>%
  rename(year = Year) %>%
  rename(commodity = Crop) %>% 
  rename(us_gm = Value) %>% 
  filter(State == "U.S.", Variety == "All GE varieties") %>% 
  select(commodity, year, us_gm)

gm$commodity <- str_to_upper(gm$commodity)
gm$commodity <- sub("UPLAND ", "", gm$commodity)

NASS <- left_join(nass, gm, by = c("commodity" = "commodity", 
                                        "year" = "year"))
rm(gm)


# Add State level GMO data --------------------------------------------------------------------
# Generate key to match states with GM observations and states grouped into "Other states"
x <- c(1, 2, 6, 13, 17, 18, 19, 20, 26, 27, 28, 29, 31, 37, 38, 39, 46, 47, 48, 55)

NASS$newfp <- 10000

for(i in 1:length(x)){
  NASS$newfp[NASS$statefp == x[i]] <- NASS$statefp[NASS$statefp == x[i]]
}


#Join state level GMO data
gm <- gmo %>%
  rename(year = Year) %>%
  rename(commodity = Crop) %>% 
  rename(st_gm = Value) %>% 
  filter(newfp != 0, Variety == "All GE varieties") %>% 
  select(commodity, year, newfp, st_gm)

gm$commodity <- str_to_upper(gm$commodity)
gm$commodity <- sub("UPLAND ", "", gm$commodity)

NASS <- left_join(NASS, gm, by = c("commodity" = "commodity",
                                         "newfp" = "newfp",
                                         "year" = "year"))
rm(gm)


# Futures price data: Edit and join -----------------------------------------------------------
crop_futures <- c("corn", "soybeans", "barley", "wheat", "cotton", "rice")
crops <- crop_futures %>%
  map(~read_csv(str_c("Futures Prices/", . ,"_futures.csv")))

av_fut <- function(x){
    x <- filter(x, !(str_detect(Date, "2017"))) %>%
    filter(month(Date) <= 2 & year(Date) >= beg_year) %>%
    group_by(year(Date)) %>%
    mutate(fut = mean(Settle)) %>%
    mutate(year = year(Date)) %>%
    ungroup() %>%
    select(year,fut) %>%
    distinct()
}

crops %<>%
  map(av_fut)

crop_futures <- str_to_upper(crop_futures)

for(i in 1:length(crop_futures)){
  crops[[i]] <- mutate(crops[[i]], commodity = crop_futures[i])
  NASS <- left_join(NASS, crops[[i]], by = c("year" = "year",
                                             "commodity" = "commodity"))
}

NASS <- mutate(NASS, fut = coalesce(fut.x, fut.y, fut.x.x, fut.y.y, fut.x.x.x, fut.y.y.y)) %>%
  select(-fut.x, -fut.y, -fut.x.x, -fut.y.y, -fut.x.x.x, -fut.y.y.y)


# Modify producer price index --------------------------------------------------------------
# Modify the producer price index then add to 
index <- filter(ppi, series_id == "WPU013", period == "M13") %>%
  rename(ppi = value) %>%
  filter(year >= beg_year) %>%
  select(year, ppi)

NASS <- left_join(NASS, index, by= c("year" = "year"))


# Caluclate insurance participation -----------------------------------------------------------
NASS2 <- NASS %>%
  rename(plnt_acr = `ACRES PLANTED`) %>%
  ungroup() %>%
  group_by(statefp, countyfp, commod_id, year) %>%
  mutate(tot_liab = sum(liab),
         futures = fut/ppi*100,
         insurance = tot_liab*100/(0.75*plnt_acr*futures*avg)) %>%
  ungroup()

# Modified Palmer Drought Index Data (PMDI) -----------------------------------------------------------------------------------
pmdi_files <- c(2016:1985)

# read files into dataframe list
pmdi <- pmdi_files %>%
  map(~read_csv(str_c("NOAA/pmdi_", . ,".txt")))

# Take dataframe list and concatenate by row
pmdi <- bind_rows(pmdi) %>%
  select(STATEFP, COUNTYFP, state, year, pdmi) %>%
  rename(pmdi = pdmi)

# Join to NASS dataset
NASS2 %<>%
  left_join(pmdi, by = c("STATE_FIPS_CODE" = "STATEFP",
                         "COUNTY_CODE" = "COUNTYFP",
                         "year" = "year"))

# Join soil data ------------------------------------------------------------------------------


# Join Precipitation data ---------------------------------------------------------------------------
ppt <- read_csv("Precipitation/m_ppt.csv")
ppt_1988 <- read_csv("Precipitation/m_ppt_1988.csv")

ppt <- bind_rows(ppt_1988, ppt)

ppt$month <- as.numeric(gsub(",","", ppt$month, fixed = TRUE))

# get indicator for winter months by winter period (spanning from previous to current year)
c <- 1
ppt$winter <- 0
for(y in 1989:2016){
  ppt$winter[ppt$month < 4 & ppt$year == y | ppt$month > 10 & ppt$year == y-1] <- c
  c <- c + 1
}

# sum the total precipitation by winter period for each county
ppt <- group_by(ppt, STATEFP, COUNTYFP, winter) %>%
  mutate(wint_ppt = sum(m_ppt)) %>%
  arrange(STATEFP, COUNTYFP, year, month) %>%
  ungroup()

# replace all non-winter months with precipiation sums of 0
ppt$wint_ppt[ppt$winter == 0 | ppt$month > 10] <- 0

# spread the winter sum over current year and reduce to one observation per state / county / year
ppt <- group_by(ppt, STATEFP, COUNTYFP, year) %>%
  mutate(w_ppt = mean(wint_ppt)) %>%
  ungroup() %>%
  select(-month, -m_ppt, -winter, -wint_ppt) %>%
  filter(year > 1988) %>%
  rename(cnty_name = NAME) %>%
  distinct()

# join to NASS2 dataset
NASS2 <-  left_join(NASS2, ppt, by = c("statefp" = "STATEFP",
                        "countyfp" = "COUNTYFP",
                        "year" = "year"))

# Join Temperature data -----------------------------------------------------------------------
#dtemp <- read_csv("Weather Scripts/Data/dt_mean.csv")
dtemp <- read_csv("dt_mean.csv")
dtemp$month <- as.numeric(gsub(",","", dtemp$month, fixed = TRUE))

# Calculate Growing Degree Days (GDD)
# get indicator for growing season days
c <- 1
dtemp$grow <- 0
for(y in 1989:2016){
  dtemp$grow[dtemp$month >= 4 & dtemp$month < 7 & dtemp$year == y] <- c       #create unique factor number for the growing season in each year so that we can sum by group next
  c <- c + 1
}

# Calculate growing degree day
temp <- mutate(dtemp, gdd = dt_mean - 10)    # Calculation for number of degrees above 10 degrees celcius in a day.

# sum the total growing degree days
temp <- group_by(temp, STATEFP, COUNTYFP, grow) %>%
  mutate(s_gdd = sum(gdd)) %>%
  arrange(STATEFP, COUNTYFP, year, month) %>%
  ungroup()

# replace all non-winter months with precipiation sums of 0
temp$s_gdd[temp$grow == 0] <- 0

# spread the winter sum over current year and reduce to one observation per state / county / year
temp <- group_by(temp, STATEFP, COUNTYFP, year) %>%
  mutate(GDD = mean(s_gdd)) %>%
  ungroup() %>%
  select(STATEFP, COUNTYFP, year, GDD) %>%
  filter(year > 1988) %>%
  distinct() 

# join to NASS2 dataset
NASS2 <- left_join(NASS2, temp, by = c("statefp" = "STATEFP",
                                       "countyfp" = "COUNTYFP",
                                       "year" = "year"))


# Save final ----------------------------------------------------------------------------------
NASS3 <- unite(NASS2, st_cnty, STATE_FIPS_CODE, COUNTY_CODE, sep = "")
write_csv(NASS3, "growth_febfutures.csv")
