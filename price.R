# Pre - Amble -----------------------------------------------------------------------------
rm(list = ls())
cat("\f")
getwd()

Sys.setenv(NASSQS_TOKEN = readLines(".secret"))

#Check if packages installed, install missing packages and load all installed packages
pckgs <- c("tidyverse", "rnassqs", "stargazer", "ggplot2", "tseries")
lapply(pckgs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

deflator <- read_csv("../../Data/farmincome_wealthstatisticsdata_november2018.csv") %>%
  select(Year, ChainType_GDP_Deflator) %>%
  distinct() %>%
  filter(Year>= 1950)

# Get and clean NASS API data -------------------------------------------------------------
# List parameters of interest to feed to rnassqs package
svy_yr = 1950
params = list(source_desc = "SURVEY", 
              state_alpha = "US",
              year__GE = svy_yr,
              reference_period_desc = "MARKETING YEAR",
              freq_desc = "ANNUAL",
              sector_desc = "CROPS")

# Commodities and values of interest
commodities <- c("CORN", "SOYBEANS")          # Commodities we want to keep
vars <- c("PRICE RECEIVED")                                                                   # Specifies production variables we want to keep
nass_cleaner <- c("ADJUSTED", "PARITY", "SWEET")

# Feed parameters to rnassqs package
price <- nassqs(params = params) %>%
  filter(str_detect(short_desc, paste(commodities, collapse = '|'))) %>%
  filter(str_detect(short_desc, paste(vars, collapse = '|'))) %>%
  filter(!(str_detect(short_desc, paste(nass_cleaner, collapse = '|')))) %>%
  select(state_name, state_alpha, state_fips_code, county_code, agg_level_desc, short_desc, Value, year) %>%  # keep variables of interest
  mutate(Value = as.numeric(gsub(",","", Value, fixed = TRUE))) %>%
  mutate(year = as.numeric(year)) %>%
  left_join(deflator, by = c("year" = "Year")) %>%
  rename(gdp_df = ChainType_GDP_Deflator)

y_2017 <- filter(price,year == 2000, str_detect(short_desc, "CORN")) %>%
  select(gdp_df) %>%
  pull(gdp_df)

# Detrend price variable with GDP deflator based on year 2000 prices
price <- mutate(price, gdp_df = gdp_df/y_2017) %>%
  mutate(Value = Value/gdp_df) %>%
  select(-c(gdp_df))

# SHORT_DESC: Clean ---------------------------------------------------------------------------
# Use regex to make all SHORT_DESC observations consistent to convert to tidy form
price <- mutate(price, short_desc = sub(",[[:blank:]][A-Z]+", "", short_desc),        # Remove characters following a comma
                  short_desc = sub(",.*", "", short_desc),
                  short_desc = gsub("- ", "-", short_desc),
                  short_desc = gsub(" -", "-", short_desc),
                  short_desc = gsub("YIELD", "YIELD ", short_desc),
                  short_desc = gsub("////[[:blank:]].*", "", short_desc),
                  short_desc = gsub(" IN.*", "", short_desc))

# Convert to tidy form ------------------------------------------------------------------------
price <- separate(price, short_desc, into = c("commodity", "description"), sep = "-") %>%
  spread(key = description, value = Value) %>%
  rename(m_year = `PRICE RECEIVED`) %>%
  mutate(commodity = tolower(commodity)) %>%
  spread(key = commodity, value = m_year) %>%
  mutate(t = year - 1949,
         t2 = t^2)

detrend <- lm(corn ~ t + t2, data = price)
price$dtrnd_c <- detrend$resid

detrend <- lm(soybeans ~ t + t2, data = price)
price$dtrnd_s <- detrend$resid

price1 <- filter(price, year > 1950)
ggplot() +
  geom_line(data = price1, aes(x=year, y=dtrnd_c, group = 1), color = "black")

ggplot() +
  geom_line(data = price1, aes(x=year, y=dtrnd_s, group = 1), color = "black")

# Time series estimations
price1 <- filter(price, year < 1994)
rd <- diff(price1$dtrnd_c, differences = 1)
acf(rd)

rd <- price1$dtrnd_c
summary(r.arma <- arima(price1$dtrnd_c, order = C(1, 0, 1)))
