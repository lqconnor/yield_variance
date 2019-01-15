# Pre - Amble -----------------------------------------------------------------------------
rm(list = ls())
cat("\f")
getwd()

Sys.setenv(NASSQS_TOKEN = readLines(".secret"))

#Check if packages installed, install missing packages and load all installed packages
pckgs <- c("tidyverse", "rnassqs", "stargazer", "tseries")
lapply(pckgs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

yr <- 1970
yr_adj  <- yr-1

deflator <- read_csv("../../Data/farmincome_wealthstatisticsdata_november2018.csv") %>%
  select(Year, ChainType_GDP_Deflator) %>%
  distinct() %>%
  filter(Year>= yr)

# Get and clean NASS API data -------------------------------------------------------------
# List parameters of interest to feed to rnassqs package
svy_yr = c(yr:2017)
params = list(source_desc = "SURVEY", 
              state_alpha = "US",
              year = svy_yr,
              reference_period_desc = c("MARKETING YEAR"),
              freq_desc = "ANNUAL",
              sector_desc = "CROPS")

# Commodities and values of interest
commodities <- c("CORN", "SOYBEANS")          # Commodities we want to keep
vars <- c("PRICE RECEIVED")                                                                   # Specifies production variables we want to keep
nass_cleaner <- c("ADJUSTED", "PARITY", "SWEET", "SILAGE")

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

y_2017 <- filter(price,year == 2017, str_detect(short_desc, "CORN")) %>%
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
  mutate(t = year - yr_adj,
         t2 = t^2)

# This is to try to generate the scaled yield data ----------------------------------------------
p_c_2017 <- filter(price,year == 2017) %>%
  select(corn) %>%
  pull(corn)

p_s_2017 <- filter(price,year == 2017) %>%
  select(soybeans) %>%
  pull(soybeans)

detrend <- lm(corn ~ t + t2, data = price)
price$dtrnd_c <- p_c_2017*(1 + detrend$resid/detrend$fitted.values)

detrend <- lm(soybeans ~ t + t2, data = price)
price$dtrnd_s <- p_s_2017*(1 + detrend$resid/detrend$fitted.values)

#price1 <- filter(price, year > 1950)
ggplot() +
  geom_line(data = price, aes(x=year, y=dtrnd_c, group = 1), color = "black")

ggplot() +
  geom_line(data = price, aes(x=year, y=dtrnd_s, group = 1), color = "black")

# Time series estimations -----------------------------------------------
# Pre 1994
price1 <- filter(price, year < 1994)
rd <- diff(price1$corn, differences = 2)
adf.test(rd)
acf(rd)

summary(r.arma <- arima(price1$corn, order = c(1, 0, 1)))
# The best fit (in terms of AIC) for the differenced, detrended, inflation adjusted corn price AFTER 1994 is the ARIMA(3,3,4)  
arima(rd, order = c(0, 10, 0))

# The best fit (in terms of AIC) for the differenced, detrended, inflation adjusted corn price BEFORE 1994 is the ARIMA(2,5,0)  
# arima(0,10,0) better or the less that 1994 double differenced version
# Dickey Fuller Test


# Time series estimations
# Post 2000
price2 <- filter(price, year > 2005)
rd <- diff(price2$corn, differences = 2)
adf.test(rd)
acf(rd)
arima(rd, order = c(1, 0, 3))


# Analysis for soybean prices ---------------------------------------
# Time series estimations
# Pre 1994
price1 <- filter(price, year < 1994)
rd <- diff(price1$soybeans, differences = 2)
adf.test(rd)
acf(rd)
arima(rd, order = c(0, 10, 0))

# The best fit (in terms of AIC) for the differenced, detrended, inflation adjusted corn price BEFORE 1994 is the ARIMA(2,5,0)  
# arima(0,10,0) better or the less that 1994 double differenced version
# Dickey Fuller Test


# Time series estimations
# Post 2000
price2 <- filter(price, year > 2000)
rd <- diff(price2$dtrnd_s, differences = 3)
adf.test(rd)
acf(rd)
arima(rd, order = c(0, 4, 0))
