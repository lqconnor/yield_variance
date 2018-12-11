# Pre - Amble -----------------------------------------------------------------------------
rm(list = ls())
cat("\f")
getwd()

Sys.setenv(NASSQS_TOKEN = readLines(".secret"))

#Check if packages installed, install missing packages and load all installed packages
pckgs <- c("tidyverse", "rnassqs", "stargazer", "ggplot2")
lapply(pckgs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

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
  mutate(Value = as.numeric(gsub(",","", Value, fixed = TRUE)))

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
  spread(key = commodity, value = m_year)

ggplot() +
  geom_line(data = price, aes(x=year, y=corn, group = 1), color = "black")

ggplot() +
  geom_line(data = price, aes(x=year, y=soybeans, group = 1), color = "black")