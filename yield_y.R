# Pre - Amble -----------------------------------------------------------------------------
Sys.setenv(NASSQS_TOKEN = readLines(".secret"))

# Get and clean NASS API data -------------------------------------------------------------
# List parameters of interest to feed to rnassqs package
svy_yr = c(1950:2017)
params = list(source_desc = "SURVEY", 
              state_alpha = "US",
              year = svy_yr,
              reference_period_desc = c("YEAR"),
              freq_desc = "ANNUAL",
              sector_desc = "CROPS")

# Commodities and values of interest
commodities <- c("CORN", "SOYBEANS")          # Commodities we want to keep
vars <- c("YIELD")                                                                   # Specifies production variables we want to keep
nass_cleaner <- c("ADJUSTED", "PARITY", "SWEET", "SILAGE")

# Feed parameters to rnassqs package
yield <- nassqs(params = params) %>%
  filter(str_detect(short_desc, paste(commodities, collapse = '|'))) %>%
  filter(str_detect(short_desc, paste(vars, collapse = '|'))) %>%
  filter(!(str_detect(short_desc, paste(nass_cleaner, collapse = '|')))) %>%
  select(state_name, state_alpha, state_fips_code, county_code, agg_level_desc, short_desc, Value, year) %>%  # keep variables of interest
  mutate(Value = as.numeric(gsub(",","", Value, fixed = TRUE))) %>%
  mutate(year = as.numeric(year))

# SHORT_DESC: Clean ---------------------------------------------------------------------------
# Use regex to make all SHORT_DESC observations consistent to convert to tidy form
yield1 <- mutate(yield, short_desc = sub(",[[:blank:]][A-Z]+", "", short_desc),        # Remove characters following a comma
                short_desc = sub(",.*", "", short_desc),
                short_desc = gsub("- ", "-", short_desc),
                short_desc = gsub(" -", "-", short_desc),
                short_desc = gsub("YIELD", "YIELD ", short_desc),
                short_desc = gsub("////[[:blank:]].*", "", short_desc),
                short_desc = gsub(" IN.*", "", short_desc))

# Convert to tidy form ------------------------------------------------------------------------
yield1 <- separate(yield1, short_desc, into = c("commodity", "description"), sep = "-") %>%
  spread(key = description, value = Value) %>%
  rename(m_year = `YIELD `) %>%
  mutate(commodity = tolower(commodity)) %>%
  spread(key = commodity, value = m_year) %>%
  mutate(t = year - 1949,
         t2 = t^2)

# This is to try to generate the scaled yield data ----------------------------------------------
p_c_2017 <- filter(yield1,year == 2017) %>%
  select(corn) %>%
  pull(corn)

p_s_2017 <- filter(yield1,year == 2017) %>%
  select(soybeans) %>%
  pull(soybeans)

detrend_y <- lm(corn ~ t + t2, data = yield1)
yield1$dtrnd_y_c <- p_c_2017*(1 + detrend_y$resid/detrend_y$fitted.values)

detrend_y <- lm(soybeans ~ t + t2, data = yield1)
yield1$dtrnd_y_s <- p_s_2017*(1 + detrend_y$resid/detrend_y$fitted.values)

yield1 <- filter(yield1, year > 1950)
ggplot() +
  geom_line(data = yield1, aes(x=year, y=dtrnd_y_c, group = 1), color = "black")

ggplot() +
  geom_line(data = yield1, aes(x=year, y=dtrnd_y_s, group = 1), color = "black")
