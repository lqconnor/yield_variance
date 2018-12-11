# Pre - Amble -----------------------------------------------------------------------------
rm(list = ls())
cat("\f")
getwd()

Sys.setenv(NASSQS_TOKEN = readLines(".secret"))

#Check if packages installed, install missing packages and load all installed packages
pckgs <- c("tidyverse", "rnassqs", "stargazer")
lapply(pckgs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# Get and clean NASS API data -------------------------------------------------------------
# List parameters of interest to feed to rnassqs package
svy_yr = 1988
commodities <- c("CORN, GRAIN, IRRIGATED - ACRES HARVESTED", 
                 "SOYBEANS, IRRIGATED - ACRES HARVESTED")           # Commodities we want to keep
yield_c <- map_df(commodities, function(x) {
                  params = list(source_desc = "SURVEY", 
                            year__GE = svy_yr,
                            short_desc = x,
                            agg_level_desc = "COUNTY",
                            reference_period_desc = "YEAR")
                  nassqs(params = params)}
)

yield_c <- mutate(yield_c, short_desc = gsub("IRRIGATED - ACRES HARVESTED", "- IRRIGATED ACRES HARVESTED", short_desc),
                  short_desc = gsub(", -", " -", short_desc))

# Don't run unless you have to -----------------------------------------------------
# Pulls harvested acres for corn and soybeans
start_time <- Sys.time()
svy_yr = c(list(1987:1990), list(1991:2002), list(2003:2016))
prod_c <- map_df(svy_yr, function(x) {
  params = list(source_desc = "SURVEY",
                year = x,
                short_desc = c("CORN, GRAIN - ACRES HARVESTED", "SOYBEANS - ACRES HARVESTED"),
                agg_level_desc = "COUNTY",
                reference_period_desc = "YEAR")
  nassqs(params = params)}
)
end_time <- Sys.time()
end_time - start_time

# Combine and clean ---------------------------------------------------------------
commods <- bind_rows(yield_c, prod_c) %>%
  select(state_name, state_alpha, state_fips_code, county_code, location_desc, asd_desc, asd_code, agg_level_desc, short_desc, Value, year) %>%  # keep variables of interest
  mutate(Value = as.numeric(gsub(",","", Value, fixed = TRUE))) %>%
  mutate(short_desc = sub(",[[:blank:]][A-Z]+", "", short_desc),        # Remove characters following a comma
                  short_desc = sub(",.*", "", short_desc),
                  short_desc = gsub("- ", "-", short_desc),
                  short_desc = gsub(" -", "-", short_desc),
                  short_desc = gsub("YIELD", "YIELD ", short_desc),
                  short_desc = gsub("////[[:blank:]].*", "", short_desc),
                  short_desc = gsub(" IN.*", "", short_desc))

# Convert to tidy form ------------------------------------------------------------------------
commods <- separate(commods, short_desc, into = c("commodity", "description"), sep = "-")
combined <- filter(commods, county_code == "998")
commods <- filter(commods, county_code != "998")
  
commods <- spread(commods, key = description, value = Value) %>%
  rename(acres = `ACRES HARVESTED`) %>%
  rename(irr_acr = `IRRIGATED ACRES HARVESTED`) %>%
  arrange(state_fips_code, county_code, commodity, year) %>%
  #add_count(state_fips_code, commodity, year) %>%
  #rename(cnty_cnt = n) %>%
  group_by(state_fips_code, commodity, asd_code, year) %>%
  mutate(missings = sum(is.na(irr_acr))) %>%
  ungroup()
  #group_by(state_fips_code, commodity, asd_code) %>%
  #mutate(all_cnty = max(cnty_cnt)) %>%
  #ungroup() %>%
  #mutate(irr_acr = replace_na(irr_acr, 0)) %>%

# Include combined counties --------------------------------------------------------------------
combined <- select(combined, state_fips_code, asd_code, commodity, year, Value) %>%
  rename(comb_irr = Value)
commods <- left_join(commods, combined, by = c("state_fips_code" = "state_fips_code",
                                               "asd_code" = "asd_code",
                                               "commodity" = "commodity",
                                               "year" = "year")) %>%
  mutate(distribute = comb_irr/missings )

commods$irr_acr[is.na(commods$irr_acr)] <- commods$distribute[is.na(commods$irr_acr)]
commods <- mutate(commods, prop_irr = irr_acr/acres)
commods <- select(commods, state_fips_code, county_code, commodity, year, irr_acr, prop_irr) %>%
  mutate(irr_acr = replace_na(irr_acr, 0)) %>%
  unite(st_cnty, state_fips_code, county_code, sep = "")
commods$prop_irr[commods$prop_irr > 1] <- 1
commods <- mutate(commods, prop_irr = replace_na(prop_irr, 0))

write_csv(commods, "irrigated.csv")