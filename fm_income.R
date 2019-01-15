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


fm_inc <- read_csv("../../Data/farmincome_wealthstatisticsdata_november2018.csv") %>%
  filter(State == "US", 
         str_detect(VariableDescriptionTotal, "Net farm income$"),
         Year >= 1950) %>%
  rename(gdp_df = ChainType_GDP_Deflator) %>%
  mutate(Income = Amount/gdp_df)

ggplot() +
  geom_line(data = fm_inc, aes(x=Year, y=Income, group = 1), color = "black")
