
rm(list = ls())
library(dplyr)
library(tidyr)
library(haven)
library(labelled)
library(zoo)
library(pipr)

#################################################################################
# Step 1: Data Prep: Gini Coefficients from PIP
#################################################################################
# Load PIP data
data <- get_stats()

data <- data %>%
  filter(poverty_line == 2.15) %>%
  filter(reporting_level == "national" | country_code %in% c("ARG")) %>%
  filter(year >= 1995) %>%
  rename(welfare_type_b = welfare_type) %>%
  mutate(welfare_type_b = to_factor(welfare_type_b)) %>%
  mutate(welfare_type = recode(welfare_type_b, 
                               `0` = "income", `1` = "consumption")) %>%
  select(-welfare_type_b)


#################################################################################
# Step 2: PREPARING PREFERRED ESTIMATES WHEN BOTH INCOME AND CONSUMPTION ARE AVAILABLE
# The necessary variable is in the frameworks dataset (provided by Minh)
# Source: "https://github.com/PIP-Technical-Team/aux_pfw/blob/DEV/pfw.dta"
#################################################################################
pfw <- read_dta("pfw_v11.dta") %>%
  filter(survey_coverage == "N" | code %in% c("ARG")) %>%
  filter(display_cp == 1) %>%
  rename(country_code = code, year_temp  = rep_year) %>%
  mutate(welfare_type = ifelse(datatype %in% c("C", "c"), "consumption", "income")) %>%
  select(-year) %>%
  rename(year = year_temp) %>%
  distinct()

data <- left_join(data, pfw, by = c("country_code", "year", "welfare_type"))

data <- data %>%
  filter(year >= 2000) %>%
  group_by(country_code) %>%
  mutate(weightall = 1 / n()) %>%
  group_by(country_code, welfare_type) %>%
  mutate(weighttype = 1 / n()) %>%
  ungroup()


#################################################################################
# Step 3: Expand to include all years 1995-2000 for each country, 
#         add income group, FCV, IDA groups and population estimates
# Class.dta source: https://github.com/GPID-WB/Class/tree/master/OutputData
#################################################################################
countries <- data %>%
  distinct(country_name, country_code) %>%
  crossing(year = 1995:2024)

data <- left_join(countries, data, by = c("country_code", "year"))

classifications <- read_dta("CLASS.dta") %>%
  rename(year = year_data, country_code = code) %>%
  select(country_code, year, starts_with("incgroup"), starts_with("ida"), starts_with("fcv"))

data <- left_join(data, classifications, by = c("country_code", "year"))


#################################################################################
# Step 4: Create the necessary variables and clean up data
# keep years 2000 until the most recent global reference year
#################################################################################

# Order data by country and year for time series operations
data <- data %>%
  arrange(country_code, year)

# Forward fill and backward fill missing values for gini
data <- data %>%
  group_by(country_code) %>%
  mutate(
    giniever = gini,
    giniever = na.locf(giniever, na.rm = FALSE), # Forward fill
    giniever = na.fill(giniever, fill = NA) # Ensure no trailing NAs
  ) %>%
  mutate(
    giniever = na.locf(giniever, fromLast = TRUE) # Backward fill
  ) %>%
  ungroup()

# Select specific variables
data <- data %>%
  select( country_code, year, region_code, region_name, welfare_time, welfare_type, gini, 
         incgroup_current, ida_current, fcv_current, giniever) %>%
  filter(year >= 2000 & year <= 2022) %>%
  mutate(highinequality = giniever > 0.40)


#################################################################################
# Step 5: Number of countries in the world with high inequality
#################################################################################


data$numberofcountries <- 1

world_highinequality <- data %>%
  group_by(year, highinequality) %>%
  summarise(numberofcountries = sum(numberofcountries), .groups = 'drop')
