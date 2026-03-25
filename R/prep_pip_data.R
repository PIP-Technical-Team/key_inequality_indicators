library(fastverse)

# POP and Version
#----------------------------------------------
pop <- pipr::get_aux(table  = "pop")
pop <- pop |> 
  fmutate(year = as.numeric(levels(year))[year]) |> 
  fsubset(year >= 2000)
fst::write.fst(pop, 
               path = fs::path("data", 
                               "pop.fst"))
vrs <- pipr::get_versions()
vrs <- paste0(vrs$release_version, 
              "_",
              vrs$ppp_version)
qs::qsave(vrs, 
          file = fs::path("data", 
                               "vrs.qs"))



load_class_data <- function() {
  
  gh_user   <- "https://raw.githubusercontent.com"
  org_data  <- paste(gh_user,
                     "GPID-WB",
                     "Class",
                     "master",
                     "OutputData",
                     "CLASS.dta",
                     sep = "/")
  filename <- "CLASS.dta"
  
  temp_file <- tempfile(fileext = fs::path_ext(filename))
  req <- httr::GET(org_data,
                   # write result to disk
                   httr::write_disk(path = temp_file))
  
  haven::read_dta(temp_file)
  
}
# dt_class2 <- load_class_data() |> 
#   fselect(-economy)

# dt pip

# Total pip output

# Step 1: data ----
dt_pip <- pipr::get_stats() |> 
  qDT()# as data.table
dt_pip[
  , 
  `:=`(
    top_10_bottom_40_ratio = (decile10)/(decile1 + decile2 + decile3 + decile4), 
    top_20_bottom_20_ratio = (decile10+decile9)/(decile1 + decile2)
  )
]
dt_pip[, 
       gini := gini*100]


# Step 2: creating general objects ----
pip_countries <- unique(dt_pip$country_name) # list of countries
pip_countries_multiple_welfare_type <- 
  dt_pip[, 
         if(.N>1) .SD, 
         by = .(country_name, year, welfare_type)
  ]$country_name |>
  unique()

pip_countries_multiple_reporting_level <- 
  dt_pip[, 
         if(.N > 1) 
           .SD, 
         by = .(country_name, year, reporting_level)
  ]$country_name |> 
  unique()

pip_countries_regions <- 
  unique(dt_pip, 
         by = c("country_name", "region_code")
  )[, 
    .(country_name, region_code, region_name)
  ]
region_matching <- unique(
  dt_pip[
    ,
    .(region_name), 
    by = region_code
  ]
)
region_matching <- rowbind(region_matching, 
                           data.table(region_code = "Global", 
                                      region_name = "Global"))
# Exclude double rows
dt_pip <- 
  dt_pip[, 
         if (.N > 1) {
           .SD[!reporting_level == "rural"] 
         } else {
           .SD
         }, 
         by = .(country_name, year, welfare_type)
  ][, 
    if (.N > 1) {
      .SD[reporting_level == "national"] 
    } else {
      .SD
    }, 
    by = .(country_name, year, welfare_type)
  ]
dt_pip <- 
  dt_pip[, 
         if (.N > 1) {
           .SD[welfare_type == "income"] 
         } else {
           .SD
         }, 
         by = .(country_name, year, reporting_level)
  ]
# make deciles percentages
dec_cols <- paste0("decile", 1:10)

# Multiply each decile column by 100
dt_pip[, (paste0("decile", 1:10)) := lapply(.SD, function(x) x * 100), .SDcols = paste0("decile", 1:10)]

# Income groups
dt_class <- load_class_data() |> 
  fgroup_by(code) |>
  fmutate(max_year = fmax(year_fiscal)) |>
  fungroup() |>
  fsubset(year_fiscal == max_year) |>
  qDT()
dt_class <- dt_class |>
  as.data.table() |> 
  unique(
    by = c("economy", "incgroup_code")
  )
## Clean country names
dt_class[
  code == "CIV", 
  economy := "Cote d'Ivoire"
]
dt_class[
  code == "STP", 
  economy := "Sao Tome and Principe"
]
dt_class[
  code == "TUR", 
  economy := "Turkiye"
]
# dt_class <- haven::read_dta(
#   here::here("data", "CLASS.dta")
# )
# dt_class <- dt_class |> 
#   as.data.table() |> 
#   unique(
#     by = c("code", "incgroup")
#   )
# # Clean country names
# dt_class[
#   code == "CIV", 
#   economy := "Cote d'Ivoire"
# ]
# dt_class[
#   code == "STP", 
#   economy := "Sao Tome and Principe"
# ]
# dt_class[
#   code == "TUR", 
#   economy := "Turkiye"
# ]
dt_pip <- joyn::joyn(
  x = dt_pip, 
  y = dt_class, 
  by = c("country_code = code"), 
  keep = "left", 
  match_type = "m:1",
  reportvar = FALSE, 
  verbose = FALSE, 
  yvars = c("incgroup_code", "incgroup")
)

fst::write.fst(dt_pip, 
               path = fs::path("data", 
                               "dt_pip.fst"))

# Inequality
#---------------------

library(dplyr)
library(tidyr)
library(haven)
library(labelled)
library(zoo)
library(pipr)
library(joyn)
# Load PIP data
data <- pipr::get_stats()

data <- data %>%
  filter(poverty_line == 3.00) %>%
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
pfw <- read_dta(fs::path("data", "pfw.dta")) %>%
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
  distinct(country_code) %>%
  crossing(year = 1995:2024)
doubles_cntry <- 
  data |> 
  fgroup_by(country_code, 
            year) |> 
  fnobs() |> 
  fungroup() |> 
  fsubset(region_code > 1) |> 
  fselect(country_code, 
          year) |> 
  fmutate(welfare_type = "income")

# 
# doubles_cntry <- 
#   data |> 
#   inner_join(doubles_cntry, 
#              by = c("country_code", 
#                     "year"), 
#              relationship = "many-to-many") |> 
#   fsubset(welfare_type == "income") |> 
#   fselect(country_code, 
#           year, 
#           welfare_type)
data <- 
  anti_join(data, 
            doubles_cntry, 
            by = c("country_code", 
                   "year", 
                   "welfare_type"))
data$.joyn <- NULL
data$.joyn.1 <- NULL
data2 <- data
data <- left_join(countries, data, by = c("country_code", "year"))

classifications <- read_dta(fs::path("data", "CLASS.dta")) %>%
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
    giniever = gini*100,
    giniever = na.locf(giniever, na.rm = FALSE), # Forward fill
    giniever = na.fill(giniever, fill = NA) # Ensure no trailing NAs
  ) %>%
  mutate(
    giniever = na.locf(giniever, fromLast = TRUE) # Backward fill
  ) %>%
  ungroup()


dt_ineq <- data |> 
  fselect( country_code, year, region_code, region_name,
           welfare_time,
           welfare_type,
           #reporting_level,
           gini,
           incgroup_current, ida_current, fcv_current, giniever) |> 
  fmutate(highinequality = giniever > 40) |> 
  fmutate(ineq_type = fifelse(giniever < 30 , "Low inequality (<30)",
                              fifelse(giniever > 40, "High inequality (>40)", 
                                      "Moderate inequality (30-40)"))) |> 
  fmutate(country_count = 1L) |> 
  fmutate(incgroup_current = factor(incgroup_current, 
                                    levels = c("LIC", 
                                               "LMIC", 
                                               "UMIC", 
                                               "HIC"))) |> 
  fmutate(region_code = fifelse(region_code == "", NA, region_code)) |> 
  fmutate(incgroup_current = fifelse(incgroup_current == "", NA, incgroup_current)) |> 
  fmutate(incgroup_current = na_locf(incgroup_current),
          incgroup_current = na_focb(incgroup_current),
          
          ida_current = na_locf(ida_current),
          ida_current = na_focb(ida_current),
          
          fcv_current = na_locf(fcv_current),
          fcv_current = na_focb(fcv_current),
          
          region_code = na_locf(region_code),
          region_code = na_focb(region_code)) |>
  fungroup()


# add population var
dt_ineq <- dt_ineq |> 
  joyn::joyn(y = pop |> 
               fsubset(data_level == "national") |> 
               frename(pop = value), 
             by         = c("country_code", 
                            "year"), 
             match_type = "1:1", 
             keep       = "left", 
             verbose    = FALSE, 
             reportvar  = FALSE)
dt_ineq$region_name <- NULL
dt_ineq <- dt_ineq |> 
  joyn(y          = dt_pip |> 
         fselect(country_code, 
                 year, 
                 region_name, 
                 survey_comparability, 
                 comparable_spell, 
                 reporting_level), 
       by         = c("country_code", "year"), 
       match_type = "1:1", 
       keep       = "left", 
       reportvar  = FALSE, 
       verbose    = FALSE) |> 
  fgroup_by(country_code) |> 
  fmutate(region_name = na_locf(region_name), 
          region_name = na_focb(region_name)) |> 
  fungroup()





fst::write.fst(dt_ineq, 
               path = fs::path("data", 
                               "dt_ineq.fst"))




