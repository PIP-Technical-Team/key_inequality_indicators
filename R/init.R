# Step 1: install packages ----
pacman::p_load(
  data.table, 
  here, 
  ggplot2, 
  plotly, 
  flexdashboard, 
  tidyverse, 
  pak
)
if(!find.package("pipr")>1)
  pak::pkg_install(
    "worldbank/pipr@httr2"
)

# Step 2: data ----
dt_pip <- pipr::get_stats()  # install entire data
dt_pip <- data.table(dt_pip) # as data.table
dt_pip[
  , 
  `:=`(
    top10_bottom40 = (decile10)/(decile1 + decile2 + decile3 + decile4), 
    top20_bottom20 = (decile10+decile9)/(decile1 + decile2)
  )
]

# Step 3: creating general objects ----
pip_countries                          <- dt_pip$country_name |> unique() # list of countries
pip_countries_multiple_welfare_type    <- dt_pip[, if(.N>1) .SD, by = .(country_name, year, welfare_type)]$country_name |> unique()
pip_countries_multiple_reporting_level <- dt_pip[, if(.N>1) .SD, by = .(country_name, year, reporting_level)]$country_name |> unique()
pip_countries_regions                  <- unique(dt_pip, by = c("country_name", "region_code"))[, .(country_name, region_code)]
# Exclude double rows
dt_pip <- dt_pip[, if (.N > 1) .SD[!reporting_level == "rural"] else .SD, by = .(country_name, year, reporting_level)][, if (.N > 1) .SD[reporting_level == "national"] else .SD, by = .(country_name, year, reporting_level)]
dt_pip <- dt_pip[, if (.N >1) .SD[welfare_type == "income"] else .SD, by = .(country_name, year, reporting_level)]
