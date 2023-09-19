
create_data_filtered <- function(
    dt, 
    countries = c("South Africa", "Colombia"), 
    regions   = ("SSA"), 
    columns
){
  
  # Argument Checks
  stopifnot(is.data.table(dt))
  if(is.null(countries) & is.null(regions)) stop("Must choose either countries or regions")
  
  # Use copy of dt
  dt_use <- copy(dt)
  
  # Countries
  if(!is.null(countries)){
    
    dt_use <- dt_use[country_name %chin% countries]
    
  }
  
  # Region
  if(is.null(countries)){
    
    dt_use <- dt_use[region_code %chin% regions]
    
  }
  
  # Columns
  dt_use <- dt_use[
    , 
    .(country_name, year, reporting_level, region_code, welfare_type, comparable_spell, columns)
  ]
  
  # return
  return(dt_use)
  
}


create_data_imputed_by_countries <- function(
    dt, 
    countries_selected  = c("South Africa", "Colombia", "United States"), 
    year_selected       = 1998, 
    indicator           = c("Gini"), 
    welfare             = NULL, # if 'consumption' or 'income' then use *only* those
    window_length       = 2 
    
){
  
  # Argument Checks ----
  stopifnot(is.data.table(dt))
  if(
    is.null(countries_selected) | 
    is.null(year)               | 
    is.null(indicator)          | 
    is.null(indicator)
  ) stop ( "Must choose" )
  if(
    length(year) > 1            | 
    length(indicator) > 1 
  ) stop ( "Can only select one year and one indicator" )
  
  # Use copy of dt
  dt_use <- copy( dt )
  
  # Rename 
  setnames(
    dt_use,
    old     = c(tolower(indicator)), 
    new     = c("Indicator")
  )
  
  # Filter ----
  if( !is.null(welfare) ) { # welfare type
    
    dt_use <- dt_use[
      welfare %chin% welfare_type
    ]
    
  }
  
  # Filter by year and country name
  dt_use <- dt_use[        # window length
    year %chin% c( (year_selected - window_length): (year_selected + window_length))
  ]
  dt_use <- dt_use[
    country_name %chin% countries_selected
  ]
  
  # Fill Missing ----
  dt_use <- joyn::merge(
    CJ(
      country_name = dt_use$country_name |> unique(), 
      year         = dt_use$year         |> unique()
    ),
    dt_use, 
    by = c("country_name", "year")
  )
  dt_use[, missing := ifelse(is.na(Indicator), TRUE, FALSE)] # missing indicator
  dt_use <- dt_use[, report := NULL]
  
  # If missing, impute forward or backwards
  setorder(dt_use, country_name, year) # order by year
  dt_forward  <- dt_use[, .SD[c((window_length + 1):.N)], by = country_name][missing == FALSE][, .(Forward = min(year)-year_selected), by = country_name]
  dt_backward <- dt_use[, .SD[c(1:(window_length + 1))], by = country_name][missing == FALSE][, .(Backward = abs(max(year)-year_selected)), by = country_name]
  dt_use <- joyn::merge(
    dt_use, 
    dt_forward, 
    by = c("country_name"), 
    yvars = T, 
    match_type = "m:1", 
    keep = "left"
  )
  dt_use <- dt_use[, report := NULL]
  dt_use <- joyn::merge(
    dt_use, 
    dt_backward, 
    by = c("country_name"), 
    yvars = T, 
    match_type = "m:1", 
    keep = "left"
  )
  dt_use <- dt_use[, report := NULL]
  dt_use[is.na(Forward), Forward := 100]
  dt_use[is.na(Backward), Backward := 100]
  dt_use[, Impute_Forward := (Backward >= Forward)]
  
  # Do imputation
  dt_use[, Indicator := {
    # Forward fill for Impute_Forward == FALSE
    Indicator[Impute_Forward == FALSE] <- nafill(Indicator[Impute_Forward == FALSE], type = "locf")
    # Backward fill for Impute_Forward == TRUE
    Indicator[Impute_Forward == TRUE] <- nafill(Indicator[Impute_Forward == TRUE], type = "nocb")
    # Return the modified Indicator
    Indicator
  }, by = country_name]
  
  # Keep only selected year
  dt_use <- dt_use[ year == year_selected]
  # Remove columns
  dt_use[, c("Forward", "Backward") := NULL]
  
  setnames(
    dt_use, 
    old = "Indicator", 
    new = tolower(indicator)
  )
  
  # Return
  return(dt_use)
  
}

dt_use <- create_data_imputed_by_countries(
  dt = dt_pip
)


create_data_ranked_by_countries <- function(
    dt, 
    countries_selected  = c("South Africa", "Colombia"), 
    year_selected       = 2020, 
    indicator           = c("Gini"), 
    welfare             = NULL, # if 'consumption' or 'income' then use *only* those
    window_length       = 2 
    
){

  # Create Imputed data set ----
  dt_use <- create_data_imputed_by_countries(
    dt                 = dt_pip, 
    countries_selected = countries_selected, 
    year_selected      = year_selected, 
    indicator          = indicator, 
    welfare            = welfare, 
    window_length      = window_length
  )
  
  # Rename
  setnames(
    dt_use,
    old     = c(tolower(indicator)),
    new     = c("Indicator")
  )

  # Rank by country
  dt_use[, Rank := frank(Indicator)]
  
  setnames(
    dt_use,
    old = "Indicator",
    new = tolower(indicator)
  )
  #Return
  return(dt_use)
  
}




create_data_ranked_all_indicators <- function(
    dt, 
    country_selected  = c("United States"), 
    year_selected       = 2000, 
    welfare             = NULL, # if 'consumption' or 'income' then use *only* those
    window_length       = 2 
){
  

  
}














create_data_filter_consumption


create_data_filter_represen



