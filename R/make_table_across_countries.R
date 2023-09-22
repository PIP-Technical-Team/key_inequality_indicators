

make_table_across_countries <- function(
    dt, 
    country_name_vec = c("Angola", "South Africa"), 
    ineq_indicators  = c(
      "Mean", 
      "Median", 
      "Mld", 
      "Gini", 
      "Decile1", 
      "Decile2", 
      "Decile3", 
      "Decile4", 
      "Decile5", 
      "Decile6", 
      "Decile7", 
      "Decile8", 
      "Decile9", 
      "Decile10",
      "Top10_bottom40", 
      "Top20_bottom20"
    ), 
    years_selected = c("All", "Latest")
){
  dt <- copy(dt)
  # Check Inputs ----
  stopifnot(is.data.table(dt))
  stopifnot(country_name_vec %chin% pip_countries)
  years_selected <- match.arg(years_selected)
  if(!ineq_indicators %in% c(
    "Mean", 
    "Median", 
    "Mld", 
    "Gini", 
    "Decile1", 
    "Decile2", 
    "Decile3", 
    "Decile4", 
    "Decile5", 
    "Decile6", 
    "Decile7", 
    "Decile8", 
    "Decile9", 
    "Decile10",
    "Top10_bottom40", 
    "Top20_bottom20"
  ) |> all()) stop("Select appropriate inequality indicators")
  
  #dt <- dt[, if (.N > 1) .SD[reporting_level == "national"] else .SD, by = .(country_name, year)]
  
  # Prepare Data ----
  selected_cols <- c(
    "Country", 
    "Region", 
    "Year", 
    "Reporting_level",
    "Welfare_type",
    ineq_indicators
  )
  setnames(
    dt, 
    old = c("country_name", "region_code"), 
    new = c("country", "region")                           # Clean variable names
  )
  setnames(
    dt, 
    stringr::str_to_title(names(dt))             # Capitalize column names
  )
  
  dt <- dt[
    Country %chin% country_name_vec,         # filter by country
    ..selected_cols                             # Select id vars and inequality indicators
  ]
  
  dt[
    ,                                                                                           
    (names(dt)) := lapply(                       # Round to 2 decimals
      .SD, 
      function(x){if(is.numeric(x)) round(x, 2) else x}
    )
  ]
  

  
  if(years_selected == "Latest"){
    dt <- dt[, .SD[which.max(Year)], by = Country]
  }

  list_of_columns <- lapply(
    names(dt), function(col_name) {              # Make the columns list items for plotly
      return(dt[[col_name]])               
    })
  
  setorder(dt, Country, Region, -Year, Reporting_level, Welfare_type)
  
  # Plot Table ----
  
  p <- plot_ly(
    type = 'table',
    #columnwidth = c(100, 100),
    header = list(
      values = dt |> colnames(), # column names vector
      align = c("center", "center"),
      line = list(width = 1, color = 'black'),
      fill = list(color = c("grey", "grey")),
      font = list(family = "Arial", size = 14, color = "white")
    ),
    cells = list(
      values = list_of_columns,
      align = c("center", "center"),
      line = list(color = "black", width = 1),
      font = list(family = "Arial", size = 12, color = c("black"))
    ))
  
  p
  
}


# Copy paste in below

```{r plot-table}

make_table_across_countries <- function(
    dt,
    country_name_vec = c("Angola", "South Africa"),
    ineq_indicators  = c(
      "Mean",
      "Median",
      "Mld",
      "Gini",
      "Decile1",
      "Decile2",
      "Decile3",
      "Decile4",
      "Decile5",
      "Decile6",
      "Decile7",
      "Decile8",
      "Decile9",
      "Decile10",
      "Top10_bottom40",
      "Top20_bottom20"
    ),
    years_selected = c("All", "Latest")
){
  dt <- copy(dt)
  # Check Inputs ----
  stopifnot(is.data.table(dt))
  stopifnot(country_name_vec %chin% pip_countries)
  years_selected <- match.arg(years_selected)
  if(!ineq_indicators %in% c(
    "Mean",
    "Median",
    "Mld",
    "Gini",
    "Decile1",
    "Decile2",
    "Decile3",
    "Decile4",
    "Decile5",
    "Decile6",
    "Decile7",
    "Decile8",
    "Decile9",
    "Decile10",
    "Top10_bottom40",
    "Top20_bottom20"
  ) |> all()) stop("Select appropriate inequality indicators")
  
  #dt <- dt[, if (.N > 1) .SD[reporting_level == "national"] else .SD, by = .(country_name, year)]
  
  # Prepare Data ----
  selected_cols <- c(
    "Country",
    "Region",
    "Year",
    "Reporting_level",
    "Welfare_type",
    ineq_indicators
  )
  setnames(
    dt,
    old = c("country_name", "region_code"),
    new = c("country", "region")                           # Clean variable names
  )
  setnames(
    dt,
    stringr::str_to_title(names(dt))             # Capitalize column names
  )
  
  dt <- dt[
    Country %chin% country_name_vec,         # filter by country
    ..selected_cols                             # Select id vars and inequality indicators
  ]
  
  dt[
    ,
    (names(dt)) := lapply(                       # Round to 2 decimals
      .SD,
      function(x){if(is.numeric(x)) round(x, 2) else x}
    )
  ]
  
  
  
  if(years_selected == "Latest"){
    dt <- dt[, .SD[which.max(Year)], by = Country]
  }
  
  list_of_columns <- lapply(
    names(dt), function(col_name) {              # Make the columns list items for plotly
      return(dt[[col_name]])
    })
  
  setorder(dt, Country, Region, -Year, Reporting_level, Welfare_type)
  
  # Plot Table ----
  
  p <- plot_ly(
    type = 'table',
    #columnwidth = c(100, 100),
    header = list(
      values = dt |> colnames(), # column names vector
      align = c("center", "center"),
      line = list(width = 1, color = 'black'),
      fill = list(color = c("grey", "grey")),
      font = list(family = "Arial", size = 14, color = "white")
    ),
    cells = list(
      values = list_of_columns,
      align = c("center", "center"),
      line = list(color = "black", width = 1),
      font = list(family = "Arial", size = 12, color = c("black"))
    ))
  
  p
  
}

renderPlotly({
  table <- make_table_across_countries(
    dt = dt_pip,
    country_name_vec = input$select_countries_table,
    ineq_indicators = input$ineq_indicator_table,
    years_selected = input$all_years_table
  )
  table
})

```




