
plot_indicator_ranking_change <- function(
    dt, 
    ineq_indicator = c(
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
    countries_selected = NULL, 
    region_selected = NULL, 
    start_year = 2000
){
  
  if(is.null(countries_selected) & is.null(region_selected))stop("Must choose either country or region")
  if(length(ineq_indicator)>1) stop("Can only select one indicator")
  # as data frame
  dt_ind <- copy(dt)
  # Step 1: Expand dt for all years ---
  setnames(dt_ind, new = c("indicator"), old = c(tolower(ineq_indicator)))
  dt_ind <- dt_ind[, .(country_name, year, region_code, indicator)]
  dt_expanded <- merge(
    CJ(
      country_name = dt_ind$country_name |> unique(), 
      year         = dt_ind$year         |> unique()
    ),
    dt_ind, 
    by = c("country_name", "year")
  )
  # Step 2: Impute missing indicator values ----
  ## by taking neares neighbour, 
  ## at most 2 year difference
  dt_expanded[, missing := ifelse(is.na(indicator), TRUE, FALSE)] # missing indicator
  dt_expanded[, `:=` (missing_5 = 
                        missing &                          #  current year
                        shift(missing, type="lag", n=1) &  #  previous year 1
                        shift(missing, type="lag", n=2) &  #  previous year 2
                        shift(missing, type="lead", n=1) & #  next year 1
                        shift(missing, type="lead", n=2)), #  next year 2
              by = country_name]
  dt_expanded[
    , indicator_use := {
      fcase(
        
        missing == FALSE, 
        indicator, 
        
        missing == TRUE & shift(missing, type = "lag", n = 1) == FALSE, 
        shift(indicator, type = "lag", n =1), 
        
        missing == TRUE & shift(missing, type = "lag", n = 1) == TRUE & shift(missing, type = "lead", n = 1) ==FALSE, 
        shift(indicator, type = "lead", n =1), 
        
        missing == TRUE & shift(missing, type = "lag", n = 1) == TRUE & shift(missing, type = "lead", n = 1) ==TRUE & shift(missing, type = "lag", n = 2) ==FALSE, 
        shift(indicator, type = "lag", n =2), 
        
        missing == TRUE & shift(missing, type = "lag", n = 1) == TRUE & shift(missing, type = "lead", n = 1) ==TRUE & shift(missing, type = "lag", n = 2) ==TRUE & shift(missing, type = "lead", n = 2) ==FALSE, 
        shift(indicator, type = "lead", n =2)
        
      )
    }
  ]
  
  # Step 3: Calculate Ranks
  end_year <- start_year + 5
  dt_expanded <- dt_expanded[year == start_year | year == end_year]
  dt_expanded <- dt_expanded[missing_5 == FALSE, .(country_name, year, indicator_use, region_code)]
  
  dt_initial_rank <- dt_expanded[year == start_year, .(Rank = frank(-indicator_use), country_name), by = year]
  dt_final_rank <- dt_expanded[year == end_year, .(Rank = frank(-indicator_use), country_name), by = year]
  dt_rank <- rbindlist(
    list(
      dt_initial_rank, 
      dt_final_rank
    ), 
    use.names = T
  )
  dt_expanded <- merge(
    dt_expanded, 
    dt_rank, 
    by = c("country_name", "year")
  )
  dt_expanded <- dt_expanded[, if(.N >1) .SD , by = country_name] # keep only countries appearing in both years
  
  dt_expanded <- joyn::merge(
    dt_expanded, 
    pip_countries_regions, 
    by = c("country_name"), 
    match_type = c("m:1"), 
    keep = c("left"), 
    update_NAs = TRUE, 
    verbose = F, 
    reportvar = NULL
  )
  
  
  # Step 4: Find countries ----
  ## if list of countries is supplied, use that as `country_vec`
  ## if region is supplied, create `country_vec` using all the countries in that region
  
  if(is.null(countries_selected)){ 
    # Using Region - no countries to as benchmark
    
    countries_selected <- dt_expanded[
      country_name %chin% unique(
        dt_expanded[region_code == region_selected]$country_name
      )
    ]$country_name |> unique()
    # select additional countries
    dt_expanded <- dt_expanded[country_name %chin% countries_selected]
    
    g <- dt_expanded |> 
      ggplot(
        aes(x = year, y = Rank, group = country_name)
      ) + 
      geom_line(
        aes( color = country_name ), size = 2, alpha = 1
      )+
      geom_point(color = "#FFFFFF", size = 4) +
      geom_point(aes(color = country_name, alpha = 1), size = 4) +
      geom_point(color = "#FFFFFF", size = 1) +
      scale_y_reverse(
        breaks = c(seq(min(dt_expanded$Rank), max(dt_expanded$Rank)+1, by = 5))
      ) +
      scale_x_continuous(breaks = seq(from = start_year-1, to = end_year + 1, by = 1)) +
      labs(x = "Year",
           y = "Rank",
           title = paste("Rank of highest to lowest by", ineq_indicator),
           subtitle = "Country rankings over time") +
      geom_point(color = "#FFFFFF", size = 1) +
      geom_text(data = dt_expanded[year == min(year)],
                aes(label = country_name, x = year-0.1), hjust = .85, fontface = "bold", color = "#888888", size = 4) +
      geom_text(data =dt_expanded[year == max(year)],
                aes(label = country_name, x = year+0.1) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
      coord_cartesian(
        ylim = c(max(dt_expanded$Rank), 1), 
        xlim = c(min(dt_expanded$year) - 0.3,max(dt_expanded$year) + 0.3 )
      ) + 
      my_theme()+
      geom_vline(xintercept = c(start_year, end_year), linetype = "solid", color = "darkgrey")
    
    
    
  } else{
    
    
    # Using Countries - other countries as benchmark
    #flag_countries <- dt_expanded[country_name %chin% countries_selected]$country_name |> unique()
    
    #countries_selected <- dt_expanded[
    #  country_name %chin% unique(
    #    c(dt_expanded[Rank %chin% round(145/c(1:5, 145))]$country_name, 
    #      countries_selected)
    #  )
    #]$country_name |> unique()
    
    #dt_expanded[, `:=`(
    #  flag = country_name %chin% flag_countries
    #)
    #]
    #dt_expanded[, `:=`(
    #  country_col = fifelse(flag == T, country_name, NA)
    #)
    #]
    
    # select additional countries
    dt_expanded <- dt_expanded[country_name %chin% countries_selected]
    setnames(
      dt_expanded, 
      old = c("country_name", "year", "indicator_use", "region_code"), 
      new = c("Country", "Year", "Indicator", "Region")
    )
    print(dt_expanded)
    g <- dt_expanded |> 
      ggplot(
        aes(x = Year, y = Rank, group = Country)
      ) + 
      geom_line(
        aes( color = Country ), size = 2, alpha = 1
      )+
      #geom_point(color = "#FFFFFF", size = 4) +
      geom_point(aes(size = Indicator), alpha = 1, size = 4) +
      geom_point(color = "#FFFFFF", size = 1) +
      scale_y_reverse(
        breaks = seq(min(dt_expanded$Rank), max(dt_expanded$Rank)+1, by = 5)
      ) +
      scale_x_continuous(breaks = seq(from = start_year-1, to = end_year + 1, by =1)) +
      labs(x = "Year",
           y = "Rank",
           title = paste("Rank of highest to lowest by", ineq_indicator),
           subtitle = "Country rankings over time") +
      geom_point(color = "#FFFFFF", size = 1) +
      geom_text(data = dt_expanded[Year == min(Year)],
                aes(label = Country, x = Year-0.1), hjust = .85, fontface = "bold", color = "#888888", size = 4) +
      geom_text(data =dt_expanded[Year == max(Year)],
                aes(label = Country, x = Year+0.1) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +

      my_theme()+
      geom_vline(xintercept = c(start_year, end_year), linetype = "solid", color = "darkgrey")

    
  }
  
  # Step 5: Bump Chart Rankings ----
  ggplotly(g, tooltip = c("Country", "Year", "Indicator", "Region", "Rank"))
  
}


