plot_over_time_ineq <- function(
    dt, 
    countries_selected = c("Colombia", "South Africa", "United States"),
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
    )
){
  ineq_indicator <- match.arg(ineq_indicator)
  dt_use <- copy(dt)
  
  setnames(
    dt_use, 
    old = c("country_name", "year", "region_code", "comparable_spell", tolower(ineq_indicator)), 
    new = c("Country", "Year", "Region", "Comparable_spell", "Indicator")
  )
  
  dt_use <- dt_use[, Indicator := round(Indicator, 2)]
  
  g <- dt_use[Country %chin% countries_selected] |>
    ggplot(aes(x = Year, y = Indicator, color = Country))+
    geom_point( aes(shape = Region), size = 3 ) +
    geom_line(aes(group = Comparable_spell))+
    my_theme() + 
    labs(
      title = paste(ineq_indicator, "over time"),
      x     = "Year", 
      y     = ineq_indicator, 
      color = "Country"
    )
  
  ggplotly(g, tooltip = c("Country", "Year", "Region", "Comparable_spell", "Indicator"))
  
  
  
}



