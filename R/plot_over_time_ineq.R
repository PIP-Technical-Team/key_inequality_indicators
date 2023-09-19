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
    #my_theme() + 
    labs(
      title = paste(ineq_indicator, "over time"),
      x     = "Year", 
      y     = ineq_indicator, 
      color = "Country", 
      caption = "note xxx"
    )#+
    #scale_colour_identity(name = 'Colour', guide = 'legend',labels = Country)
    #scale_colour_manual(name = 'the colour', 
                        #values =c('black'='black','red'='red'), labels = c('c2','c1'))
  
  ggplotly(g, tooltip = c("Country", "Year", "Region", "Comparable_spell", "Indicator"))
  
  
  
}

plot_over_time_ineq(countries_selected = c("United States", "Colombia"), ineq_indicator = "Gini", dt = dt_pip)



dt_use <- copy(dt_pip)

dt_use |> head() |> select(country_name, region_code, gini, year, comparable_spell)

dt_use |> 
  filter(country_name %chin% c("South Africa", "Colombia", "United States", "Germany", "Nepal", "Peru", "Argentina")) |>
  ggplot(aes(x = year, y = gini, color = country_name)) +
  geom_point(
    aes(x = year, y = gini, color = country_name, shape = region_code)
  ) + 
  geom_line(aes(group = comparable_spell)) +
  labs(
    title = "Gini over time", 
    subtitle = "Selected countries", 
    x = "Year", 
    y = "Gini", 
    color = "Country", 
    shape = "Region"
  )+
  theme_bw()
  




