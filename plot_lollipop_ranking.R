plot_lollipop_ranking <- function(
    
){
  
  
}



dt_use <- copy(dt_pip)
dt_use[, if(.N>1) .SD, by = c("country_name", "year")]

dt_use <- create_data_ranked_by_countries(
  dt                 = dt_use, 
  countries_selected = pip_countries,
  window_length      = 2, 
  year               = 2000
)
setorder(dt_use, Rank)
dt_use[c(1:10), .(country_name, year, gini, Rank)]

dt_use[, country_name := factor(country_name, levels = country_name[order(Rank)])]

dt_use |> 
  rename("Region" = "region_code") |> 
  #filter(country_name %chin% sample(x = pip_countries, size = 12) ) |>
  ggplot( aes( x = country_name, y = gini) ) + 
  geom_point(aes(color = Region)) +
  geom_segment( aes( xend = country_name, y = 0, yend = gini) ) + 
  coord_flip()+
  theme_bw()+
  labs(
    title = paste("Ranking of Gini from highest to lowest"), 
    subtitle = "In Year 2000", 
    x = "Country", 
    y = "Gini",
    caption = "Note: \n If is no data available in year 2000 use the nearest value within a 2 year window, else exclude"
  )

