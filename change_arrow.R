dt_use <- copy(dt_pip)
dt_use2 <- copy(dt_pip)

dt_use <- create_data_ranked_by_countries(
  dt = dt_use, 
  countries_selected = pip_countries, 
  year_selected = 2000, 
  indicator = "Gini"
)

dt_use3 <- joyn::merge(
  dt_use, 
  dt_use2 |> rename("Gini2" = "gini") |> filter(year == 2005), 
  match_type = "1:1", 
  keep = "left", 
  yvar = "Gini2", 
  by = c("country_name", "welfare_type", "reporting_level")
) |> 
  mutate(
    GiniChange = round((gini - Gini2)/gini, 2)
  ) |>
  filter(!is.na(GiniChange)) |> 
  mutate(
    Change = ifelse(GiniChange<0, "Negative", "Positive")
  ) 
dt_use3[, country_name := factor(country_name, levels = country_name[order(Rank)])]

dt_use3 |> select(country_name, gini, GiniChange, Change) |> head()

dt_use3 |>
  ggplot() +
  geom_segment( aes(x=gini, xend=Gini2, y=country_name, yend=country_name, color = Change),arrow = arrow(length = unit(1.2, "mm"),
                                                                                                        type = "closed")) + 
  
  #geom_point(aes(x = GiniChange, y = country_name, color = Change))+
  scale_color_manual(values = c("Negative" = "red", "Positive" = "darkgreen")) + 
  geom_point(aes(x = gini, y = country_name, colour = Change), size = 1) +
  my_theme()+
  labs(
    title = "Change in Gini from 2000 to 2005",
    subtitle = "Countries ordered from highest to lowest 2000 Gini", 
    x = "Change in Gini", 
    y = "Country"
  )


