dt_use <- create_data_ranked_by_countries(
  dt = dt_pip,
  year_selected = c(2000), 
  countries_selected = pip_countries
)
dt_use2 <- create_data_ranked_by_countries(
  dt = dt_pip,
  year_selected = c(2005), 
  countries_selected = pip_countries
)
dt_use <- dt_use |> filter(country_name %chin% intersect(dt_use$country_name, dt_use2$country_name))
dt_use2 <- dt_use2 |> filter(country_name %chin% intersect(dt_use$country_name, dt_use2$country_name))


dt_use <- create_data_ranked_by_countries(
  dt = dt_use |> select(-Rank),
  year_selected = c(2000), 
  countries_selected = intersect(dt_use$country_name, dt_use2$country_name)
)
dt_use2 <- create_data_ranked_by_countries(
  dt = dt_use2 |> select(-Rank),
  year_selected = c(2005), 
  countries_selected = intersect(dt_use$country_name, dt_use2$country_name)
)

rbindlist(
  list(dt_use |> 
  select(country_name, year, comparable_spell, gini, Rank), 
dt_use2 |> select(country_name, year, comparable_spell, gini, Rank))) |> 
  mutate(
    Selected = ifelse(country_name %chin% c("South Africa", "Peru", "United States"), "Yes", "No" )
  ) |>
  mutate(
    SelectedNames = ifelse(country_name %chin% c("South Africa", "Peru", "United States"), country_name, "" )
  ) |>
  ggplot(aes(x = year, y = Rank, group = country_name, colour = Selected, alpha = Selected))+
  geom_line() +
  geom_point() + 
  scale_y_reverse(
    breaks = seq(min(dt_use$Rank), max(dt_use$Rank)+1, by = 5)
  ) +
  geom_text(aes(label = SelectedNames),nudge_y = -2) +
  scale_colour_manual(values = c("Yes" = "red", "No" = "lightgrey"))+ 
  scale_alpha_manual(values = c("Yes" = 1, "No" = 0.5))+ 
  theme_bw() +
  labs(
    x = "Year", 
    y = "Rank", 
    title = "Changing Country Ranks over Time", 
    subtitle = "Ranked by Lowest to Highest by Gini"
  )

