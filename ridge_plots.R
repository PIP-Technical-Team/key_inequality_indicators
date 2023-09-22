dt_use <- copy(dt_pip)

# ridge plot - means
pacman::p_load(
  ggridges, 
  viridis, 
  hrbrthemes
)

ggplot(data = dt_use |> filter(year >2000 & year < 2015), aes(x = gini, y = year, group = year)) +
  geom_density_ridges_gradient( scale = 3, rel_min_height = 0.01) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE in 2016') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )