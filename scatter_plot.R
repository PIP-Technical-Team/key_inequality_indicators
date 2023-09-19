# Create data
dt_use <- copy(dt_pip)


# Scatterplot 1
dt_use |> 
  select(
    country_name, region_code, year, mean, gini, welfare_type, reporting_level
  ) |> 
  rename("Region" = "region_code") |> 
  ggplot(
    aes(
      x = gini, y = mean
    )
  ) +
  geom_point(aes(color = Region)) + 
  labs(
    title = "Scatterplot of relationship between Mean and Gini", 
    x = "Gini", 
    y = "Mean"
  ) + 
  theme_classic()


# Scatterplot 2
dt_use2 <- dt_use |> 
  filter(country_name %chin% c(
    "South Africa", 
    "Colombia", 
    "United States", 
    "Benin", 
    "Germany", 
    "Nepal", 
    "Argentina", 
    "Namibia", 
    "Italy"
  ))

ggplot() +
  geom_point(aes(x = dt_use$gini, y = dt_use$mean), colour = "lightgrey")+
  geom_point(aes(x = dt_use2$gini, y = dt_use2$mean, colour = dt_use2$region_code)) + 
  labs(
    title = "Scatterplot of relationship between Mean and Gini", 
    subtitle = "For selected countries",
    x = "Gini", 
    y = "Mean", 
    colour = "Region"
  ) + 
  theme_classic()




# Scatterplot 3 - high, medium, low
dt_use |> head()
dt_use |> 
  mutate(
    Group = case_when(
      gini < quantile(dt_use$gini, probs = c(0.33, 0.67, 1), na.rm = T)[[1]] ~ "Low", 
      gini > quantile(dt_use$gini, probs = c(0.33, 0.67, 1), na.rm = T)[[2]] ~ "High", 
      gini < quantile(dt_use$gini, probs = c(0.33, 0.67, 1), na.rm = T)[[2]] ~ "Medium"
    )
  ) |>
  mutate(
    lmean = log(mean)
  ) |>
  filter(!is.na(Group)) |>
  select(country_name, gini, Group, year, lmean) |>
  ggplot() +
  geom_point(aes(x = gini, y = lmean, color = Group)) +
  scale_color_manual(values = c("Low" = "darkgreen", "Medium" = "orange", "High" = "red")) + 
  labs(
    title = "Scatterplot of relationship between Mean and Gini", 
    subtitle = "For selected countries",
    x = "Gini", 
    y = "Mean", 
    colour = "Inequality Level"
  ) + 
  theme_classic()



# Scatterplot 4 - high, medium, low by region
dt_use |> head()
dt_use |> 
  mutate(
    Group = case_when(
      gini < quantile(dt_use$gini, probs = c(0.33, 0.67, 1), na.rm = T)[[1]] ~ "Low", 
      gini > quantile(dt_use$gini, probs = c(0.33, 0.67, 1), na.rm = T)[[2]] ~ "High", 
      gini < quantile(dt_use$gini, probs = c(0.33, 0.67, 1), na.rm = T)[[2]] ~ "Medium"
    )
  ) |>
  mutate(
    lmean = log(mean)
  ) |>
  filter(!is.na(Group)) |>
  select(country_name, gini, Group, year, lmean, mean, region_code) |>
  ggplot() +
  #geom_boxplot(aes(x = region_code, y = lmean), alpha = 10) +
  geom_jitter(aes(x = region_code, y = lmean, color = Group, shape = region_code)) +
  scale_color_manual(values = c("Low" = "darkgreen", "Medium" = "orange", "High" = "red")) + 
  labs(
    title = "Welfare and Inequality by Region", 
    subtitle = "For all countries and years",
    x = "Region", 
    y = "Log of Mean Welfare", 
    colour = "Inequality Level"
  ) + 
  theme_classic()





