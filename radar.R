dt_use <- copy(dt_pip)
pacman::p_load(fmsb)



dt_use2 <- left_join(
create_data_imputed_by_countries(
  dt = dt_use, 
  countries_selected = pip_countries, 
  year_selected = 1998, 
  indicator = c("Mean")
) |> select(country_name, year, mean), 
create_data_imputed_by_countries(
  dt = dt_use, 
  countries_selected = pip_countries, 
  year_selected = 1998, 
  indicator = c("Median")
) |> select(country_name, year, median)) |> 
  left_join(
    create_data_imputed_by_countries(
      dt = dt_use, 
      countries_selected = pip_countries, 
      year_selected = 1998, 
      indicator = c("Gini")
    ) |> select(country_name, year, gini)) |> 
  left_join(
    create_data_imputed_by_countries(
      dt = dt_use, 
      countries_selected = pip_countries, 
      year_selected = 1998, 
      indicator = c("Mld")
    ) |> select(country_name, year, mld)) |> 
  left_join(
    create_data_imputed_by_countries(
      dt = dt_use, 
      countries_selected = pip_countries, 
      year_selected = 1998, 
      indicator = c("Polarization")
    ) |> select(country_name, year, polarization)) |> 
  left_join(
    create_data_imputed_by_countries(
      dt = dt_use, 
      countries_selected = pip_countries, 
      year_selected = 1998, 
      indicator = c("Top10_bottom40")
    ) |> select(country_name, year, top10_bottom40)) |> 
  left_join(
    create_data_imputed_by_countries(
      dt = dt_use, 
      countries_selected = pip_countries, 
      year_selected = 1998, 
      indicator = c("Top20_bottom20")
    ) |> select(country_name, year, top20_bottom20)) |> 
  left_join(
    create_data_imputed_by_countries(
      dt = dt_use, 
      countries_selected = pip_countries, 
      year_selected = 1998, 
      indicator = c("Headcount")
    ) |> select(country_name, year, headcount)) |> 
  left_join(
    create_data_imputed_by_countries(
      dt = dt_use, 
      countries_selected = pip_countries, 
      year_selected = 1998, 
      indicator = c("Poverty_gap")
    ) |> select(country_name, year, poverty_gap)) |> 
  left_join(
    create_data_imputed_by_countries(
      dt = dt_use, 
      countries_selected = pip_countries, 
      year_selected = 1998, 
      indicator = c("Poverty_severity")
    ) |> select(country_name, year, poverty_severity))


dt_use2 |> 
  head() |> 
  select(country_name, year, mean) |> 
  mutate(
    mean = mean/max(mean)
  )







# Custom the radarChart !
radarchart( dt_use
)

# Create data: note in High school for Jonathan:
data <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(20,10) , rep(0,10) , data)
data
radarchart(data |> select(-c(`data-viz`, music, math)))


dt_use |> 
  filter(country_name = "South Africa")