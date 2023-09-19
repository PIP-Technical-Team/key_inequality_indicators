# Library
pacman::p_load(fmsb)

# Create data: note in High school for Jonathan:
data <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbidt_use <- copy(dt_pip)
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
    GiniChange = gini - Gini2
  ) |>
  filter(!is.na(GiniChange)) |> 
  mutate(
    Change = ifelse(GiniChange<0, "Negative", "Positive")
  ) 
dt_use3[, country_name := factor(country_name, levels = country_name[order(Rank)])]

dt_use3 |>
  ggplot() +
  geom_segment( aes(x=0, xend=GiniChange, y=country_name, yend=country_name, color = Change)) + 
  geom_point(aes(x = GiniChange, y = country_name, color = Change))+
  scale_color_manual(values = c("Negative" = "red", "Positive" = "darkgreen")) + 
  my_theme()+
  labs(
    title = "Change in Gini from 2000 to 2005",
    subtitle = "Countries ordered from highest to lowest 2000 Gini", 
    x = "Change in Gini", 
    y = "Country"
  )
  

nd(rep(20,10) , rep(0,10) , data)

# Check your data, it has to look like this!
# head(data)

# Custom the radarChart !
radarchart( data  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)