library(plotly)
library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(tidyverse)

#Data Objects
merged_data <- unicef_metadata %>%
  inner_join(unicef_indicator_1, by = c("country", "year"))
merged_data_1 <- unicef_metadata %>%
  inner_join(unicef_indicator_1, by = c("country", "year"))
unicef_metadata <- read_csv("unicef_metadata.csv")
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
data_join <- full_join(unicef_metadata, unicef_indicator_1, by = c("country"))
merged_data <- unicef_metadata %>%
  inner_join(unicef_indicator_1, by = c("country", "year"))
world_data <- map_data("world")
merged_data_map <- merge(world_data, merged_data, by.x = "region", by.y = "country")
world_data <- left_join(world_data, unicef_indicator_1, by = c("region" = "country"))
timeseries_plot_1 <- data_join
if (!all(c('year', 'child') %in% names(merged_data))) {
  stop("The required columns are not all present in the dataframe.")
}
merged_data$year <- as.numeric(as.character(merged_data$year))
merged_data <- merged_data[!is.na(merged_data$child), ]
merged_data <- merged_data[!duplicated(merged_data$year), ]
merged_data <- merged_data[order(merged_data$year), ]
if (!all(c('year', 'child') %in% names(merged_data))) {
  stop("The required columns are not all present in the dataframe.")
}
merged_data$year <- as.numeric(as.character(merged_data$year))
merged_data <- merged_data[!is.na(merged_data$child), ]
merged_data <- merged_data[!duplicated(merged_data$year), ]
merged_data <- merged_data[order(merged_data$year), ]
max_child <- max(merged_data$child, na.rm = TRUE)
max_gdp <- max(merged_data$gdp, na.rm = TRUE)
merged_data <- merged_data %>%
  mutate(child_scaled = child / max_child * 100,
         gdp_scaled = gdp / max_gdp * 100)
high_child_deprivation <- unicef_indicator_1 %>% 
  filter(child > 20)
data_2018 <- unicef_metadata %>%
  filter(year == 2018)

custom_theme <- theme(
  plot.background = element_rect(fill = "black", color = NA),
  panel.background = element_rect(fill = "black", color = NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  text = element_text(color = "white"),
  axis.title = element_text(color = "white"),
  axis.text = element_text(color = "white"),
  legend.background = element_rect(fill = "black", color = NA),
  legend.text = element_text(color = "white"),
  legend.title = element_text(color = "white")
)


#Visualisations

#MAPS

#MAP1
ggplot(data = world_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = child), color = "white") + 
  scale_fill_gradient(low = "skyblue", high = "darkblue", na.value = "gray50", 
                      limits = c(NA, max(world_data$child, na.rm = TRUE))) + custom_theme
labs(title = "Child Deprivation Index by Country", fill = "Child Deprivation %") + 
  theme_void() + legend.position = "none" +
  coord_fixed(1.3) +
ggsave("child_deprivation_map.png")



#Time Series

#TIMESERIES1
merged_data <- merged_data %>%
  mutate(child_scaled = child / max_child * 100,
         gdp_scaled = gdp / max_gdp * 100)

ggplot(merged_data, aes(x = year)) +
  geom_line(aes(y = child_scaled, color = "Child Deprivation"), size = 1) +
  geom_line(aes(y = gdp_scaled, color = "GDP"), size = 1) +
  theme_minimal() +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "bottom") +
  labs(x = "Year", y = "", title = "Child Deprivation and GDP Trends Over Time", color = "Indicator") +
  guides(color = guide_legend(override.aes = list(size = 2))) +   theme_minimal()   +
  custom_theme

ggsave("child_gdp_trends_over_time_normalized.png", width = 10, height = 6)

#Bar Chart

#BARCHART1
ggplot(high_child_deprivation, aes(x = reorder(country, child), y = child, fill = country)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  scale_fill_viridis_d() +
  theme_minimal() + custom_theme +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5)) + # Adjust text angle and justification for Y axis
  labs(y = "Country", x = "Child Deprivation Severity", title = "Child Deprivation Severity; Worst Countries") +
  guides(fill = guide_legend(title = "Country"))

ggplot(high_child_deprivation, aes(x = reorder(country, child), y = child)) +
  geom_bar(stat = "identity", aes(fill = child)) +  # Use 'child' for fill color scale
  coord_flip() +
  scale_fill_gradient(low = "lightcoral", high = "darkred") +  
  custom_theme +  # Apply custom theme that includes black background and white text
  theme(axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5),
        legend.position = "none") +  # Further customize theme based on needs
  labs(y = "Country", x = "Child Deprivation Severity", title = "Child Deprivation Severity; Worst Countries")

s
ggsave("high_child_deprivation_bar_chart_flipped.png", width = 10, height = 8)

#ScatterPlot1 
     ggplot(merged_data_1, aes(x = gdp, y = child)) +
      geom_point(aes(color = country)) + # Add points for each observation, color-coded by country
      geom_smooth(method = "lm", color = "white", se = FALSE) + 
      theme_minimal() + 
      custom_theme +
      annotate("text", x = Inf, y = Inf, label = "This graph shows as GDP increases, 
               child deprivation decreases.", hjust = 1.1, vjust = 2, color = "white", size = 5) +
      labs(x = "GDP", y = "Child Deprivation Per Country", title = "Relationship between GDP and Child Deprivation Per Country") +
      theme(legend.position = "none")  
     
   
     library(plotly)
    
     filtered_data <- merged_data[merged_data$sex != "total", ]
     data_2018_test <- dplyr::filter(data_join, year == 2018)
  #ScatterPlot2
     p <- data_join %>%
      
       ggplot( aes(gdp, lifexp, size = pop, color=country)) +
       geom_point() +
       custom_theme
     
    
     
     p <- data_2018 %>%
       
       ggplot( aes(gdp, military, size = pop, color=country)) +
       geom_point() + 
       labs(x = "GDP", y = "Military Expenditure") +
       custom_theme
     ggplotly(p)
     
     
     
     
     
     
     
     
     
     
    
