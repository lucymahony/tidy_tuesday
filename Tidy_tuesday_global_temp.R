# Tidy tuesday global temperature 
# Install packages
library(tidyverse)
library(tidytuesdayR)
library(dplyr)
library(plotrix)
library(plyr)
library(plotly)

# Load data 
# Data from Tidy Tuesdaay 
data <- tidytuesdayR::tt_load(2023, week = 28)
global_temps <- data$global_temps
colnames(global_temps) 
# Reformat to yearly temperature differences 
global_temps <- global_temps %>%
  select("Year", "J-D") # The full year is Jan to Dec (J-D). So only this column is required when looking at trends per year 
global_temps <- global_temps %>% rename(Temperature =`J-D` ) # Rename it as Temperature

# Temperature data from two sources one dates from 1885 to 2018 and the other
# from 1979 to 2023
# Temperature data from https://ourworldindata.org/atmospheric-concentrations
#Hannah Ritchie, Max Roser and Pablo Rosado (2020) 
owid_co2_79_to_23 <- read_csv("/Users/mahony/Downloads/climate-change.csv") 
# Temperature data fromhttps://www.eea.europa.eu/data-and-maps/daviz/atmospheric-concentration-of-carbon-dioxide-5#tab-chart_5_filters=%7B%22rowFilters%22%3A%7B%7D%3B%22columnFilters%22%3A%7B%22pre_config_polutant%22%3A%5B%22CH4%20(ppb)%22%5D%7D%7D
eea_data_to_2018 <- read_csv("/Users/mahony/Downloads/atmospheric-concentration-of-carbon-dioxide-5.csv")
# Sort and combine temperature datasets
eea_data_to_2018 <- eea_data_to_2018 %>%
  filter(`Polutant:text` =='CO2 (ppm)', 
         `Year:year` > 1880)
eea_data_to_2018 <- eea_data_to_2018 %>% select("Year:year", "Value:number")
eea_data_to_2018 <- eea_data_to_2018 %>% rename(Year = `Year:year`, Average_CO2 = `Value:number`)

owid_co2_79_to_23 <- owid_co2_79_to_23 %>%
  select("Entity", "Date", "Monthly averaged...19")
owid_co2_79_to_23 <- owid_co2_79_to_23 %>% filter(Entity=="World") # Keep only world data
owid_co2_79_to_23 <- na.omit(owid_co2_79_to_23)
owid_co2_79_to_23 <- owid_co2_79_to_23 %>%
  mutate(Year = as.numeric(format(Date, "%Y"))) %>%
  group_by(Year) %>%
  summarize(Average_CO2 = mean(`Monthly averaged...19`, na.rm = TRUE))
owid_co2_19_to_23 <- owid_co2_79_to_23 %>% filter(Year> 2019)
combined_data <- rbind(eea_data_to_2018, owid_co2_19_to_23) # Combine CO2 datasets 

# Create a plot for Temperature
plot_temp <- ggplot(global_temps, aes(x = Year, y = Temperature)) +
  geom_point(aes(color = Temperature)) +
  theme_minimal() +
  labs(
    title = "Temperature and CO2 over time",
    x = "",
    y = "Temperature"
  ) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(limits = c(-0.5, 1))

# Create a plot for CO2
plot_co2 <- ggplot(combined_data, aes(x = Year, y = Average_CO2)) +
  geom_point(aes(color = Average_CO2)) +
  theme_minimal() +
  labs(
    x = "",
    y = "CO2 ppm"
  ) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(limits = c(275, 425))

# Create bar chart for temperature
plot_temp <- ggplot(global_temps, aes(x = Year, y = Temperature, fill = Temperature)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "Temperature over time",
    x = "",
    y = "Temperature"
  ) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, guide = guide_colorbar(title = "Temperature")) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")


# Create a plot that overlays temperature difference and global CO2 
final_dataset <- merge(combined_data[, c("Year", "Average_CO2")], global_temps[, c("Year", "Temperature")], by = "Year", all=TRUE)

final_plot <- plot_ly(data=final_dataset,
             x=~Year,
             y=~Temperature,
             type="bar",
             name="Temperature", 
             marker = list(color =~Temperature,
                           colorscale = 'RdBu'), 
             opacity =0.8
) %>% add_trace(
  type="scatter",
  mode="markers",
  y=~Average_CO2,
  name="CO2",
  yaxis="y2",
  marker = list(symbol = "circle", size = 8, opacity = 0.8, color = 'black'),
  opacity = 0.6
) %>%
  layout(yaxis=list(side="left",title="Temperature Difference (°C)", showgrid=FALSE),
         yaxis2=list(side="right",title="CO2 concentration (ppm)",overlaying='y',
                     showgrid=FALSE), 
         xaxis =list(),
         legend = list(x = 0, y = 1, xanchor = "left", yanchor = "top"),
         margin = list(l = 50, r = 50, b = 50, t = 50))




