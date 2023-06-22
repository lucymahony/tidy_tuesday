library(tidyverse)
library(ggmap)
library(maps)
library(dplyr)
library(ggrepel)
library(gridExtra)

ufo_sightings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')
places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/places.csv')
worldmap = map_data('world')


sightings_by_city <- ufo_sightings %>%
  filter(reported_date_time >= as.Date("2000-01-01"), country_code == "GB") %>%
  mutate(city = str_to_lower(city), #remove inconsistent capitalization
         city = stringi::stri_trans_general(city, "Latin-ASCII")) %>% #get rid of accents recorded on some sightings but not others
  count(city, state, sort = TRUE) %>%
  left_join(places %>% select(city, state, latitude, longitude, population) 
            %>% mutate(city = str_to_lower(city), city = stringi::stri_trans_general(city, "Latin-ASCII")) %>% distinct_all(),
            by = c("city", "state")) %>%
  rename("region" = "state") %>%
  mutate(sightings_per_100000 = (n/population)*100000)
sightings_by_city <- sightings_by_city %>% filter(population > 0)


custom_theme<-theme(
  legend.position="bottom",
  legend.title = element_text(color = "white"),
  legend.text = element_text(colour="white"),
  legend.direction = "horizontal",    
  legend.box = "horizontal",
  plot.background = element_rect(fill = "black", color = "black"),
  plot.title = element_text(color = "white", size = 15, face = "bold", hjust = 0.5))

location_plot <- sightings_by_city %>% 
  ggplot() +
  geom_polygon(data = worldmap, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = 'gray35', 
               color = "white",
               linewidth = 0.2) + 
  coord_fixed(xlim = c(-10, 3), 
              ylim = c(50.3, 59)) +
  ggtitle(" Reported UFO sightings since 2000")+
  labs(color="Number\n of sightings ") +
  geom_point(aes(x = longitude, y = latitude, color = n), alpha=0.8) +
  scale_color_gradient(low = "#cba9de", high = "#3D064E",trans="log10" ) +
  theme_void() + 
  custom_theme


# Sightings per person for the 10 biggest cities 
big_cities <- sightings_by_city %>%
  arrange(desc(population)) %>%  # Arrange in ascending order
  slice_head(n = 10)

# Plot their size 
city_plot <- big_cities %>% 
  ggplot() +
  geom_polygon(data = worldmap, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = 'gray35', 
               color = "white",
               linewidth = 0.2) + 
  coord_fixed(xlim = c(-10, 3), 
              ylim = c(50.3, 59)) +
  ggtitle("UFO sightings per person")+
  labs(color="Number\n of sightings\n per 100000 people ") +
  geom_point(aes(x = longitude, y = latitude, color = sightings_per_100000, size= sightings_per_100000), alpha=0.8) +
  geom_text_repel(aes(x = longitude, y = latitude, label = toupper(city)),colour="white") +
  scale_color_gradient(low = "#FFF7EB", high = "#F59300") +
  theme_void() + 
  custom_theme + guides(size="none")



width <- 10
height <- 8
resolution <- 300
ggsave("~/Downloads/ufo_plot.png", arrangeGrob(location_plot,city_plot,ncol = 2), width = width, height = height, dpi = resolution)

