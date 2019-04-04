

library(tidyverse)
library(sf)
library(plotbiomes) #https://github.com/valentinitnelav/plotbiomes

# Read the WG data 
lter_data <- read_csv("Data/mean_site_all21.csv") %>%
  select(SiteUID,Temp,Precip)

# convert units to match the one used in the package
lter_data_fixed <- lter_data %>% 
  mutate(Temp=Temp/10) %>%
  mutate(Precip=Precip/10) # the sapce is in cm

# create spatial objects
lter_point <- st_as_sf(lter_data_fixed, coords = c("Temp", "Precip"))
sp_biomes <- st_as_sf(Whittaker_biomes_poly)

# join the data using spatial join
out <- st_intersection(lter_point, sp_biomes) 

# plot the data
whittaker_base_plot() +
  # add the temperature - precipitation data points
  geom_point(data = lter_data_fixed, 
             aes(x = Temp, 
                 y = Precip), 
             size   = 3,
             shape  = 21,
             colour = "gray95", 
             fill   = "black",
             stroke = 1,
             alpha  = 0.5) +
  theme_bw()

# join back the data
lter_biomes <- left_join(lter_data, out, by="SiteUID") %>% select(-geometry)

# write csv
write_csv(lter_biomes, "Data/mean_site_all21_biomes.csv")

