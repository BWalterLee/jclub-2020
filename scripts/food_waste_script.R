# Waste Data Test Examination

library(tidyverse)

fwaste_code <- read.csv("data/country_level_codebook.csv")
view(fwaste_code)

fwaste_data <- read.csv("data/country_level_data_0.csv") %>% 
  select(region_id, country_name,gdp, composition_food_organic_waste_percent) %>% 
  rename(fw_perc = composition_food_organic_waste_percent)


View(fwaste_data)

mean(fwaste_data$fw_perc, na.rm=T)
  
