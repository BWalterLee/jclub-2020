# Urbanization Data Import and Organization 

#  All data sourced from World Bank
#     https://data.worldbank.org/topic/urban-development 

library(tidyverse)

# This Script covers all 
# Data Cleaning and Organization after import of CSVs from worldbank.org 

# Metrics to Analyze ####
#
#  Total Population
#  Total Population Density
#  Total Urban Population
#  Total Urban Population (Percent)
#  Annual Urban Population Growth Rate (Percent)
#  Total Urban Area (square km)
#  Percentage of population n cities over 1 million
#  
#        Several more available from WB


# Cleaning Function ####
#  Input is "data = ..." but header must =F for the file,
#   and format = "long" or "wide" 
#   "long" 

  wb_dataclean <- function(data, format){
  
  # Remove Excess Columns
  chop_top <- data.frame(data[-c(1,2),]) 
  
  # Recreate Header
  colnames(chop_top) <- c(chop_top[1,])
  
  # Remove old header row
  wide_data <- chop_top[-1,]
  
  # Pivot to long format
  long_data = pivot_longer(data = wide_data, cols = !c(1:4), names_to = "Year", values_to = "count")
  
  # Return data based on format choice
  ifelse(format == "wide", return(wide_data), return(long_data))
  
}

# Data Import and Cleaning ####
tot_population <- wb_dataclean(data = read.csv("../data/wb_pop_total.csv", header = F), format = "long")
tot_pop_density <- wb_dataclean(data = read.csv("../data/wb_pop_density.csv", header = F),format = "long")
tot_urb_pop <- wb_dataclean(data = read.csv("../data/wb_urb_total.csv", header = F),format = "long")
tot_urb_perc <-wb_dataclean(data = read.csv("../data/wb_urb_perc.csv", header = F),format = "long")
urb_growth_perc <- wb_dataclean(data = read.csv("../data/wb_urb_growth_perc.csv", header = F),format = "long")
tot_urb_area <- wb_dataclean(data = read.csv("../data/wb_urb_area_sqkm.csv", header = F),format = "long")
perc_pop_cities1m <- wb_dataclean(data = read.csv("../data/wb_perc_pop_cityover1m.csv", header = F),format = "long")


# Grouping of separate data 
full_tall <- rbind(tot_population,
        tot_pop_density,
        tot_urb_pop,
      tot_urb_perc,
      urb_growth_perc,
        tot_urb_area,
      perc_pop_cities1m)

Urbanization_data_final <- full_tall %>% 
  dplyr::select(-`Indicator Code`) %>% 
  pivot_wider(values_from = count, names_from = `Indicator Name`)


# Final file written
write.csv(Urbanization_data_final, "../data/Urbanization_data_final.csv")





