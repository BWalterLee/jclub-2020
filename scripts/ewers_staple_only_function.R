# New plotting function

# Updated 1/29/2021 to include all available years.

# Notes for Ben for metrics to include




#  Extra investigations (interaction terms)
# - Percent Undernourishment
# - Fertilizer addition
# - Value of Exports/Imports per capita
# - Gross production value
# - 
# - 
# - 

library(rlang)
library(tidyverse)

# The function
ewers_plot <- function(staple,pop,start,end){

  
start.lab = as.name(paste("x",as.character(start),sep = ""))
end.lab = as.name(paste("x",as.character(end),sep = ""))
#
  
  area.tall <- pivot_longer(data = staple %>% filter(Element == "Area harvested"),cols = 8:66, names_to = "Year", values_to = "area")
  area.tall$Year <- as.numeric(area.tall$Year)
  
  # Joining Area with population, calculating crop hectares per capita
  pc.area <- left_join(area.tall,pop) %>% 
    dplyr::mutate(ha_percap = area/Population)%>% 
    dplyr::select(Area,Item,Year,ha_percap)
  
  # Separating out values by year for comparison
  pc.area.comparison <- pc.area %>% 
    filter(Year == start | Year == end)
  
  pc.area.tall <- pivot_wider(pc.area.comparison, names_from = Year, values_from = ha_percap, names_prefix = "x") %>% 
    dplyr::filter(!is.na(!!start.lab) & !is.na(!!end.lab) &  !!start.lab != 0 & !!end.lab !=0) 
  
  # Calculation on log(percap cropland ratio 2018: 1999) according to Ewers 2009
  pc.area.dif <- pc.area.tall %>% 
    group_by(Area) %>% 
    dplyr::summarise(total.start = sum(!!start.lab), total.end = sum(!!end.lab)) %>%        
    dplyr::mutate(log.delta.area = log(total.end / total.start)) %>%  
    dplyr::mutate(dir = if_else(log.delta.area >= 0, "+", "-")) %>% 
    dplyr::select(-total.start,-total.end)
  
  # Yield New
  
  yield.tall <- pivot_longer(data = staple %>% filter(Element == "Yield"),cols = 8:66, names_to = "Year", values_to = "yield")
  yield.tall$Year <- as.numeric(yield.tall$Year)
  # Manually creating calorie list because no tables exist :(
  # Stolen from https://iopscience.iop.org/1748-9326/8/3/034015/media/erl472821suppdata.pdf 
  
  kcal = data.frame(c("Apples","Barley", "Bananas","Cassava","Coconuts", "Cottonseed", "Grapes", "Maize", "Millet",
                      "Oats", "Onions, dry", "Oil palm fruit", "Plantains", "Potatoes", "Rice, paddy", "Sorghum",
                      "Soybeans", "Sugar beet", "Sugar cane", "Sunflower seed", "Sweet potatoes", "Vegetables, fresh nes",
                      "Wheat", "Yams"),  
                    c(479479.33, 3360215.93,614244.63,1058035.85,1430000.04, 4100000.00, 591589.58, 3580802.60, 3463917.52,
                      3850000.19, 394971.60, 5400000.00, 846666.67, 702122.60, 2800000.00, 3430000.40,
                      3596499.11, 700000.00, 291428.56, 2982902.68, 939797.99, 220000.00,
                      3284000.00,1090000.00))
  colnames(kcal) = c("Item", "kcal.tonne")
  kcal <- kcal %>% 
    dplyr::mutate(kcal.hg = kcal.tonne/10000)
  # write.csv(kcal, "../data/kcal_staple.csv") # NEVER AGAIN
  
  # Conversion
  kcal.ha <- left_join(yield.tall,kcal %>% dplyr::select(Item,kcal.hg)) %>% 
    dplyr::mutate(kcal.ha.cap = yield*kcal.hg) %>% 
    dplyr::select(Area, Item, Year,kcal.ha.cap) %>% 
    dplyr::filter(!is.na(kcal.ha.cap) )
  
  # Pick Years
  kcal.ha.start.end <- kcal.ha %>% 
    filter(Year == start | Year == end) 
  
  pc.yield.tall <- pivot_wider(kcal.ha.start.end, names_from = Year, values_from = kcal.ha.cap, names_prefix = "x") %>% 
    dplyr::filter(!is.na(!!start.lab) & !is.na(!!end.lab) &  !!start.lab != 0 & !!end.lab !=0) 
  
  kcal.yield.dif <- pc.yield.tall %>% 
    group_by(Area) %>% 
    dplyr::summarise(total.start = sum(!!start.lab), total.end = sum(!!end.lab)) %>% 
    dplyr::mutate(log.delta.yield = log(total.end / total.start)) %>% 
    dplyr::mutate(dir.1 = if_else(log.delta.yield >= 0, "+", "-")) %>% 
    dplyr::select(Area,log.delta.yield,dir.1)
  
  #
  
  combined_data <- left_join(pc.area.dif,kcal.yield.dif) 
  
  plot <- ggplot(data = combined_data, mapping = aes(x = log.delta.yield, y = log.delta.area)) + theme_classic() +
    geom_point() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + geom_smooth(method = "lm") + xlim(-.6,1.5)+
    ylim(-3.5,1.5) + labs(x = "log(yield ratio)", y = "log(per capita cropland ratio)", title = paste(as.character(start),"to",as.character(end),"n = ", as.character(nrow(combined_data))))
  
  return(plot)
}

# Currently annoying header specification means you need to run this lower code to get the 
#   data to merge correctly.

# Load and Clean Crop Data
staple_data <- read.csv("../data/Production_Crops_E_All_Data_NOFLAG.csv", sep = ",", header = T) %>% 
  dplyr::filter(Area.Code < 1000) %>% 
  dplyr::filter(Item =="Apples"| Item == "Barley"| Item ==  "Bananas"| Item == "Cassava"| Item == "Coconuts"| Item ==  "Cottonseed"| 
                  Item ==  "Grapes"| Item ==  "Maize"| Item ==  "Millet"| Item == "Oats"| Item ==  "Onions| Item ==  dry"| Item ==  "Oil palm fruit"| 
                  Item ==  "Plantains"| Item ==  "Potatoes"| Item ==  "Rice, paddy"| Item ==  "Sorghum"| 
                  Item == "Soybeans"| Item ==  "Sugar beet"| Item ==  "Sugar cane"| Item ==  "Sunflower seed"| Item ==  "Sweet potatoes"| 
                  Item ==  "Vegetables, fresh nes"| Item == "Wheat"| Item ==  "Yams") %>% 
  dplyr::filter(Element != "Production") 
names(staple_data) = gsub(pattern = "Y", replacement = "", x = names(staple_data))
staple_data_clean <- staple_data

# Load and Clean Pop data
pop_data <- read.csv("../data/fao_pop.csv", sep = ",", header = T) %>% 
  dplyr::mutate(Population = Value*1000) %>% 
  dplyr::select(Area,Population,Year)
names(pop_data) <- as.character(names(pop_data))
pop_data_clean <- pop_data

View(staple_data_clean)
View(pop_data_clean)

# Running Function
ewers_plot(staple = staple_data_clean, pop = pop_data_clean, start = 1979, end = 1999)

# Function for Data exporting
# The function
ewers_data <- function(staple,pop,start,end){
  
  
  start.lab = as.name(paste("x",as.character(start),sep = ""))
  end.lab = as.name(paste("x",as.character(end),sep = ""))
  #
  
  area.tall <- pivot_longer(data = staple %>% filter(Element == "Area harvested"),cols = 8:66, names_to = "Year", values_to = "area")
  area.tall$Year <- as.numeric(area.tall$Year)
  
  # Joining Area with population, calculating crop hectares per capita
  pc.area <- left_join(area.tall,pop) %>% 
    dplyr::mutate(ha_percap = area/Population)%>% 
    dplyr::select(Area,Item,Year,ha_percap)
  
  # Separating out values by year for comparison
  pc.area.comparison <- pc.area %>% 
    filter(Year == start | Year == end)
  
  pc.area.tall <- pivot_wider(pc.area.comparison, names_from = Year, values_from = ha_percap, names_prefix = "x") %>% 
    dplyr::filter(!is.na(!!start.lab) & !is.na(!!end.lab) &  !!start.lab != 0 & !!end.lab !=0) 
  
  # Calculation on log(percap cropland ratio 2018: 1999) according to Ewers 2009
  pc.area.dif <- pc.area.tall %>% 
    group_by(Area) %>% 
    dplyr::summarise(total.start = sum(!!start.lab), total.end = sum(!!end.lab)) %>%        
    dplyr::mutate(log.delta.area = log(total.end / total.start)) %>%  
    dplyr::mutate(dir = if_else(log.delta.area >= 0, "+", "-")) %>% 
    dplyr::select(-total.start,-total.end)
  
  # Yield New
  
  yield.tall <- pivot_longer(data = staple %>% filter(Element == "Yield"),cols = 8:66, names_to = "Year", values_to = "yield")
  yield.tall$Year <- as.numeric(yield.tall$Year)
  # Manually creating calorie list because no tables exist :(
  # Stolen from https://iopscience.iop.org/1748-9326/8/3/034015/media/erl472821suppdata.pdf 
  
  kcal = data.frame(c("Apples","Barley", "Bananas","Cassava","Coconuts", "Cottonseed", "Grapes", "Maize", "Millet",
                      "Oats", "Onions, dry", "Oil palm fruit", "Plantains", "Potatoes", "Rice, paddy", "Sorghum",
                      "Soybeans", "Sugar beet", "Sugar cane", "Sunflower seed", "Sweet potatoes", "Vegetables, fresh nes",
                      "Wheat", "Yams"),  
                    c(479479.33, 3360215.93,614244.63,1058035.85,1430000.04, 4100000.00, 591589.58, 3580802.60, 3463917.52,
                      3850000.19, 394971.60, 5400000.00, 846666.67, 702122.60, 2800000.00, 3430000.40,
                      3596499.11, 700000.00, 291428.56, 2982902.68, 939797.99, 220000.00,
                      3284000.00,1090000.00))
  colnames(kcal) = c("Item", "kcal.tonne")
  kcal <- kcal %>% 
    dplyr::mutate(kcal.hg = kcal.tonne/10000)
  # write.csv(kcal, "../data/kcal_staple.csv") # NEVER AGAIN
  
  # Conversion
  kcal.ha <- left_join(yield.tall,kcal %>% dplyr::select(Item,kcal.hg)) %>% 
    dplyr::mutate(kcal.ha.cap = yield*kcal.hg) %>% 
    dplyr::select(Area, Item, Year,kcal.ha.cap) %>% 
    dplyr::filter(!is.na(kcal.ha.cap) )
  
  # Pick Years
  kcal.ha.start.end <- kcal.ha %>% 
    filter(Year == start | Year == end) 
  
  pc.yield.tall <- pivot_wider(kcal.ha.start.end, names_from = Year, values_from = kcal.ha.cap, names_prefix = "x") %>% 
    dplyr::filter(!is.na(!!start.lab) & !is.na(!!end.lab) &  !!start.lab != 0 & !!end.lab !=0) 
  
  kcal.yield.dif <- pc.yield.tall %>% 
    group_by(Area) %>% 
    dplyr::summarise(total.start = sum(!!start.lab), total.end = sum(!!end.lab)) %>% 
    dplyr::mutate(log.delta.yield = log(total.end / total.start)) %>% 
    dplyr::mutate(dir.1 = if_else(log.delta.yield >= 0, "+", "-")) %>% 
    dplyr::select(Area,log.delta.yield,dir.1)
  
  #
  
  combined_data <- left_join(pc.area.dif,kcal.yield.dif) 

  return(combined_data)
}


data_79_99 <- ewers_data(staple = staple_data_clean, pop = pop_data_clean, start = 1979, end = 1999)
View(data_79_99)
lm_79_99 <- lm(log.delta.area ~ log.delta.yield, data = data_79_99)
summary(lm_79_99)


data_00_18 <- ewers_data(staple = staple_data_clean, pop = pop_data_clean, start = 2000, end = 2018)
lm_00_18 <- lm(log.delta.area ~ log.delta.yield, data = data_00_18)
summary(lm_00_18)
