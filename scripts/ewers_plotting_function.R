
library(rlang)
library(tidyverse)
ewers_data <- function(staple,pop,start,end){
  
staple_data <- staple %>% 
    filter(Year == start | Year == end)
pop_data <- pop %>%
    filter(Year == start | Year == end) %>% 
    dplyr::mutate(Population = Value*1000) %>% 
    dplyr::select(Area,Population,Year)
  
  
pc.area <- left_join(staple_data %>% filter(Element == "Area harvested"),pop_data) %>% 
  dplyr::mutate(ha_percap = Value/Population)%>% 
  dplyr::select(Area,Item,Year,ha_percap) 

pc.area
# Separating out values by year for comparison


start.lab = as.name(paste("x",as.character(start),sep = ""))
end.lab = as.name(paste("x",as.character(end),sep = ""))

pc.area.tall <- pivot_wider(pc.area, names_from = Year, values_from = ha_percap, names_prefix = "x") %>% 
  dplyr::filter(!is.na(!!start.lab) & !is.na(!!end.lab) &  !!start.lab != 0 & !!end.lab !=0) 
pc.area.tall
# Calculation on log(percap cropland ratio end: start) according to Ewers 2009
pc.area.dif <- pc.area.tall %>% 
  group_by(Area) %>% 
  dplyr::summarise(total.start = sum(!!start.lab), total.end = sum(!!end.lab)) %>%          # Fix specification of column names here dude wow
  dplyr::mutate(log.delta.area = log(total.end / total.start)) %>%  
  dplyr::mutate(dir = if_else(log.delta.area >= 0, "+", "-")) %>% 
  dplyr::select(-total.start,-total.end)

yield <- staple_data %>% filter(Element == "Yield") %>% 
  dplyr::select(Area,Item,Year,Unit,Value) 

yield.tall <- pivot_wider(yield, names_from = Year, values_from = Value, names_prefix = "x") %>% 
  dplyr::filter(!is.na(!!start.lab) & !is.na(!!end.lab) &  !!start.lab != 0 & !!end.lab !=0) 

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
kcal.ha <- left_join(yield.tall,kcal %>% 
  dplyr::select(Item,kcal.hg)) %>% 
  dplyr::mutate(kcal.ha.start = !!start.lab*kcal.hg, kcal.ha.end = !!end.lab*kcal.hg) %>% 
  dplyr::select(Area, Item, kcal.ha.start, kcal.ha.end) %>% 
  dplyr::filter(!is.na(kcal.ha.start) & !is.na(kcal.ha.end))

kcal.yield.dif <- kcal.ha %>% 
  group_by(Area) %>% 
  dplyr::summarise(total.start = sum(kcal.ha.start), total.end = sum(kcal.ha.end)) %>% 
  dplyr::mutate(log.delta.yield = log(total.end / total.start)) %>% 
  dplyr::mutate(dir.1 = if_else(log.delta.yield >= 0, "+", "-")) %>% 
  dplyr::select(Area,log.delta.yield,dir.1)

combined_data <- left_join(pc.area.dif,kcal.yield.dif) 

plot <- ggplot(data = combined_data, mapping = aes(x = log.delta.yield, y = log.delta.area)) + theme_classic() +
  geom_point() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + geom_smooth(method = "lm") + xlim(-.6,1.5)+
  ylim(-3.5,1.5) + labs(x = "log(yield ratio)", y = "log(per capita cropland ratio)", title = paste(as.character(start),"to",as.character(end)))

  return(plot)
}

staple_data <- staple_data <- read.csv("../data/fao_staples.csv", sep = ",", header = T) 
pop_data <- read.csv("../data/fao_pop.csv", sep = ",", header = T)


ewers_data(staple = staple_data, pop = pop_data, start = 1990, end = 2008)  



