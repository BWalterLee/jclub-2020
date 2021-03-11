# Last Update 2/11/2021

# 
# Data must be "fao_composite_tall.csv" to run
# X , Y, or Z can currently be:
#      -"tonnes_nitrogen"      (ag use only)
#      -"import_value_usd"     (ag products only)
#      -"export_value_usd"     (ag products only)
#      -"total_ag_value_pin"   (PIN is FAO "Production Index Number")
#      -"kcal.ha.tot"          (still from only staple crops)
#      -"area.tot"             (still from only staple crops)
#      -"tonnes_pest_total"   ( pesticide use for agriculture per capita)
#
#    All values are calculated per capita following Ewers 2009
#    New values can be added as needed
#
#    Example calculation is below this function

library(tidyverse)
library(ggpubr)

ewers_plot_all <- function(data, X, Y,Z, start,end){
  
  start.lab = as.name(paste("x",as.character(start),sep = ""))
  end.lab = as.name(paste("x",as.character(end),sep = ""))
  x.lab = as.name(X)
  y.lab = as.name(Y)
  z.lab = as.name(Z)
  #
  # Cut data for years and variables only, adjust for per capita
  data_cut = data %>% 
    dplyr::filter(Year == start | Year == end) %>% 
    dplyr::select(Area,Year,Population,!!X,!!Y, !!Z) %>% 
    dplyr::filter(!is.na(!!x.lab) & !is.na(!!y.lab)) %>% 
    dplyr::mutate(x_percap = !!x.lab/Population, y_percap = !!y.lab/Population, z_percap = !!z.lab/Population) %>% 
    dplyr::select(Area,Year,x_percap,y_percap,z_percap) %>% 
    pivot_wider(names_from = Year, values_from = c(x_percap,y_percap,z_percap)) %>% 
    dplyr::mutate(x_dif = !!as.name(paste("x_percap_",as.character(end), sep = "")) / !!as.name(paste("x_percap_",as.character(start), sep = "")),
                  y_dif = !!as.name(paste("y_percap_",as.character(end), sep = "")) / !!as.name(paste("y_percap_",as.character(start), sep = "")),
                  z_dif = !!as.name(paste("z_percap_",as.character(end), sep = "")) / !!as.name(paste("z_percap_",as.character(start), sep = "")))
  
  mz <- 5*mean(data_cut$z_dif,na.rm = T)
  lmz <- mean(data_cut$z_dif,na.rm = T)/5
  data_cut = data_cut %>% 
    dplyr::mutate(log.x.dif = log(x_dif),
                  log.y.dif = log(y_dif),
                  log.z.dif = ifelse(z_dif >= (mz)| z_dif <= (lmz)| is.na(z_dif), NA ,z_dif)) %>% 
    dplyr::filter(!is.na(log.x.dif) & !is.na(log.y.dif))

# Fix how model runs later
#lm = lm(log.y.dif ~ log.x.dif, data = data_cut)

#lm.coef = round(as.data.frame(coef(lm))[2,1],digits = 3)

  plot <- ggplot(data = data_cut, mapping = aes(x = log.x.dif, y = log.y.dif, color = log.z.dif)) + theme_classic() +
    geom_text(aes(label = Area), size = 3.5 ) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
    geom_smooth(method = "lm")  + 
    labs(x = x.lab, y = y.lab, title = paste(as.character(start),"to",as.character(end), "n = ", as.character(nrow(data_cut))), color = z.lab) +
    scale_color_viridis_c(begin = 0.2,end = .9,option = "inferno", na.value = "grey50") + theme(legend.position = "bottom")
 # plot.coef <- ggplot(data = data_cut, mapping = aes(x = log.x.dif, y = log.y.dif)) + theme_classic() +
   # geom_point() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + geom_smooth(method = "lm")  + labs(x = x.lab, y = y.lab, title = paste(as.character(start),"to",as.character(end), "n = ", as.character(nrow(data_cut)), "coef =", as.character(lm.coef)) )
  
  return(plot)
 # if_else(is.na(lm.coef), return(plot),return(plot.coef))
}


fao_composite_tall = read.csv("../data/fao_composite_tall.csv", header = T, sep = ",")

n_a <- ewers_plot_all(data = fao_composite_tall, 
               X = "kcal.ha.tot",
               Y = "area.tot",
               Z = "tonnes_nitrogen",
               start = 2010,
               end = 2017)

p_a <- ewers_plot_all(data = fao_composite_tall, 
                       X = "kcal.ha.tot",
                       Y = "area.tot",
                       Z = "tonnes_pest_total",
                   start = 2010,
                   end = 2017)

e_a <- ewers_plot_all(data = fao_composite_tall, 
                      X = "kcal.ha.tot",
                      Y = "area.tot",
                      Z = "CO2_eq_emissions",
                      start = 2010,
                      end = 2017)
exp_a <- ewers_plot_all(data = fao_composite_tall, 
                      X = "kcal.ha.tot",
                      Y = "area.tot",
                      Z = "export_value_usd",
                      start = 2000,
                      end = 2010)
exp_a
ewers_modifiers_plot_10to17 <- ggarrange(n_a,p_a,e_a, nrow = 1)
ewers_modifiers_plot_10to17
ggsave("../Figures/Ewers_nitrogen_10to17.png", n_a,width = 10, height = 8)
ggsave("../Figures/Ewers_pesticides_10to17.png", p_a,width = 10, height = 8)
ggsave("../Figures/Ewers_emissions_10to17.png", e_a,width = 10, height = 8)
ggsave("../Figures/Ewers_exports_00to10.png", exp_a,width = 10, height = 8)




# Example Calculations
nitro_value_8090 <- ewers_plot_all(data = fao_composite_tall, 
               X = "tonnes_nitrogen",
               Y = "total_ag_value_pin",
               start = 1980,
               end = 1990)

nitro_value_9000 <- ewers_plot_all(data = fao_composite_tall, 
                               X = "tonnes_nitrogen",
                               Y = "total_ag_value_pin",
                               start = 1990,
                               end = 2000)

nitro_value_0010 <- ewers_plot_all(data = fao_composite_tall, 
                               X = "tonnes_nitrogen",
                               Y = "total_ag_value_pin",
                               start = 2000,
                               end = 2010)

nitro_value_1018 <- ewers_plot_all(data = fao_composite_tall, 
                               X = "tonnes_nitrogen",
                               Y = "total_ag_value_pin",
                               start = 2010,
                               end = 2018)

ggarrange(nitro_value_8090,nitro_value_9000,nitro_value_0010,nitro_value_1018, ncol = 2,nrow =2)




ewers_plot_all(data = fao_composite_tall, 
               X = "kcal.ha.tot",
               Y = "total_ag_value_pin",
               start = 1990,
               end = 2000)


# IGNORE ####

# Initial Joining of Data to single Sheet ####

# Previously Adapted Values
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

area.tall <- pivot_longer(data = staple_data_clean 
                          %>% filter(Element == "Area harvested"),cols = 8:66, names_to = "Year", values_to = "area")%>% 
  dplyr::select(Area, Item, Year,area)
area.tall$Year <- as.numeric(area.tall$Year)
head(area.tall)

yield.tall <- pivot_longer(data = staple_data_clean %>% filter(Element == "Yield"),cols = 8:66, names_to = "Year", values_to = "yield") 
yield.tall$Year <- as.numeric(yield.tall$Year)

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
  dplyr::mutate(kcal.ha = yield*kcal.hg) %>% 
  dplyr::select(Area, Item, Year,kcal.ha) %>% 
  dplyr::filter(!is.na(kcal.ha) )
View(kcal.ha)
# Load and Clean Pop data
pop_data <- read.csv("../data/fao_pop.csv", sep = ",", header = T) %>% 
  dplyr::mutate(Population = Value*1000) %>% 
  dplyr::select(Area,Population,Year)

names(pop_data) <- as.character(names(pop_data))
pop_data_clean <- pop_data

# Data Split by Staple
split_data <- left_join(area.tall,kcal.ha) 
str(kcal.ha)
# Summed by Year 
area_yield_data <- split_data %>% 
  group_by(Area,Year) %>% 
  dplyr::summarise(kcal.ha.tot = sum(kcal.ha),
                   area.tot = sum(area))
area_yield_data

#####################
# New Values
nitrogen_data <- read.csv("../data/fao_nitrogen.csv", sep = ",", header = T) %>% 
  dplyr::mutate(tonnes_nitrogen = Value) %>% 
  dplyr::select(Area,tonnes_nitrogen,Year)
head(nitrogen_data)

imp_data <- read.csv("../data/fao_imp_exp.csv", sep = ",", header = T) %>% 
  dplyr::filter(Element == "Import Value") %>% 
  dplyr::mutate(import_value_usd = Value*1000) %>% 
  dplyr::select(Area,import_value_usd,Year)

exp_data <- read.csv("../data/fao_imp_exp.csv", sep = ",", header = T) %>% 
  dplyr::filter(Element == "Export Value") %>% 
  dplyr::mutate(export_value_usd = Value*1000) %>% 
  dplyr::select(Area,export_value_usd,Year)

ag_value_data <- read.csv("../data/fao_ag_value.csv", sep = ",", header =T)%>% 
  dplyr::mutate(total_ag_value_pin = Value) %>% 
  dplyr::select(Area,total_ag_value_pin,Year)

# Even newer values
pest_total_data <- read.csv("../data/fao_pest_total.csv", sep = ",", header =T)%>% 
  dplyr::mutate(tonnes_pest_total = Value) %>% 
  dplyr::select(Area,tonnes_pest_total,Year)

for_invest_data <- read.csv("../data/fao_foreign_invest.csv", sep = ",", header =T)%>% 
  dplyr::mutate(for_invest_usd = Value) %>% 
  dplyr::select(Area,for_invest_usd,Year)

emissions_data <- read.csv("../data/fao_emissions.csv", sep = ",", header =T)%>% 
  dplyr::mutate(CO2_eq_emissions = Value) %>% 
  dplyr::select(Area,CO2_eq_emissions,Year)


mergeset <- read.csv("../data/mergeset.csv", sep = ",", header = T) %>% 
  dplyr::select(1,2,4,5)
head(mergeset)

fao_composite_tall <- left_join(pop_data_clean,nitrogen_data) %>% 
  left_join(.,imp_data) %>% 
  left_join(.,exp_data) %>% 
  left_join(.,ag_value_data) %>% 
  left_join(.,area_yield_data) %>% 
  left_join(.,pest_total_data) %>% 
  left_join(.,for_invest_data) %>% 
  left_join(.,emissions_data) %>% 
  left_join(.,mergeset)
View(fao_composite_tall)
write.csv(fao_composite_tall, "../data/fao_composite_tall.csv")
View(fao_composite_tall)




