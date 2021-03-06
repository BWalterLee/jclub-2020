# Full FAO Data Assembly to create fao_composite_tall.csv for use in function

# SOLVED PROBLEM WITH MISSING COUNTRIES

# Holy shit Ben you're trash, "Cottonseed" now changed to "Seed cotton" 3/24

# Added Food supply 3/24

staple_data <- read.csv("../data/Production_Crops_E_All_Data_NOFLAG.csv", sep = ",", header = T) %>% 
  dplyr::filter(Area.Code < 1000) %>% 
  dplyr::filter(Item =="Apples"| Item == "Barley"| Item ==  "Bananas"| Item == "Cassava"| Item == "Coconuts"| Item ==  "Seed cotton"| 
                  Item ==  "Grapes"| Item ==  "Maize"| Item ==  "Millet"| Item == "Oats"| Item ==  "Onions, dry"| Item ==  "Oil palm fruit"| 
                  Item ==  "Plantains"| Item ==  "Potatoes"| Item ==  "Rice, paddy"| Item ==  "Sorghum"| 
                  Item == "Soybeans"| Item ==  "Sugar beet"| Item ==  "Sugar cane"| Item ==  "Sunflower seed"| Item ==  "Sweet potatoes"| 
                  Item ==  "Vegetables, fresh nes"| Item == "Wheat"| Item ==  "Yams") %>% 
  dplyr::filter(Element != "Production") 
names(staple_data) = gsub(pattern = "Y", replacement = "", x = names(staple_data))
staple_data_clean <- staple_data
area.tall <- pivot_longer(data = staple_data_clean %>% filter(Element == "Area harvested"),cols = 8:66, names_to = "Year", values_to = "area")%>% 
  dplyr::select(Area, Item, Year,area)
area.tall$Year <- as.numeric(area.tall$Year)
head(area.tall)

yield.tall <- pivot_longer(data = staple_data_clean %>% filter(Element == "Yield"),cols = 8:66, names_to = "Year", values_to = "yield") 
yield.tall$Year <- as.numeric(yield.tall$Year)
View(yield.tall)
kcal = data.frame(c("Apples","Barley", "Bananas","Cassava","Coconuts", "Seed cotton", "Grapes", "Maize", "Millet",
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
write.csv(kcal, "../data/kcal_staple.csv") # NEVER AGAN

# Conversion
kcal.ha <- left_join(yield.tall,kcal %>% dplyr::select(Item,kcal.hg)) %>% 
  dplyr::mutate(kcal.per.ha = yield*kcal.hg) %>% 
  dplyr::select(Area, Item, Year,kcal.per.ha) %>% 
  dplyr::filter(!is.na(kcal.per.ha) )
View(kcal.ha)
# Load and Clean Pop data
pop_data <- read.csv("../data/fao_pop.csv", sep = ",", header = T) %>% 
  dplyr::mutate(Population = Value*1000) %>% 
  dplyr::select(Area,Population,Year)
str(pop_data_clean)
names(pop_data) <- as.character(names(pop_data))
pop_data_clean <- pop_data

# Data Split by Staple
split_data <- left_join(area.tall,kcal.ha)  
str(kcal.ha)
View(split_data)
# Summed by Year 
area_yield_data <- split_data %>% 
  mutate(kcal.total = area*kcal.per.ha) %>% 
  group_by(Area,Year) %>% 
  dplyr::summarise(kcal.produced.tot = sum(kcal.total,na.rm = T),
                   area.tot = sum(area,na.rm = T)) %>% 
  mutate(kcal.ha.avg = kcal.produced.tot / area.tot) %>% 
  dplyr::select(-kcal.produced.tot)


View(area_yield_data)
View(area_yield_data %>% 
       group_by(Area) %>% 
       summarize(test = sum(kcal.ha.avg)))

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

# Final Merge
mergeset <- read.csv("../data/mergeset.csv", sep = ",", header = T)

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

write.csv(fao_composite_tall, "../data/fao_staple_data.csv")

# New Data Sheet with specific staple values only where able ####

# Value of Ag Production Staple Only
staple_production_value <- read.csv("../data/Value_of_Production_E_All_Data_NOFLAG.csv", sep = ",", header = T) %>% 
  dplyr::filter(Area.Code < 1000, 
                Element == "Gross Production Value (constant 2014-2016 thousand I$)") %>% 
  dplyr::filter(Item =="Apples"| Item == "Barley"| Item ==  "Bananas"| Item == "Cassava"| Item == "Coconuts"| Item ==  "Seed cotton"| 
                  Item ==  "Grapes"| Item ==  "Maize"| Item ==  "Millet"| Item == "Oats"| Item ==  "Onions, dry"| Item ==  "Oil palm fruit"| 
                  Item ==  "Plantains"| Item ==  "Potatoes"| Item ==  "Rice, paddy"| Item ==  "Sorghum"| 
                  Item == "Soybeans"| Item ==  "Sugar beet"| Item ==  "Sugar cane"| Item ==  "Sunflower seed"| Item ==  "Sweet potatoes"| 
                  Item ==  "Vegetables, fresh nes"| Item == "Wheat"| Item ==  "Yams") %>% 
  dplyr::filter(Element != "Production") 
names(staple_production_value) = gsub(pattern = "Y", replacement = "", x = names(staple_data))
staple_value_clean <- staple_production_value
staple_value_tall <- pivot_longer(data = staple_value_clean,cols = 8:66, names_to = "Year", values_to = "staple.value")%>% 
  dplyr::select(Area, Item, Year,staple.value)
staple_value_tall$Year <- as.numeric(staple_value_tall$Year)

staple_value_complete <- staple_value_tall %>% 
  dplyr::group_by(Area, Year) %>% 
  dplyr::summarise(mean.staple.value = mean(staple.value, na.rm = T))
View(staple_value_complete)

# Imports Staple Only
staple_imports_raw <- read.csv("../data/fao_staple_import_value.csv", sep = ",", header = T) %>% 
  dplyr::mutate(staple.import.value = Value) %>% 
  dplyr::select(Area,Item,Year,staple.import.value) %>% 
  dplyr::group_by(Area,Year) %>% 
  dplyr::summarise(mean.staple.imports = mean(staple.import.value, na.rm = T))

# exports Staple Only
staple_exports_raw <- read.csv("../data/fao_staple_export_value.csv", sep = ",", header = T) %>% 
  dplyr::mutate(staple.export.value = Value) %>% 
  dplyr::select(Area,Item,Year,staple.export.value) %>% 
  dplyr::group_by(Area,Year) %>% 
  dplyr::summarise(mean.staple.exports = mean(staple.export.value, na.rm = T))

  

fao_staple_complete <- left_join(pop_data_clean,nitrogen_data) %>% 
  left_join(.,staple_imports_raw) %>% 
  left_join(.,staple_exports_raw) %>% 
  left_join(.,staple_value_complete) %>% 
  left_join(.,area_yield_data) %>% 
  left_join(.,pest_total_data) %>% 
  left_join(.,for_invest_data) %>% 
  left_join(.,emissions_data) %>% 
  left_join(.,mergeset)
View(fao_staple_complete)

write.csv(fao_staple_complete, "../data/fao_updated_staple.csv")

