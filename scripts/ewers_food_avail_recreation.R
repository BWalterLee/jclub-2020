# Recreate Ewers Food- availability interaction model

# The Classics
library(tidyverse)
library(broom)
library(ggpubr)

# New 
fao_updated_staple = read.csv("../data/fao_updated_staple.csv", header = T, sep = ",")
fao_updated_staple$HDI  <- factor(fao_updated_staple$HDI, levels = c("Low", "Medium", "High", "Very high"))
data = read.csv("../data/fao_updated_staple.csv", header = T, sep = ",") 
View(fao_updated_staple)

# Functions 
# Plotting Function 
ewers_plot_avg <- function(data, X, Y, Z, start, end, facet= NA){
  
  start.lab = as.name(paste("x",as.character(start),sep = ""))
  end.lab = as.name(paste("x",as.character(end),sep = ""))
  x.lab = as.name(X)
  y.lab = as.name(Y)
  z.lab = as.name(Z)
  #
  # Cut data for years and variables only, adjust for per capita
  data_cut = data %>% 
    # dplyr::filter(export_value_usd >= 200000) %>%  # FILTER COUNTRIES HERE
    dplyr::select(Area,Year,Population,!!X,!!Y, !!Z) %>%    # 
    dplyr::filter(!is.na(!!x.lab) & !is.na(!!y.lab)) %>%   # 
    dplyr::filter(between(Year,(start-2),(start+2))|between(Year,(end-2),(end+2)))   %>%                            # Select Years
    dplyr::mutate(x_percap = !!x.lab, y_percap = !!y.lab/Population, z_percap = !!z.lab/Population,
                  time = if_else(Year <= (start+2) & Year >= (start-2),"start.5","end.5"))%>% #Note that X here now is not per-capita
    dplyr::select(Area,time,x_percap,y_percap,z_percap) %>% 
    dplyr::group_by(Area,time) %>% 
    dplyr::summarize(avg.x = mean(x_percap,na.rm = T),avg.y = mean(y_percap,na.rm = T),avg.z = mean(z_percap,na.rm = T))%>% 
    pivot_wider(names_from = time, values_from = c(avg.x,avg.y,avg.z))  %>% 
    dplyr::mutate(x_dif = avg.x_end.5 / avg.x_start.5,
                  y_dif = avg.y_end.5 / avg.y_start.5,
                  z_dif = avg.z_end.5 / avg.z_start.5) %>% 
    dplyr::filter(z_dif != Inf & z_dif != 0)
  
  mz <- 10*mean(data_cut$z_dif,na.rm = T)
  lmz <- mean(data_cut$z_dif,na.rm = T)/10
  data_cut = data_cut %>% 
    dplyr::mutate(log.x.dif = log(x_dif), 
                  log.y.dif = log(y_dif),
                  log.z.dif = ifelse(z_dif >= (mz)| z_dif <= (lmz)| is.na(z_dif), NA ,log(z_dif))) %>% 
    dplyr::filter(!is.na(log.x.dif) & !is.na(log.y.dif))
  
  # Fix how model runs later
  #lm = lm(log.y.dif ~ log.x.dif, data = data_cut)
  
  #lm.coef = round(as.data.frame(coef(lm))[2,1],digits = 3)
  head(data)
  merge_set <- data %>% 
    dplyr::select(2,14,16,17) %>% 
    distinct(.)
  head(merge_set)
  data_cut_facet <- left_join(data_cut,merge_set, by = "Area") %>% 
    dplyr::filter(!is.na(HDI) & HDI != "N/A")
  
  
  plot <- ggplot(data = data_cut_facet, mapping = aes(x = log.x.dif, y = log.y.dif, color = log.z.dif)) + theme_classic() +
    geom_point(size = 3, alpha = .75) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
    geom_smooth(method = "lm")  + 
    labs(x = x.lab, y = y.lab, title = paste(as.character(start),"to",as.character(end), "n = ", as.character(nrow(data_cut))), color = z.lab) +
    scale_color_viridis_c(begin = 0.2,end = .9,option = "inferno", na.value = "grey50") + theme(legend.position = "bottom")
  # plot.coef <- ggplot(data = data_cut, mapping = aes(x = log.x.dif, y = log.y.dif)) + theme_classic() +
  # geom_point() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + geom_smooth(method = "lm")  + labs(x = x.lab, y = y.lab, title = paste(as.character(start),"to",as.character(end), "n = ", as.character(nrow(data_cut)), "coef =", as.character(lm.coef)) )
  
  plot <- plot + 
    {if(!is.na(facet)) facet_wrap(facet)
    }
  
  return(plot)
  # if_else(is.na(lm.coef), return(plot), return(plot.coef))
} 

# Function to generate data frames for average values ####
ewers_plot_avg_data <- function(data, X, Y, Z, start, end, facet= NA){
  
  start.lab = as.name(paste("x",as.character(start),sep = ""))
  end.lab = as.name(paste("x",as.character(end),sep = ""))
  x.lab = as.name(X)
  y.lab = as.name(Y)
  z.lab = as.name(Z)
  #
  # Cut data for years and variables only, adjust for per capita
  data_cut = data %>% 
    dplyr::select(Area,Year,Population,!!X,!!Y, !!Z) %>%    # grab only relevant data
    dplyr::filter(!is.na(!!x.lab) & !is.na(!!y.lab)) %>%   # filters out missing values
    dplyr::filter(between(Year,(start-2),(start+2))|between(Year,(end-2),(end+2)))   %>%                            # Select Years
    dplyr::mutate(x_percap = !!x.lab, y_percap = !!y.lab/Population, z_percap = !!z.lab/Population,
                  time = if_else(Year <= (start+2) & Year >= (start-2),"start.5","end.5"))%>% #Note that X here now is not per-capita
    dplyr::select(Area,time,x_percap,y_percap,z_percap) %>% 
    dplyr::group_by(Area,time) %>% 
    dplyr::summarize(avg.x = mean(x_percap,na.rm = T),avg.y = mean(y_percap,na.rm = T),avg.z = mean(z_percap,na.rm = T))%>% 
    pivot_wider(names_from = time, values_from = c(avg.x,avg.y,avg.z))  %>% 
    dplyr::mutate(x_dif = avg.x_end.5 / avg.x_start.5,
                  y_dif = avg.y_end.5 / avg.y_start.5,
                  z_dif = avg.z_end.5 / avg.z_start.5)%>% 
    dplyr::filter(z_dif != Inf & z_dif != 0)
  
  mz <- 10*mean(data_cut$z_dif,na.rm = T)
  lmz <- mean(data_cut$z_dif,na.rm = T)/10
  data_cut = data_cut %>% 
    dplyr::mutate(log.x.dif = log(x_dif), 
                  log.y.dif = log(y_dif),
                  log.z.dif = ifelse(z_dif >= (mz)| z_dif <= (lmz)| is.na(z_dif), NA ,log(z_dif))) %>% 
    dplyr::filter(!is.na(log.x.dif) & !is.na(log.y.dif))
  
  # Fix how model runs later
  #lm = lm(log.y.dif ~ log.x.dif, data = data_cut)
  
  #lm.coef = round(as.data.frame(coef(lm))[2,1],digits = 3)
  head(data)
  merge_set <- data %>% 
    dplyr::select(2,16,18,19) %>% 
    distinct(.)
  head(merge_set)
  data_cut_facet <- left_join(data_cut,merge_set, by = "Area") %>% 
    dplyr::filter(!is.na(HDI) & HDI != "N/A")
  
  data_clean <- data_cut_facet %>% 
    dplyr::select(Area,log.x.dif,log.y.dif,log.z.dif,HDI,Developed,Continent)
  
  return(data_clean)
  # if_else(is.na(lm.coef), return(plot), return(plot.coef))
} 

# Recreate Original Ewers Model (ignore exports in this case)
data79_99_exp <- ewers_plot_avg_data(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
                                     X = "kcal.ha.avg",
                                     Y = "area.tot",
                                     Z = "mean.staple.exports",
                                     start = 1979,
                                     end = 1999)
ewers_plot_avg(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
               X = "kcal.ha.avg",
               Y = "area.tot",
               Z = "mean.staple.exports",
               start = 1979,
               end = 1999)
summary(lm(log.y.dif ~ log.x.dif, data = data79_99_exp))

# Add the start date food availability to the dataset for use as a predictor
food_avail_79 <- read.csv("../data/food-supply-kcal.csv", sep = ",", header = T) %>% 
  dplyr::filter(Code != "" & Year == 1979) %>% 
  dplyr::select(-Code, -Year)
    #View(food_avail_79)
data79_99_exp_fa <- left_join(data79_99_exp,food_avail_79)

# Plot and Model examining effects of Changes in staple crop Yield and food availability 
#   on area of cropland for staple production
a <- ggplot(data = data79_99_exp_fa, mapping = aes(x = log.x.dif, y = log.y.dif, color = kcal.capita.day)) + theme_classic() +
  geom_point(size = 3, alpha = .75) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  geom_smooth(method = "lm", alpha = .2)  + 
  labs(x = "Change in Yield (kcal/ha/percap) of Staple Crops", y = "Change in Area for Staple Crop Production",
       title = "1979-1999") +
  scale_color_viridis_c(begin = 0.2,end = .9,option = "inferno", na.value = "grey50") + theme(legend.position = "bottom") +
  facet_wrap(~HDI)


summary(lm(log.y.dif ~ log.x.dif*kcal.capita.day, data = data79_99_exp_fa))
      # Takeaways: - Land sparing IS occurring, increase yield decreased area used in ag
      #            - Countries with greater food availability somewhat improved sparing (not signifciantly)
      #            - Countries with high food availability and high yield did NOT spare land as well? 
      #            - Counterintuitive takeaway here...
      # BUT THIS IS ALL COUNTRIES!  Ewers looked only at Developing countries, so I'll try that

summary(lm(log.y.dif ~ log.x.dif*kcal.capita.day, data = data79_99_exp_fa %>% dplyr::filter(Developed == "Less developed")))


# New Modern Period
data95_15_exp <- ewers_plot_avg_data(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
                                     X = "kcal.ha.avg",
                                     Y = "area.tot",
                                     Z = "mean.staple.exports",
                                     start = 1995,
                                     end = 2015)

food_avail_95 <- read.csv("../data/food-supply-kcal.csv", sep = ",", header = T) %>% 
  dplyr::filter(Code != "" & Year == 1995) %>% 
  dplyr::select(-Code, -Year)
#View(food_avail_79)
data95_15_exp_fa <- left_join(data95_15_exp,food_avail_95)

b <- ggplot(data = data95_15_exp_fa %>% dplyr::filter(Area != "Trinidad and Tobago" & Area != "Saint Lucia"), mapping = aes(x = log.x.dif, y = log.y.dif, color = kcal.capita.day)) + theme_classic() +
  geom_point(size = 3, alpha = .75) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  geom_text(aes(label = Area), alpha = .5)+
  geom_smooth(method = "lm", alpha = .2)  + 
  labs(x = "Change in Yield (kcal/ha/percap) of Staple Crops", y = "Change in Area for Staple Crop Production",
       title = "1995-2015") +
  scale_color_viridis_c(begin = 0.2,end = .9,option = "inferno", na.value = "grey50") + theme(legend.position = "bottom") +
  facet_wrap(~HDI)

summary(lm(log.y.dif ~ log.x.dif*kcal.capita.day, data = data95_15_exp_fa %>% dplyr::filter(Developed == "Less developed")))


# Quick High HDI comparison
early_high <- ggplot(data = data79_99_exp_fa %>% dplyr::filter(Area != "Trinidad and Tobago" & Area != "Saint Lucia"  & Area != "Jordan"), mapping = aes(x = log.x.dif, y = log.y.dif, color = kcal.capita.day)) + theme_classic() +
  geom_point(size = 3, alpha = .75) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  geom_text(aes(label = Area), alpha = .5)+
  geom_smooth(method = "lm", alpha = .2)  + 
  labs(x = "Change in Yield (kcal/ha/percap) of Staple Crops", y = "Change in Area for Staple Crop Production",
       title = "1979-1999") +
  scale_color_viridis_c(begin = 0.2,end = .9,option = "inferno", na.value = "grey50") + theme(legend.position = "bottom")


late_high <- ggplot(data = data95_15_exp_fa %>% dplyr::filter(Area != "Trinidad and Tobago" & Area != "Saint Lucia" ), mapping = aes(x = log.x.dif, y = log.y.dif, color = kcal.capita.day)) + theme_classic() +
  geom_point(size = 3, alpha = .75) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  geom_text(aes(label = Area), alpha = .5)+
  geom_smooth(method = "lm", alpha = .2)  + 
  labs(x = "Change in Yield (kcal/ha/percap) of Staple Crops", y = "Change in Area for Staple Crop Production",
       title = "1995-2015") +
  scale_color_viridis_c(begin = 0.2,end = .9,option = "inferno", na.value = "grey50") + theme(legend.position = "bottom")

ggarrange(early_high,late_high, common.legend = T, nrow = 1)
