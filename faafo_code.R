
library(tidyverse)
library(plotrix)
library(lme4)
library(car)
library(emmeans)
library(dplyr)
library(ggpubr)
install.packages("directlabels")
library(directlabels)

# JC 2020 Hypothesis plotting
#  Urbanization rates correlated with shifts to commodity crop production?
#  Initial plots and fun having 
setwd("C:/Users/bwl42/Desktop/JC2020")

urb_perc_base <- read.csv("wb_urb_percent.csv", header = T, sep = ",")
fexp_base <- read.csv("wb_food_export_perc.csv", header = T, sep = ",")


Food_exp <- fexp_base %>%
  gather(key = Year, value = food_exp_percent, 5:64) %>% 
  filter(!is.na(Year)) %>% 
  dplyr::select(-X2020,-Country.Code,-Indicator.Name,-Indicator.Code)
Food_exp$Year = as.numeric(gsub("X","",Food_exp$Year))  
Food_exp$food_exp_percent = as.numeric(Food_exp$food_exp_percent)
View(Food_exp)


Urb_perc <- urb_perc_base %>%
  gather(key = Year, value = urb_percent, 5:64) %>% 
  filter(!is.na(Year)) %>% 
  dplyr::select(-X2020,-Country.Code,-Indicator.Name,-Indicator.Code)
Urb_perc$Year = as.numeric(gsub("X","",Urb_perc$Year))  
Urb_perc$urb_percent = as.numeric(Urb_perc$urb_percent)
View(Urb_perc)

Urb_Exp <- left_join(Urb_perc,Food_exp) %>% 
  rename(Country = ï..Country.Name)

  filter(!is.na(food_exp_percent), !is.na(urb_percent))
View(Urb_Exp)
head(Urb_Exp)

Big_Ones <- Urb_Exp %>% 
  filter(Country == "East Asia & Pacific"|Country == "European Union" | 
           Country == "Latin America & Caribbean"|Country == "South Asia" | Country == "Sub-Saharan Africa" |
           Country == "United States"|Country == "European Union" )

big_food <- ggplot(data = Big_Ones, mapping = aes(x = Year, y = food_exp_percent, color = Country)) +
  geom_line() + geom_dl(aes(label = Country), method = list(dl.trans(x = x + 0.2),"last.points"))+ theme_classic()+
  theme(legend.position = "none")+ xlim(1960,2040)
big_food
big_urb <- ggplot(data = Big_Ones, mapping = aes(x = Year, y = urb_percent, color = Country)) +
  geom_line()+ geom_dl(aes(label = Country), method = list(dl.trans(x = x + 0.2),"last.points"))+theme_classic()+
  theme(legend.position = "none")+ xlim(1960,2040)

ggarrange(big_urb,big_food, ncol = 2 )

big_food <- ggplot(data = Big_Ones, mapping = aes(x = Year, y = food_exp_percent, color = Country)) +
  geom_line() + geom_point(aes(y = urb_percent)) + geom_dl(aes(label = Country), method = list(dl.trans(x = x + 0.2),"last.points"))+ theme_classic()+
  theme(legend.position = "none")+ xlim(1960,2040)+ facet_wrap(~Country)
big_food



