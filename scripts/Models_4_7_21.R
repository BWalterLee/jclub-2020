# 4_7_2021 Requested Analyses for Journal Club

library(tidyverse)
library(ggpubr)

fao_updated_staple = read.csv("../data/fao_updated_staple.csv", header = T, sep = ",")
fao_updated_staple$HDI  <- factor(fao_updated_staple$HDI, levels = c("Low", "Medium", "High", "Very high"))
# Functions: ####
  # Non-Z function 
ewers_plot_avg_noZ <- function(data, X, Y, start, end, facet= NA){
  
  start.lab = as.name(paste("x",as.character(start),sep = ""))
  end.lab = as.name(paste("x",as.character(end),sep = ""))
  x.lab = as.name(X)
  y.lab = as.name(Y)
 
  #
  # Cut data for years and variables only, adjust for per capita
  data_cut = data %>% 
    # dplyr::filter(export_value_usd >= 200000) %>%  # FILTER COUNTRIES HERE
    dplyr::select(Area,Year,Population,!!X,!!Y) %>%    # 
    dplyr::filter(!is.na(!!x.lab) & !is.na(!!y.lab)) %>%   # 
    dplyr::filter(between(Year,(start-2),(start+2))|between(Year,(end-2),(end+2)))   %>%                            # Select Years
    dplyr::mutate(x_percap = !!x.lab/Population, y_percap = !!y.lab/Population,
                  time = if_else(Year <= (start+2) & Year >= (start-2),"start.5","end.5"))%>% #Note that X here now is not per-capita
    dplyr::select(Area,time,x_percap,y_percap) %>% 
    dplyr::group_by(Area,time) %>% 
    dplyr::summarize(avg.x = mean(x_percap,na.rm = T),avg.y = mean(y_percap,na.rm = T))%>% 
    pivot_wider(names_from = time, values_from = c(avg.x,avg.y))  %>% 
    dplyr::mutate(x_dif = avg.x_end.5 / avg.x_start.5,
                  y_dif = avg.y_end.5 / avg.y_start.5) 
  
  data_cut = data_cut %>% 
    dplyr::mutate(log.x.dif = log(x_dif), 
                  log.y.dif = log(y_dif)
                  ) %>% 
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
  
  
  plot <- ggplot(data = data_cut_facet, mapping = aes(x = log.x.dif, y = log.y.dif)) + theme_classic() +
    geom_point(size = 3, alpha = .75) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
    geom_smooth(method = "lm")  + 
    labs(x = x.lab, y = y.lab, title = paste(as.character(start),"to",as.character(end), "n = ", as.character(nrow(data_cut)))) + 
    theme(legend.position = "bottom")+ ylim(-2.5,1) + xlim(-1.5,1)
  # plot.coef <- ggplot(data = data_cut, mapping = aes(x = log.x.dif, y = log.y.dif)) + theme_classic() +
  # geom_point() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + geom_smooth(method = "lm")  + labs(x = x.lab, y = y.lab, title = paste(as.character(start),"to",as.character(end), "n = ", as.character(nrow(data_cut)), "coef =", as.character(lm.coef)) )
  
  plot <- plot + 
    {if(!is.na(facet)) facet_wrap(facet)
    }
  
  return(plot)
  # if_else(is.na(lm.coef), return(plot), return(plot.coef))
} 

ewers_plot_avg_data_noZ <- function(data, X, Y, start, end, facet= NA){
  
  start.lab = as.name(paste("x",as.character(start),sep = ""))
  end.lab = as.name(paste("x",as.character(end),sep = ""))
  x.lab = as.name(X)
  y.lab = as.name(Y)
  
  #
  # Cut data for years and variables only, adjust for per capita
  data_cut = data %>% 
    # dplyr::filter(export_value_usd >= 200000) %>%  # FILTER COUNTRIES HERE
    dplyr::select(Area,Year,Population,!!X,!!Y) %>%    # 
    dplyr::filter(!is.na(!!x.lab) & !is.na(!!y.lab)) %>%   # 
    dplyr::filter(between(Year,(start-2),(start+2))|between(Year,(end-2),(end+2)))   %>%                            # Select Years
    dplyr::mutate(x_percap = !!x.lab/Population, y_percap = !!y.lab/Population,
                  time = if_else(Year <= (start+2) & Year >= (start-2),"start.5","end.5"))%>% #Note that X here now is not per-capita
    dplyr::select(Area,time,x_percap,y_percap) %>% 
    dplyr::group_by(Area,time) %>% 
    dplyr::summarize(avg.x = mean(x_percap,na.rm = T),avg.y = mean(y_percap,na.rm = T))%>% 
    pivot_wider(names_from = time, values_from = c(avg.x,avg.y))  %>% 
    dplyr::mutate(x_dif = avg.x_end.5 / avg.x_start.5,
                  y_dif = avg.y_end.5 / avg.y_start.5) 
  
  data_cut = data_cut %>% 
    dplyr::mutate(log.x.dif = log(x_dif), 
                  log.y.dif = log(y_dif)
    ) %>% 
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
 
  return(data_cut_facet)
  # if_else(is.na(lm.coef), return(plot), return(plot.coef))
} 
# "npc" mean "non per-capita", so Znpc means the Z value is not adjusted to per-capita

    # Z npc Plotting Function
ewers_plot_avg_Znpc <- function(data, X, Y, Z, start, end, facet= NA){
  
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
    dplyr::mutate(x_percap = !!x.lab/Population, y_percap = !!y.lab/Population, z_percap = !!z.lab,
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
    dplyr::select(2,16,18,19) %>% 
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

    # Z npc Data Function
ewers_plot_avg_data_Znpc <- function(data, X, Y, Z, start, end, facet= NA){
  
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
    dplyr::mutate(x_percap = !!x.lab/Population, y_percap = !!y.lab/Population, z_percap = !!z.lab,
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

    # Z pc Plotting Function
ewers_plot_avg_Zpc <- function(data, X, Y, Z, start, end, facet= NA){
  
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
    dplyr::mutate(x_percap = !!x.lab/Population, y_percap = !!y.lab/Population, z_percap = !!z.lab/Population,
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
    dplyr::select(2,16,18,19) %>% 
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

    # Z pc Data Function
ewers_plot_avg_data_Zpc <- function(data, X, Y, Z, start, end, facet= NA){
  
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
    dplyr::mutate(x_percap = !!x.lab/Population, y_percap = !!y.lab/Population, z_percap = !!z.lab/Population,
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
# end functions ####

# Fig 1: Ewers Analysis 1999-2015 ####
    # Figure 1
base_ewers_79_99 <- ewers_plot_avg_noZ(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
                   X = "kcal.ha.avg",
                   Y = "area.tot",
                   start = 1979,
                   end = 1999) 
base_ewers_95_15 <- ewers_plot_avg_noZ(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
                                       X = "kcal.ha.avg",
                                       Y = "area.tot",
                                       start = 1995,
                                       end = 2015) 
JCfig_1 <- ggarrange(base_ewers_79_99,base_ewers_95_15, common.legend = T)
JCfig_1
    # Alternate Figure 1 with faceting
facet_ewers_79_99 <- ewers_plot_avg_noZ(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
                                       X = "kcal.ha.avg",
                                       Y = "area.tot",
                                       facet = "HDI",
                                       start = 1979,
                                       end = 1999) 
facet_ewers_95_15 <- ewers_plot_avg_noZ(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
                                       X = "kcal.ha.avg",
                                       Y = "area.tot",
                                       facet = "HDI",
                                       start = 1995,
                                       end = 2015) 
JCfig_1_opt2 <- ggarrange(facet_ewers_79_99, facet_ewers_95_15, common.legend = T)
JCfig_1_opt2
  # Analyses
ewers_79_99_data <- ewers_plot_avg_data_noZ(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
                                        X = "kcal.ha.avg",
                                        Y = "area.tot",
                                        start = 1979,
                                        end = 1999)

ewers_95_15_data <- ewers_plot_avg_data_noZ(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
                                            X = "kcal.ha.avg",
                                            Y = "area.tot",
                                            start = 1995,
                                            end = 2015)
    # Old Ewers Model
summary(lm(log.y.dif ~ log.x.dif, data = ewers_79_99_data))
    # New Ewers Model
summary(lm(log.y.dif ~ log.x.dif, data = ewers_95_15_data))

    # Old Ewers Model Faceted
summary(lm(log.y.dif ~ log.x.dif*HDI, data = ewers_79_99_data))
    # New Ewers Model Faceted
summary(lm(log.y.dif ~ log.x.dif*HDI, data = ewers_95_15_data))

# Fig 3: Nitrogen Analysis ####
# Initial datasets
data79_99_nitro <- ewers_plot_avg_data_Znpc(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
                                         X = "kcal.ha.avg",
                                         Y = "area.tot",
                                         Z = "tonnes_nitrogen",
                                         start = 1979,
                                         end = 1999)

data95_15_nitro <- ewers_plot_avg_data_Znpc(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
                                         X = "kcal.ha.avg",
                                         Y = "area.tot",
                                         Z = "tonnes_nitrogen",
                                         start = 1995,
                                         end = 2015)
# Ewers Style Interaction Models
early_nitro <- ewers_plot_avg_Zpc(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
                                    X = "kcal.ha.avg",
                                    Y = "area.tot",
                                    Z = "tonnes_nitrogen",
                                    start = 1979,
                                    end = 1999)
summary(lm(log.y.dif ~ log.x.dif*log.z.dif, data = data79_99_nitro))
corrplot(cor(drop_na(fao_updated_staple %>% filter(Year == 2015) %>% 
               dplyr::select(3,tonnes_nitrogen,CO2_eq_emissions,tonnes_pest_total,kcal.ha.avg,mean.staple.exports,mean.staple.imports))))


recent_nitro <- ewers_plot_avg_Zpc(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
                                     X = "kcal.ha.avg",
                                     Y = "area.tot",
                                     Z = "tonnes_nitrogen",
                                     start = 1995,
                                     end = 2015)
summary(lm(log.y.dif ~ log.x.dif*log.z.dif, data = data95_15_nitro))

ggarrange(early_nitro,recent_nitro, nrow =1, common.legend = T)

# Nitro solo
nitro_on_area_95_15 <- ggplot(data = data95_15_nitro, mapping = aes(x = log.z.dif, y = log.y.dif)) + theme_classic() +
  geom_point(size = 3, alpha = .75) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  geom_smooth(method = "lm", alpha = .2)  + 
  labs(x = "Change in Nitrogen Applied in Agriculture", y = "Change in Area for Staple Crop Production") +
  scale_color_viridis_d(begin = 0.2,end = .9,option = "inferno", na.value = "grey50") + theme(legend.position = "bottom")
nitro_on_area_95_15
summary(lm(log.y.dif ~ log.z.dif, data = data95_15_nitro))


nitro_on_area_95_15_HDI <- ggplot(data = data95_15_nitro, mapping = aes(x = log.z.dif, y = log.y.dif)) + theme_classic() +
  geom_point(size = 3, alpha = .75) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  geom_smooth(method = "lm", alpha = .2)  + 
  labs(x = "Change in Nitrogen Applied in Agriculture", y = "Change in Area for Staple Crop Production") +
  scale_color_viridis_d(begin = 0.2,end = .9,option = "inferno", na.value = "grey50") + theme(legend.position = "bottom") +
  facet_wrap(~HDI)
nitro_on_area_95_15_HDI
summary(lm(log.y.dif ~ log.z.dif*HDI, data = data95_15_nitro))



# Fig 4: Import/Export Analysis ####
# First examination is using models with an interaction, 
#  which is kinda limited by the fact that the area ~ yield interaction is so weak.

# Mean staple exports over time 
# Data For Models
data79_99_exp <- ewers_plot_avg_data_Zpc(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
                                     X = "kcal.ha.avg",
                                     Y = "area.tot",
                                     Z = "mean.staple.exports",
                                     start = 1979,
                                     end = 1999)

data95_15_exp <- ewers_plot_avg_data_Zpc(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
                                     X = "kcal.ha.avg",
                                     Y = "area.tot",
                                     Z = "mean.staple.exports",
                                     start = 1995,
                                     end = 2015)

# Plots and models
early_exports <- ewers_plot_avg_Zpc(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
               X = "kcal.ha.avg",
               Y = "area.tot",
               Z = "mean.staple.exports",
               start = 1979,
               end = 1999)
summary(lm(log.y.dif ~ log.x.dif*log.z.dif, data = data79_99_exp))

recent_exports <- ewers_plot_avg_Zpc(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
               X = "kcal.ha.avg",
               Y = "area.tot",
               Z = "mean.staple.exports",
               start = 1995,
               end = 2015)
summary(lm(log.y.dif ~ log.x.dif*log.z.dif, data = data95_15_exp))

ggarrange(early_exports,recent_exports, nrow =1, common.legend = T)

# Mean staple imports over time 
# Data
data79_99_imp <- ewers_plot_avg_data_Zpc(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
                                     X = "kcal.ha.avg",
                                     Y = "area.tot",
                                     Z = "mean.staple.imports",
                                     start = 1979,
                                     end = 1999)

data95_15_imp <- ewers_plot_avg_data_Zpc(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
                                     X = "kcal.ha.avg",
                                     Y = "area.tot",
                                     Z = "mean.staple.imports",
                                     start = 1995,
                                     end = 2015)

ewers_plot_avg_Zpc(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
               X = "kcal.ha.avg",
               Y = "area.tot",
               Z = "mean.staple.imports",
               start = 1979,
               end = 1999)
summary(lm(log.y.dif ~ log.x.dif*log.z.dif, data = data79_99_imp))

ewers_plot_avg_Zpc(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
               X = "kcal.ha.avg",
               Y = "area.tot",
               Z = "mean.staple.imports",
               start = 1995,
               end = 2015)
summary(lm(log.y.dif ~ log.x.dif*log.z.dif, data = data95_15_imp))

# Given the lack of contribution of yield to area changes for 95-15 period, reexamining
#   using just exp/imports and area changes
#   X axis is now exp/imp value, Y still area total

#Exp
exp_on_area_95_15 <- ggplot(data = data95_15_exp, mapping = aes(x = log.z.dif, y = log.y.dif)) + theme_classic() +
  geom_point(size = 3, alpha = .75) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  geom_smooth(method = "lm", alpha = .2)  + 
  labs(x = "Change in Value of Staple Crop Exports", y = "Change in Area for Staple Crop Production") +
  scale_color_viridis_d(begin = 0.2,end = .9,option = "inferno", na.value = "grey50") + theme(legend.position = "bottom")
exp_on_area_95_15
summary(lm(log.y.dif ~ log.z.dif, data = data95_15_exp))


exp_on_area_95_15_HDI <- ggplot(data = data95_15_exp, mapping = aes(x = log.z.dif, y = log.y.dif)) + theme_classic() +
  geom_point(size = 3, alpha = .75) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  geom_smooth(method = "lm", alpha = .2)  + 
  labs(x = "Change in Value of Staple Crop Exports", y = "Change in Area for Staple Crop Production") +
  scale_color_viridis_d(begin = 0.2,end = .9,option = "inferno", na.value = "grey50") + theme(legend.position = "bottom") +
  facet_wrap(~HDI)
exp_on_area_95_15_HDI
Anova(lm(log.y.dif ~ log.z.dif*HDI, data = data95_15_exp))

#Imp
imp_on_area_95_15 <- ggplot(data = data95_15_imp, mapping = aes(x = log.z.dif, y = log.y.dif)) + theme_classic() +
  geom_point(size = 3, alpha = .75) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  geom_smooth(method = "lm", alpha = .2)  + 
  labs(x = "Change in Value of Staple Crop imports", y = "Change in Area for Staple Crop Production") +
  scale_color_viridis_d(begin = 0.2,end = .9,option = "inferno", na.value = "grey50") + theme(legend.position = "bottom")
imp_on_area_95_15
summary(lm(log.y.dif ~ log.z.dif, data = data95_15_imp))

imp_on_area_95_15_HDI <- ggplot(data = data95_15_imp, mapping = aes(x = log.z.dif, y = log.y.dif)) + theme_classic() +
  geom_point(size = 3, alpha = .75) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  geom_smooth(method = "lm", alpha = .2)  + 
  labs(x = "Change in Value of Staple Crop imports", y = "Change in Area for Staple Crop Production") +
  scale_color_viridis_d(begin = 0.2,end = .9,option = "inferno", na.value = "grey50") + theme(legend.position = "bottom") +
  facet_wrap(~HDI)
imp_on_area_95_15_HDI

ggarrange(exp_on_area_95_15_HDI,imp_on_area_95_15_HDI, nrow = 1)
summary(lm(log.y.dif ~ log.z.dif*HDI, data = data95_15_exp))
summary(lm(log.y.dif ~ log.z.dif*HDI, data = data95_15_imp))
 
# end ####




