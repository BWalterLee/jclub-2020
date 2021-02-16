# Last Update 2/12/2021


# Install these packages
library(tidyverse)
library(ggpubr)

# Run this function
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

# Load this data, was attached to email
fao_composite_tall = read.csv("../data/fao_composite_tall.csv", header = T, sep = ",")

# Now you can use this function to visualize any trend you want from the time period 1979-2018,
#  though some data will be missing from those time periods. 

# Can select from any of these values
#    X , Y, or Z can currently be:
#      -"tonnes_nitrogen"      (ag use only)
#      -"import_value_usd"     (ag products only)
#      -"export_value_usd"     (ag products only)
#      -"total_ag_value_pin"   (PIN is FAO "Production Index Number")
#      -"kcal.ha.tot"          (still from only staple crops)
#      -"area.tot"             (still from only staple crops)
#      -"tonnes_pest_total"    (pesticide use for agriculture per capita)
#      -"for_invest_usd"       (Foreign investment in agriculture per capita)
#      -"CO2_eq_emissions"     (CO2 equivalent emissions per capita)

# For Example, here is change in caloric yield per capita on the X axis, 
#                      change in cropland area per capita on the Y axis (same as Ewers),
#                 and tonnes of nitrogen fertilizer added to fields per capita as the color scale 
#                 from years 2010 to 2017. 
ewers_plot_all(data = fao_composite_tall, 
                      X = "kcal.ha.tot",
                      Y = "area.tot",
                      Z = "tonnes_nitrogen",
                      start = 2010,
                      end = 2017)

# Feel free to try your own years by plugging into the template below! 
#      Some years may cause an error if there are too many NA's for the Z value
ewers_plot_all(data = fao_composite_tall, 
               X = "",
               Y = "",
               Z = "",
               start = ,
               end = )




