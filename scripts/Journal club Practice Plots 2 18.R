# Last Update 2/20/2021


# Issue with missing data has been solved
# data_organization_script.R updated to ignore na for crop types
# fao_composite_tall.csv is now updated

# Install these packages
library(tidyverse)
library(ggpubr)




# Run this function
ewers_plot_all <- function(data, X, Y, Z, start, end, facet= NA){
  
  start.lab = as.name(paste("x",as.character(start),sep = ""))
  end.lab = as.name(paste("x",as.character(end),sep = ""))
  x.lab = as.name(X)
  y.lab = as.name(Y)
  z.lab = as.name(Z)
  #
  # Cut data for years and variables only, adjust for per capita
  data_cut = data %>% 
    dplyr::select(Area,Year,Population,!!X,!!Y, !!Z) %>%                      # grab only relevant data
    dplyr::filter(!is.na(!!x.lab) & !is.na(!!y.lab)) %>%   # filters out missing values
    dplyr::filter(Year == start | Year == end) %>%                            # Select Years
    dplyr::mutate(x_percap = !!x.lab, y_percap = !!y.lab/Population, z_percap = !!z.lab/Population) %>% #Note that X here now is not per-capita
    dplyr::select(Area,Year,x_percap,y_percap,z_percap)%>% 
    pivot_wider(names_from = Year, values_from = c(x_percap,y_percap,z_percap))  %>% 
    dplyr::mutate(x_dif = !!as.name(paste("x_percap_",as.character(end), sep = "")) / !!as.name(paste("x_percap_",as.character(start), sep = "")),
                  y_dif = !!as.name(paste("y_percap_",as.character(end), sep = "")) / !!as.name(paste("y_percap_",as.character(start), sep = "")),
                  z_dif = !!as.name(paste("z_percap_",as.character(end), sep = "")) / !!as.name(paste("z_percap_",as.character(start), sep = "")))
  
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
 
  data_cut_facet <- left_join(data_cut,merge_set) %>% 
    dplyr::filter(!is.na(.[,14:16]) & .[14:16] != "N/A")
 
  
  plot <- ggplot(data = data_cut_facet, mapping = aes(x = log.x.dif, y = log.y.dif, color = log.z.dif)) + theme_classic() +
    geom_text(aes(label = Area), size = 3.5 ) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
    geom_smooth(method = "lm")  + 
    labs(x = x.lab, y = y.lab, title = paste(as.character(start),"to",as.character(end), "n = ", as.character(nrow(data_cut))), color = z.lab) +
    scale_color_viridis_c(begin = 0.2,end = .9,option = "inferno", na.value = "grey50") + theme(legend.position = "bottom")
  # plot.coef <- ggplot(data = data_cut, mapping = aes(x = log.x.dif, y = log.y.dif)) + theme_classic() +
  # geom_point() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + geom_smooth(method = "lm")  + labs(x = x.lab, y = y.lab, title = paste(as.character(start),"to",as.character(end), "n = ", as.character(nrow(data_cut)), "coef =", as.character(lm.coef)) )

  plot <- plot + 
    {if(!is.na(facet)) facet_wrap(facet)
    }
   
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
#
#     Added the option to add a faceting factor to compare by country characteristics
#     Default is set to NA, but can set facet= "HDI", "Developed", or "Continent" 
#        Sometimes makes the plots more difficult to interpret, but cool over some time periods!

# For Example, here is change in caloric yield per capita on the X axis, 
#                      change in cropland area per capita on the Y axis (same as Ewers),
#                 and tonnes of nitrogen fertilizer added to fields per capita as the color scale 
#                 from years 2010 to 2017. 
ewers_nitro <- ewers_plot_all(data = fao_composite_tall %>% filter(Area != "Brunei Darussalam"), 
                      X = "kcal.ha.avg",
                      Y = "area.tot",
                      Z = "tonnes_nitrogen",
                      start = 1979,
                      end = 1999)

ewers_nitro_facet <- ewers_plot_all(data = fao_composite_tall %>% filter(Area != "Brunei Darussalam"), 
                              X = "kcal.ha.avg",
                              Y = "area.tot",
                              Z = "tonnes_nitrogen",
                              start = 1979,
                              end = 1999, facet = "HDI")

ggsave("../Figures/Ewers_nitro_final.png", ewers_nitro, width = 10.5, height = 8)
ggsave("../Figures/Ewers_nitro_final_facet.png", ewers_nitro_facet, width = 12, height = 9)


View(fao_composite_tall %>% filter(Continent == "Europe"))
# Example including facet
ewers_plot_all(data = fao_composite_tall, 
               X = "kcal.ha.avg",
               Y = "area.tot",
               Z = "tonnes_nitrogen",
               start = 1979,
               end = 1999, facet = "Continent")

# Feel free to try your own years by plugging into the template below! 
#      Some years may cause an error if there are too many NA's for the Z value
ewers_plot_all(data = fao_composite_tall, 
               X = "",
               Y = "",
               Z = "",
               start = ,
               end = )


# Examples from powerpoint
fao_composite_tall$HDI <- factor(fao_composite_tall$HDI,levels = c("Low","Medium", "High", "Very high"))
ewers_plot_all(data = fao_composite_tall %>% filter(Area != "Marshall Islands", Area != "Maldives"), 
               X = "kcal.ha.avg",
               Y = "area.tot",
               Z = "tonnes_pest_total",
               start = 2005,
               end = 2010)


