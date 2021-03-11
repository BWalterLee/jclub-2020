
# Load Package
library(tidyverse)

# Load Data
fao_composite_tall = read.csv("../data/fao_composite_tall.csv", header = T, sep = ",")

# Run Function 
ewers_plot_avg <- function(data, X, Y, Z, start, end, facet= NA){
  
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
                  z_dif = avg.z_end.5 / avg.z_start.5)
  
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
    geom_text(aes(label = Area), size = 4, alpha = .75) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
    geom_smooth(method = "lm", alpha = .2)  + 
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

# Show Plot Using all Countries

ewers_plot_avg(data = fao_composite_tall, 
               X = "kcal.ha.avg",
               Y = "area.tot",
               Z = "tonnes_nitrogen",
               start = 1979,
               end = 1999)

# Show Plot using only Specified Countries
ewers_plot_avg(data = fao_composite_tall %>% filter(Area == "Nigeria"| Area == "Sudan" | Area == "Ethiopia" |
                                                      Area == "Brazil" | Area == "Indonesia"), 
               X = "kcal.ha.avg",
               Y = "area.tot",
               Z = "tonnes_nitrogen",
               start = 1995,
               end = 2015)


