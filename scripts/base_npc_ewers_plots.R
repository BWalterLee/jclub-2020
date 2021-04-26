# Base Non-per-capita ewers plots

ewers_plot_avg_noZ_npc <- function(data, X, Y, start, end, facet= NA){
  
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
    dplyr::mutate(x_percap = !!x.lab, y_percap = !!y.lab,
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
a <- ewers_plot_avg_noZ_npc(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
                   X = "kcal.ha.avg",
                   Y = "area.tot",
                   start = 1979,
                   end = 1999) 
b <- ewers_plot_avg_noZ_npc(data = fao_updated_staple %>% filter(Area != "Brunei Darussalam", Area != "Maldives"), 
                       X = "kcal.ha.avg",
                       Y = "area.tot",
                       start = 1995,
                       end = 2015) 

ggarrange(a,b,nrow = 1, common.legend = T)
