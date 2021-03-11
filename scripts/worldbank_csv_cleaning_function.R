# World Bank Data Cleaning Function ####

# This function was written to be compatible with downloadable csv files from
#   https://data.worldbank.org . Simply click the "Download CSV" button on the
#    bottom half of the screen, and this function should be able to chop off
#    unnecessary headers and get it into an easier to analyze format.


#  Input is "data = ..." but header must =F for the file,
#   and format = "long" or "wide" 
#   "long" format is preferred for time-series analysis, but 
#   "wide" may be preferable for viewing of a single country responses over time


wb_dataclean <- function(data, format){
  
  # Remove Excess Columns
  chop_top <- data.frame(data[-c(1,2),]) 
  
  # Recreate Header
  colnames(chop_top) <- c(chop_top[1,])
  
  # Remove old header row
  wide_data <- chop_top[-1,]
  
  # Pivot to long format
  long_data = pivot_longer(data = wide_data, cols = !c(1:4), names_to = "Year", values_to = "count")
  
  # Return data based on format choice
  ifelse(format == "wide", return(wide_data), return(long_data))
  
}

# Example of how this function is used ####

#  Let's say the file we downloaded is called "wb_urb_total.csv" 

#  We would use the function to make a new object, called "Urb_pop_clean" 
#  then run the function on our downloaded file, like this

Urb_pop_clean <- wb_dataclean(data = read.csv("../data/wb_urb_total.csv", header = F),format = "long")







