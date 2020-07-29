# Metadata ----------------------------------------------------------------
# Title: source code
# Purpose: Functions for 10n data processing
# Author(s): @pablocal
# Date Created: 2019-11-09
#
# Comments ----------------------------------------------------------------
# 
# 
# 
#
#
# Options and packages ----------------------------------------------------


## get avance data
get_avance <- function(path){
  
  col_names <- c("CAUT", "CPROV", "DEL_", "CMUN", "CDISTMUN", "MUN", "DEL_", "DEL_", "mesas_totales", 
                 "mesas_const", "mesas_suspendidas", "censo", "censo_const", "censo_susp", 
                 "avance1_mesas", "avance1_censo", "avance1_part",
                 "avance2_mesas", "avance2_censo", "avance2_part",
                 "DEL_")
  
  avan_mun <- read.csv2(path, header = F, col.names = col_names, stringsAsFactors = F) 
  
  
  return_df <- avan_mun %>% 
    select(-starts_with("DEL_")) %>% 
    arrange(CPROV, CMUN)
  
  return(return_df)
  
}
