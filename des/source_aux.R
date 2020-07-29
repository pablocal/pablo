# Metadata ----------------------------------------------------------------
# Title: Source
# Purpose: Aux function to handle electoral data
# Author(s): @pablocal
# Date Created: 2019-11-02
#
# Comments ----------------------------------------------------------------
# 
# 
# 
#
#
# Options and packages ----------------------------------------------------


compute_ipv <- function(data, years, parties){
  
  library(tidyverse)

  return_df <- data %>% 
    filter(year %in% years & party %in% parties) %>% 
    mutate(per_val = votes/valid*100,
           nat_per_val = nat_votes/nat_valid*100,
           pvi = per_val-nat_per_val,
           pvi_rel = per_val/nat_per_val) %>%
    group_by(cprov, party) %>% 
    summarise(pvi = mean(pvi),
              pvi_rel = mean(pvi_rel))
  
  return(return_df)
  
}

