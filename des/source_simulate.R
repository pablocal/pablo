# Metadata ----------------------------------------------------------------
# Title: Simulate election
# Purpose: Source code for simulate election functions
# Author(s): EduC & @pablocal
# Date Created: 2019-11-05
#
# Comments ----------------------------------------------------------------
# 
# 
# 
#
#
# Options and packages ----------------------------------------------------


# 1. Function to simulate election ----------------------------------------
simulate_election <- function(est, n_simulation, cov_mat, prov_id){    
  
  ## cov_mat = covariance matrix from wrap function
  ## est = estimate from wrap fucntion
  ## cprov is the standard province code
  ## n_simulation is the number of simulations
  require(tidyverse)
  require(mvtnorm)
  
  est_prov <- est %>% 
    filter(cprov == prov_id)
  
  party_names <- est_prov %>% 
    select(party) %>% 
    pull()
  
  ## we select the matrix for prov, only with the parties that are present in prov
  cov_prov <- cov_mat[party_names, party_names]
  
  ## the diagonal of the matrix corresponds to the standar deviation (errors) from the file
  ## this approach neds to be checked; not sure it's right
  diag(cov_prov) <- est_prov %>% 
    select(error) %>%
    sqrt() %>% 
    pull()
  
  ## delta are the point stimates for the parties in prov
  delta <- est_prov %>% 
    select(per_prov) %>% 
    pull()
  
  ## now the simulation; simulate 1,000 times the elections in prov
  ## the diagonal of the matrix corresponds to the variance and square root standar deviation (errors) 
  ## and we take the square root to compute the standar deviation (errors) from the file.
  df_number <- est_prov %>% 
    count() %>% 
    as.numeric()
  
  prov_elec_sim <- rmvt(n_simulation, delta = delta, sigma = cov_prov, df = df_number)
  
  #simulation to long format; recode per < 0 to 0
  colnames(prov_elec_sim) <- party_names
   
  return_df <- as_tibble(prov_elec_sim) %>%
       rowid_to_column("id") %>%
       gather("party", "per", 2:(length(party_names)+1)) %>%
       mutate(per = ifelse(per < 0, 0, per),
              cprov = prov_id,
              sim_id = id) %>% 
    select(cprov, sim_id, party, per)
    
  return(return_df)

  }
  
# 2. Function to compute seats for each simulations -----------------------

seats_simulation <- function(simu_df, election_totals){
  
  require(tidyverse)
  
  cprovs <- unique(simu_df$cprov)
  return_df <- map_df(cprovs, ~seats_simulation_to_map(simu_df = simu_df, election_totals = election_totals, prov_id = .x))
  
  return(return_df)
}

  

# 3. Seat simulation to be mapped -----------------------------------------

seats_simulation_to_map <- function(simu_df, election_totals, prov_id){
    
  require(tidyverse)
  source("source/source_dhont.R")
  
  
  ##creates dataset for adding numbers in next step
  param <- filter(election_totals, cprov == prov_id)
  
  valid_param <- param %>% 
    select(valid) %>% 
    pull() %>% 
    as.numeric()
  
  seats_param <- param %>% 
    select(seats) %>% 
    pull() %>% 
    as.numeric()
  
  blanks_param <- param %>% 
    select(blanks) %>% 
    pull() %>% 
    as.numeric() 
  
  prov_elec_to_seats <- simu_df %>%
    filter(cprov == prov_id) %>% 
    mutate(valid = valid_param,
           votes = round(per/100*valid, 0))
  
  max_simu <- max(prov_elec_to_seats$sim_id)
  
  return_df <- map_df(1:max_simu, ~seats_dhont(data = prov_elec_to_seats,
                                               party = "party",
                                               votes = "votes",
                                               prov_id = .x,
                                               seats = seats_param,
                                               blanks = blanks_param,
                                               barrier = 3))
  
  return_df <- return_df %>% 
    mutate(cprov = prov_id) %>% 
    rename(votes = Votes,
           party = Party,
           seats = esc) %>% 
    select(cprov, sim_id, party, votes, seats)
  
  return(return_df)
  
  }


