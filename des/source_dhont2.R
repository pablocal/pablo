# Metadata ----------------------------------------------------------------
# Title:
# Purpose:
# Author(s):
# Date Created: 2019-10-04
#
# Comments ----------------------------------------------------------------
# 
# 
# 
#
#
# Options and packages ----------------------------------------------------

# 1. Data -----------------------------------------------------------------

seats_dhont <- function(data, party, votes, prov_id, seats, blanks = 0, barrier = 0){
  
  require(tidyverse)
  df <- data[data$CPROV == prov_id, c(party, votes)]
   
  colnames(df) <- c("Party", "Votes")
  df <- arrange(df, -Votes)
  
  ### apply barrier ###
  if(barrier > 0){
    df_dhont <- df %>%
      mutate(prc = Votes/(sum(Votes)+blanks)*100,
             Barrier = ifelse(prc > !! barrier, "Y", "N")) %>%
      filter(Barrier == "Y")
  } else {
    df_dhont <- df
  }
  
  ### prepare matrix for dhont###
  dhont <- matrix(nrow = nrow(df_dhont), ncol = seats)
  dhont[,1] <- as.matrix(df_dhont$Votes)
  
  if(seats > 1){
  for(i in 2:seats) {
    dhont[ , i] <- dhont[ , 1]/i
  }
  }
  ### set cut point for seats ###
  limit <- sort(as.vector(dhont))[length(as.vector(dhont))-(seats-1)]
  ### compute seats ###
  esc <- dhont >= limit
  esc <- 1*esc
  esc <- rowSums(esc)

    
  ### return df###
  if(nrow(df) != length(esc)){
    length_esc <- length(esc)
    nrow_df <- nrow(df)
    esc <- c(esc, replicate(nrow_df-length_esc, 0))
  }
  
  return_df <- cbind(df, esc)
  return_df$CPROV <- prov_id
  return(return_df)
}
