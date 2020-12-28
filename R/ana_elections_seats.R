

#' Compute D'Hondt rule to assign seats
#'
#' This function computes the seats in election data.
#'
#' @param data Data frame. Input data frame with two columns party and votes.
#' @param party Character. Name of the column for parties.
#' @param votes Character. Name of the columns with the number of votes.
#' @param seats Numeric. Number of seats to be assigned.
#' @param blanks Numeric. Number of blank votes in the circunscription. Default is zero.
#' @param barrier Numeric. Vote percentage for electoral barrier. Default is zero.
#' @return A data frame with three columns party, votes and seats.
#' @export


seats_dhont <- function(df, party, votes, seats, blanks = 0, barrier = 0){


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
  return(return_df)
}
