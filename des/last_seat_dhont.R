


##### seats dhont last
last_seat_dhont <- function(data, cprov, party, votes, seats, blanks = 0, barrier = 0){
  require(tidyverse)
  
  df <- data[data$CPROV == cprov,c(party, votes)]
  colnames(df) <- c("Party", "Votes")
  ### apply barrier ###
  if(barrier > 0){
    df_dhont <- df %>%
      mutate(prc = Votes/(sum(Votes)+blanks)*100,
        Barrier = ifelse(prc > !! barrier, "Y", "N")) %>%
      filter(Barrier ==  "Y")
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
  primer_sin <- sort(as.vector(dhont), partial = length(as.vector(dhont))- (seats))[length(as.vector(dhont)) - (seats)]
  
  ### compute seats ###
  esc <- dhont >= limit
  esc <- 1*esc
  esc <- rowSums(esc)
  
  ### set last and difference ##
  pos_primer_sin <- which(dhont == primer_sin, arr.ind=T)
  vfalta <- as.integer((limit - primer_sin)*pos_primer_sin[, 2])[1]+1
  
  pultimo <- which(dhont == limit, arr.ind=T)
  pultimo <- pultimo[, 1]
  pultimo <- df_dhont$Party[pultimo][1]
 
   if(nrow(df_dhont) != nrow(df)){
  total_votos <- sum(df$Votes)+blanks
  df_barrera <-  df %>%
    mutate(prc = Votes/(sum(Votes)+blanks)*100,
           Barrier = ifelse(prc > !! barrier, "Y", "N")) %>%
    filter(Barrier ==  "N") %>% 
    rowwise() %>% 
    mutate(vfalta_seat = limit - Votes,
           vfalta_barrera = find_votos_barrera(votos = Votes, barrera = barrier, total_votos = total_votos) - Votes,
           next_seat = ifelse(vfalta_seat < vfalta & vfalta_barrera < vfalta & Votes == max(Votes), 1, 0))
  
  if(sum(df_barrera$next_seat, na.rm = T) == 1) {
    
    pprimer_sin <- df_barrera$Party[which(df_barrera$next_seat == 1)]
    vfalta <- max(df_barrera$vfalta_seat[which(df_barrera$next_seat == 1)], df_barrera$vfalta_barrera[which(df_barrera$next_seat == 1)])
    
  } else {
    
    pprimer_sin <- which(dhont == primer_sin, arr.ind=T)
    pprimer_sin <- pprimer_sin[, 1]
    pprimer_sin <- df_dhont$Party[pprimer_sin][1]
    
  }
  
  } else {
 
  pprimer_sin <- which(dhont == primer_sin, arr.ind=T)
  pprimer_sin <- pprimer_sin[, 1]
  pprimer_sin <- df_dhont$Party[pprimer_sin][1]
  }
  
  prov_name <- data[data$CPROV == cprov,]
  prov_name <- prov_name$NAME[1]
  ### return df###
  return_df <- data.frame(CPROV = cprov, PROV = prov_name, ultimo_esc = pultimo, primer_sin_esc = pprimer_sin,
    diferencia_votos = as.integer(vfalta), diferencia_rel_candid = round(vfalta/(sum(df$Votes)+blanks)*100, 1),
    total_esc = seats, stringsAsFactors = F)
  return(return_df)
}




##### seats dhont last per party
next_seat_dhont_party <- function(data, party, votes, seats, blanks = 0, barrier = 0){
  require(tidyverse)
  df <- data[,c(party, votes)]
  colnames(df) <- c("Party", "Votes")
  ### apply barrier ###
  if(barrier > 0){
    df_dhont <- df %>%
      mutate(prc = Votes/(sum(Votes)+blanks)*100,
        Barrier = ifelse(prc > !! barrier, "Y", "N")) %>%
      filter(Barrier ==  "Y")
  } else {
    df_dhont <- df
  }
  ### prepare matrix for dhont###
  dhont <- matrix(nrow = nrow(df_dhont), ncol = seats)
  dhont[,1] <- as.matrix(df_dhont$Votes)
  for(i in 2:seats) {
    dhont[ , i] <- dhont[ , 1]/i
  }
  ### set cut point for seats ###
  limit <- sort(as.vector(dhont))[length(as.vector(dhont))-(seats-1)]
  limit_pen <- sort(as.vector(dhont))[length(as.vector(dhont))-(seats-2)]
  primer_sin <- sort(as.vector(dhont), partial = length(as.vector(dhont))- (seats))[length(as.vector(dhont)) - (seats)]
  
  pultimo <- which(dhont == limit, arr.ind=T)
  pultimo <- pultimo[, 1]
  pultimo <- df_dhont$Party[pultimo][1]
  ### compute seats ###
  esc <- dhont >= limit
  esc <- 1*esc
  esc <- rowSums(esc)
  ### set last and difference ##
  df_dhont <- df_dhont %>% 
    mutate(
      esc = esc,
      div = Votes/(esc+1),
      limit_ = ifelse(Party == pultimo, limit_pen, limit),
      dif = as.integer((((limit_-div)*(esc+1))+1))
  )  %>% 
    select(Party, dif)
  
  ### return df###
  barrera <- as.integer(((sum(df$Votes)+blanks)*0.05 )+1) 
  total_votes <- as.integer(sum(df$Votes)+blanks)
  
  if(barrier > 0){
    return_df <- df %>%
      select(Party, Votes) %>% 
      left_join(df_dhont, by = "Party") %>% 
      rowwise() %>% 
      mutate(dif = ifelse(is.na(dif) & Votes < barrera & barrera > limit, 
        find_votos_barrera(Votes, barrier, total_votes) - Votes, 
        ifelse(is.na(dif), 
          as.integer((((limit-Votes))+1)), dif)),
        dif_rel = round(dif/total_votes*100,1))  %>% 
      select(Party, dif, dif_rel) %>% 
      rename(Dif = dif, Dif_relative = dif_rel)
    
  } else {
    return_df <- df_dhont %>% 
      select(Party, dif) %>% 
      mutate(dif_rel = round(dif/total_votes*100, 1)) %>% 
      rename(Dif = dif, Dif_relative = dif_rel)
  }
  
  return(return_df)
}


## find votos necesario to overcome barrera

find_votos_barrera <- function(votos, barrera, total_votos){

  p <- votos/total_votos
  
while (p <= (barrera/100)) {
  
votos <- votos + 1
total_votos <- total_votos+1
p <- votos/total_votos

}

return(votos)  
  
}



##### seats dhont last insular
last_seat_dhont_ins <- function(data_ins, data_aut, party, votes, seats, blanks_ins = 0, blanks_aut =0, barrier_ins = 0, barrier_aut = 0){
  require(tidyverse)
  df <- data_ins[,c(party, votes)]
  colnames(df) <- c("Party", "Votes")
  
  df_aut_all <- data_aut[,c(party, votes)]
  colnames(df_aut_all) <- c("Party", "Votes")
  
  ### apply barrier ###
  if(barrier_ins > 0 | barrier_aut > 0){
    df_aut <- df_aut_all %>% 
      mutate(prc = Votes/(sum(Votes)+blanks_aut)*100,
             Barrier_aut = ifelse(prc > !! barrier_aut, "Y", "N")) %>% 
      select(Party, Barrier_aut)
    
    df_dhont <- df %>%
      mutate(prc = Votes/(sum(Votes)+blanks_ins)*100,
             Barrier_ins = ifelse(prc > !! barrier_ins, "Y", "N")) %>%
      left_join(df_aut, by ="Party") %>% 
      filter(Barrier_ins == "Y" | Barrier_aut == "Y") 
    
  } else {
    df_dhont <- df
  }
  ### prepare matrix for dhont###
  dhont <- matrix(nrow = nrow(df_dhont), ncol = seats)
  dhont[,1] <- as.matrix(df_dhont$Votes)
  for(i in 2:seats) {
    dhont[ , i] <- dhont[ , 1]/i
  }
  ### set cut point for seats ###
  limit <- sort(as.vector(dhont))[length(as.vector(dhont))-(seats-1)]
  primer_sin <- sort(as.vector(dhont), partial = length(as.vector(dhont))- (seats))[length(as.vector(dhont)) - (seats)]
  ### compute seats ###
  esc <- dhont >= limit
  esc <- 1*esc
  esc <- rowSums(esc)
  ### set last and difference ##
  pos_primer_sin <- which(dhont == primer_sin, arr.ind=T)
  vfalta <- as.integer((limit - primer_sin)*pos_primer_sin[, 2])[1]+1
  
  pultimo <- which(dhont == limit, arr.ind=T)
  pultimo <- pultimo[, 1]
  pultimo <- df_dhont$Party[pultimo][1]
  
  if(nrow(df_dhont) != nrow(df)){
    df_aut <- df_aut_all %>% 
      mutate(prc = Votes/(sum(Votes)+blanks_aut)*100,
             Barrier_aut = ifelse(prc > !! barrier_aut, "Y", "N")) %>% 
      rename(Votes_aut = Votes) %>% 
      select(Party, Votes_aut, Barrier_aut)
    
    total_votos <- sum(df$Votes)+blanks_ins
    total_votos_aut <- sum(df_aut_all$Votes)+blanks_aut
    
    df_barrera <-  df %>%
      mutate(prc = Votes/(sum(Votes)+blanks_ins)*100,
             Barrier_ins = ifelse(prc > !! barrier_ins, "Y", "N")) %>%
      left_join(df_aut, by ="Party") %>% 
      filter(Barrier_ins == "N" & Barrier_aut == "N") %>% 
      rowwise() %>% 
      mutate(vfalta_seat = limit - Votes,
             vfalta_barrera_ins = find_votos_barrera(votos = Votes, barrera = barrier_ins, total_votos = total_votos) - Votes,
             vfalta_barrera_aut = find_votos_barrera(votos = Votes_aut, barrera = barrier_aut, total_votos = total_votos_aut) - Votes_aut,
             next_seat = ifelse(vfalta_seat < vfalta & (vfalta_barrera_aut < vfalta |  vfalta_barrera_ins < vfalta) & Votes == max(Votes), 1, 0))
    
    if(sum(df_barrera$next_seat, na.rm = T) == 1) {
      
      pprimer_sin <- df_barrera$Party[which(df_barrera$next_seat == 1)]
      min_barrera <- min(df_barrera$vfalta_barrera_aut[which(df_barrera$next_seat == 1)], df_barrera$vfalta_barrera_ins[which(df_barrera$next_seat == 1)])
      vfalta <- max(df_barrera$vfalta_seat[which(df_barrera$next_seat == 1)], min_barrera)
      
    } else {
      
      pprimer_sin <- which(dhont == primer_sin, arr.ind=T)
      pprimer_sin <- pprimer_sin[, 1]
      pprimer_sin <- df_dhont$Party[pprimer_sin][1]
      
    }
    
  } else {
    
    pprimer_sin <- which(dhont == primer_sin, arr.ind=T)
    pprimer_sin <- pprimer_sin[, 1]
    pprimer_sin <- df_dhont$Party[pprimer_sin][1]
  }
  ### return df###
  return_df <- data.frame(ultimo_esc = pultimo, primer_sin_esc = pprimer_sin,
                          diferencia_votos = as.integer(vfalta), diferencia_rel_candid = round(vfalta/(sum(df$Votes)+blanks_ins)*100, 1),
                          total_esc = seats, stringsAsFactors = F)
  return(return_df)
}


##### seats dhont last per party ins
next_seat_dhont_party_ins <- function(data_ins, data_aut, party, votes, seats, blanks_ins = 0, blanks_aut =0, barrier_ins = 0, barrier_aut = 0){
  require(tidyverse)
  df <- data_ins[,c(party, votes)]
  colnames(df) <- c("Party", "Votes")
  
  df_aut_all <- data_aut[,c(party, votes)]
  colnames(df_aut_all) <- c("Party", "Votes")
  
  ### apply barrier ###
  if(barrier_ins > 0 | barrier_aut > 0){
    df_aut <- df_aut_all %>% 
      mutate(prc = Votes/(sum(Votes)+blanks_aut)*100,
             Barrier_aut = ifelse(prc > !! barrier_aut, "Y", "N")) %>% 
      select(Party, Barrier_aut)
    
    df_dhont <- df %>%
      mutate(prc = Votes/(sum(Votes)+blanks_ins)*100,
             Barrier_ins = ifelse(prc > !! barrier_ins, "Y", "N")) %>%
      left_join(df_aut, by ="Party") %>% 
      filter(Barrier_ins == "Y" | Barrier_aut == "Y") 
    
  } else {
    df_dhont <- df
  }
  ### prepare matrix for dhont###
  dhont <- matrix(nrow = nrow(df_dhont), ncol = seats)
  dhont[,1] <- as.matrix(df_dhont$Votes)
  for(i in 2:seats) {
    dhont[ , i] <- dhont[ , 1]/i
  }
  ### set cut point for seats ###
  limit <- sort(as.vector(dhont))[length(as.vector(dhont))-(seats-1)]
  limit_pen <- sort(as.vector(dhont))[length(as.vector(dhont))-(seats-2)]
  primer_sin <- sort(as.vector(dhont), partial = length(as.vector(dhont))- (seats))[length(as.vector(dhont)) - (seats)]
  
  pultimo <- which(dhont == limit, arr.ind=T)
  pultimo <- pultimo[, 1]
  pultimo <- df_dhont$Party[pultimo][1]
  ### compute seats ###
  esc <- dhont >= limit
  esc <- 1*esc
  esc <- rowSums(esc)
  ### set last and difference ##
  df_dhont <- df_dhont %>% 
    mutate(
      esc = esc,
      div = Votes/(esc+1),
      limit_ = ifelse(Party == pultimo, limit_pen, limit),
      dif = as.integer((((limit_-div)*(esc+1))+1))
    )  %>% 
    select(Party, dif)
  
  ### return df###

  if(nrow(df_dhont) != nrow(df)){
    barrera_ins <- as.integer(((sum(df$Votes)+blanks_ins)*(barrier_ins/100 ))+1)
    barrera_aut <- as.integer(((sum(df_aut_all$Votes)+blanks_aut)*(barrier_aut/100))+1) 
    total_votes_ins <- as.integer(sum(df$Votes)+blanks_ins)
    total_votes_aut <- as.integer(sum(df_aut_all$Votes)+blanks_aut)
    
    
    df_aut_all <- df_aut_all %>% 
      rename(Votes_aut = Votes)
    
    return_df <- df %>%
      select(Party, Votes) %>% 
      left_join(df_dhont, by = "Party") %>% 
      left_join(df_aut_all, by = "Party") %>% 
      rowwise() %>% 
      mutate(vfalta_barrera_aut = find_votos_barrera(Votes_aut, barrier_aut, total_votes_aut) - Votes_aut,
             vfalta_barrera_ins = find_votos_barrera(Votes, barrier_ins, total_votes_ins) -  Votes,
             vfalta_esc = as.integer((limit-Votes)+1),
             dif = ifelse(is.na(dif), max(min(vfalta_barrera_ins, vfalta_barrera_aut), vfalta_esc), dif),
             dif_rel = round(dif/total_votes_ins*100,1))  %>% 
      select(Party, dif, dif_rel, vfalta_barrera_aut, vfalta_barrera_ins) %>% 
      rename(Dif = dif, Dif_relative = dif_rel)
    
  } else {
    return_df <- df_dhont %>% 
      select(Party, dif) %>% 
      mutate(dif_rel = round(dif/total_votes*100, 1),
             vfalta_barrera_aut = 0,
             vfalta_barrera_ins = 0) %>% 
      rename(Dif = dif, Dif_relative = dif_rel)
  }
  
  return(return_df)
}







