### plot_part_global ##



plot_part_global <- function(data, avance, input_PROV, input_ISLA, input_MUN){
  
  library(tidyverse)
  
  gg_theme <- theme(
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "gray96"),
    strip.text = element_text(color = "black", hjust = .2, face = "bold"),
    plot.caption = element_text(color = "grey40", size = 7),
    plot.subtitle = element_text(color = "grey40"))
  
  
  
  if(input_PROV == "Total"){
# canarias
data <- data %>%
  select(year, starts_with("part_"), CensusCount) %>% 
  group_by(year) %>% 
  summarise_all(sum, na.rm = T) %>% 
  mutate(p_part_avance1 = part_avance1/CensusCount*100,
    p_part_avance2 = (part_avance2/CensusCount*100) - p_part_avance1,
    p_part_final = (part_final/CensusCount*100) - (p_part_avance1 + p_part_avance2),
    year = as.character(year)) %>% 
  select(year, starts_with("p_part_")) %>% 
  gather(var, part, p_part_avance1:p_part_final)

data$var <- as.factor(data$var)
data$var <- with(data, factor(data$var, levels = rev(levels(data$var))))

data_line <- data %>% 
  filter(year == 2019)

part1 <- data_line$part[1]
part2 <- data_line$part[1] + data_line$part[2]



  } else if(input_PROV != "Total" & input_ISLA == "") {

## por provincia
    data <- data %>%
      filter(PROV == input_PROV) %>% 
      select(year, starts_with("part_"), CensusCount) %>% 
      group_by(year) %>% 
      summarise_all(sum, na.rm = T) %>% 
      mutate(p_part_avance1 = part_avance1/CensusCount*100,
        p_part_avance2 = (part_avance2/CensusCount*100) - p_part_avance1,
        p_part_final = (part_final/CensusCount*100) - (p_part_avance1 + p_part_avance2),
        year = as.character(year)) %>% 
      select(year, starts_with("p_part_")) %>% 
      gather(var, part, p_part_avance1:p_part_final)
    
    data$var <- as.factor(data$var)
    data$var <- with(data, factor(data$var, levels = rev(levels(data$var))))
    
    data_line <- data %>% 
      filter(year == 2019)
    
    part1 <- data_line$part[1]
    part2 <- data_line$part[1] + data_line$part[2]


  } else if(input_PROV != "Total" & input_ISLA != "" & input_MUN == "") {
    
    data <- data %>%
      filter(ISLA == input_ISLA) %>% 
      select(year, starts_with("part_"), CensusCount) %>% 
      group_by(year) %>% 
      summarise_all(sum, na.rm = T) %>% 
      mutate(p_part_avance1 = part_avance1/CensusCount*100,
        p_part_avance2 = (part_avance2/CensusCount*100) - p_part_avance1,
        p_part_final = (part_final/CensusCount*100) - (p_part_avance1 + p_part_avance2),
        year = as.character(year)) %>% 
      select(year, starts_with("p_part_")) %>% 
      gather(var, part, p_part_avance1:p_part_final)
    
    data$var <- as.factor(data$var)
    data$var <- with(data, factor(data$var, levels = rev(levels(data$var))))
    
    data_line <- data %>% 
      filter(year == 2019)
    
    part1 <- data_line$part[1]
    part2 <- data_line$part[1] + data_line$part[2]

    
  } else {
   
    
    data <- data %>%
      filter(PROV == input_PROV, MUN == input_MUN) %>% 
      select(year, starts_with("part_"), CensusCount) %>% 
      group_by(year) %>% 
      summarise_all(sum, na.rm = T) %>% 
      mutate(p_part_avance1 = part_avance1/CensusCount*100,
        p_part_avance2 = (part_avance2/CensusCount*100) - p_part_avance1,
        p_part_final = (part_final/CensusCount*100) - (p_part_avance1 + p_part_avance2),
        year = as.character(year)) %>% 
      select(year, starts_with("p_part_")) %>% 
      gather(var, part, p_part_avance1:p_part_final)
    
    data$var <- as.factor(data$var)
    data$var <- with(data, factor(data$var, levels = rev(levels(data$var))))
    
    data_line <- data %>% 
      filter(year == 2019)
    
    part1 <- data_line$part[1]
    part2 <- data_line$part[1] + data_line$part[2] 
    
  }
  
  if(avance == "avance1") {
    
    ggplot(filter(data, year != 2019), aes(x = year, y = part, fill = var)) + 
      geom_col() + 
      geom_hline(aes(yintercept = part1), col = "blue", size = 1) +
      scale_fill_brewer(palette = "Reds") +
      labs(
        x = "",
        y = "") +
      gg_theme
    
  } else {
    
    ggplot(filter(data, year != 2019), aes(x = year, y = part, fill = var)) + 
      geom_col() + 
      geom_hline(aes(yintercept = part2), col = "blue", size = 1) +
      scale_fill_brewer(palette = "Reds") +
      labs(
        x = "",
        y = "% Participación") +
      gg_theme
  }
  
}




plot_part_mun <- function(data, avance, input_year, input_partido, input_PROV, input_ISLA, input_MUN){
  
  library(tidyverse)
  
  gg_theme <- theme(
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "gray96"),
    strip.text = element_text(color = "black", hjust = .2, face = "bold"),
    plot.caption = element_text(color = "grey40", size = 7),
    plot.subtitle = element_text(color = "grey40"))
  
  
if(input_PROV == "Total"){
    # canarias
  
  data <- data %>% 
    filter(year == input_year)
  
  hline1 <- (sum(data$part_avance1, na.rm = T)/sum(data$CensusCount, na.rm = T)*100) - (sum(data$prev_part_avance1, na.rm = T)/sum(data$prev_CensusCount, na.rm = T)*100) 
  hline2 <- (sum(data$part_avance2, na.rm = T)/sum(data$CensusCount, na.rm = T)*100) - (sum(data$prev_part_avance2, na.rm = T)/sum(data$prev_CensusCount, na.rm = T)*100) 
  
  } else if(input_PROV != "Total" & input_ISLA == "") {
    
    ## por provincia
    data <- data %>%
      filter(year == input_year, PROV == input_PROV) 
    
    hline1 <- (sum(data$part_avance1, na.rm = T)/sum(data$CensusCount, na.rm = T)*100) - (sum(data$prev_part_avance1, na.rm = T)/sum(data$prev_CensusCount, na.rm = T)*100) 
    hline2 <- (sum(data$part_avance2, na.rm = T)/sum(data$CensusCount, na.rm = T)*100) - (sum(data$prev_part_avance2, na.rm = T)/sum(data$prev_CensusCount, na.rm = T)*100) 
    
  } else if(input_PROV != "Total" & input_ISLA != "") {
    
    data <- data %>%
      filter(year == input_year, ISLA == input_ISLA) 
    
    hline1 <- (sum(data$part_avance1, na.rm = T)/sum(data$CensusCount, na.rm = T)*100) - (sum(data$prev_part_avance1, na.rm = T)/sum(data$prev_CensusCount, na.rm = T)*100) 
    hline2 <- (sum(data$part_avance2, na.rm = T)/sum(data$CensusCount, na.rm = T)*100) - (sum(data$prev_part_avance2, na.rm = T)/sum(data$prev_CensusCount, na.rm = T)*100) 
    
  }
  
  
  input_partido <- paste0("p_", input_partido)
  
if(avance == "avance1") {
    
    ggplot(data, aes_string(x = input_partido, y = "dif_part_avance1", size = "CensusCount")) +
      geom_point(shape = 1, alpha = .2, col = "black") +
      geom_smooth(method = lm, se = F) +
      geom_hline(aes(yintercept = hline1), col = "red") +
      scale_y_continuous(lim = c(-20, 20)) +
      labs(
        x = "% de votos en elecciones anteriores",
        y = "% Participación - % Participación anteriores") +
      gg_theme +
      theme(legend.position = "none")
    
    
  } else {
    
    ggplot(data, aes_string(x = input_partido, y = "dif_part_avance2", size = "CensusCount")) +
      geom_point(shape = 1, alpha = .2, col = "black") +
      geom_smooth(method = lm, se = F) +
      geom_hline(aes(yintercept = hline2), col = "red") +
      scale_y_continuous(lim = c(-20, 20)) +
      labs(
        x = "% de votos en elecciones anteriores",
        y = "% Participación - % Participación anteriores") +
      gg_theme +
      theme(legend.position = "none")
    
  }
  
 
}
