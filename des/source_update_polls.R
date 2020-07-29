# Metadata ----------------------------------------------------------------
# Title: Update polls
# Purpose: Scrape electocracia to get data from last polls
# Author(s): @pablocal
# Date Created: 2019-09-30
#
# Comments ----------------------------------------------------------------
# 
# 
# 
#
#
# Options and packages ----------------------------------------------------


# 1. Update polls overall function ----------------------------------------

update_polls <- function(save = TRUE, token_path = "source/token.RDS"){

  token <- readRDS(token_path)
  
  new_data <- retrieve_data()
  return_df <- join_data(new_data = new_data, token = token)
  
  if(isTRUE(save)){
    save_acum(return_df, token = token)
  }
  
  return(return_df)

}



# 2. Retrieve data --------------------------------------------------------

retrieve_data <- function(){

library(rvest)
library(tidyverse)

url <- xml2::read_html("https://electocracia.com/")
  
medio <- url %>%
  html_nodes(".column-1") %>%
  html_text()

pollster <- url %>%
  html_nodes(".column-2") %>%
  html_text()

fpub <- url %>%
  html_nodes(".column-3") %>%
  html_text()

fincampo <- url %>%
  html_nodes(".column-4") %>%
  html_text()

muestra <- url %>%
  html_nodes(".column-5") %>%
  html_text() %>% 
  as.integer()

psoe <- url %>%
  html_nodes(".column-6") %>%
  html_text() %>% 
  str_replace_all(",", ".") %>% 
  as.numeric()

pp <- url %>%
  html_nodes(".column-7") %>%
  html_text() %>% 
  str_replace_all(",", ".") %>% 
  as.numeric()

cs <- url %>%
  html_nodes(".column-8") %>%
  html_text() %>% 
  str_replace_all(",", ".") %>% 
  as.numeric()

podemos <- url %>%
  html_nodes(".column-9") %>%
  html_text() %>% 
  str_replace_all(",", ".") %>% 
  as.numeric()

vox <- url %>%
  html_nodes(".column-10") %>%
  html_text() %>% 
  str_replace_all(",", ".") %>% 
  as.numeric()

mp <- url %>%
  html_nodes(".column-11") %>%
  html_text() %>% 
  str_replace_all(",", ".") %>% 
  as.numeric()

return_df <- tibble(fpub = parse_date(fpub[2:length(fpub)], "%d/%m/%Y"),
       medio = medio[2:length(medio)],
       pollster = pollster[2:length(pollster)],
       fincampo = parse_date(fincampo[2:length(fincampo)], "%d/%m/%Y"),
       muestra = muestra[2:length(muestra)],
       pp = pp[2:length(pp)],
       podemos = podemos[2:length(podemos)],
       psoe = psoe[2:length(psoe)],
       cs = cs[2:length(cs)],
       vox = vox[2:length(vox)],
       mp = mp[2:length(mp)])

return(return_df)

}


# 3. Join data from source ------------------------------------------------
join_data <- function(new_data, token){

library(googlesheets)
library(lubridate)

# get cumulative file
gs_auth(token = token)  

gsheet <- gs_title("Acumulador de encuestas")
acum <- gs_read(gsheet)

# set the date variables
# acum <- acum %>% 
#   mutate(inicampo = parse_date(inicampo, "%Y-%m-%d"),
#          fincampo = parse_date(fincampo, "%Y-%m-%d"),
#          fpub = parse_date(fpub, "%Y-%m-%d")
#          ) 

# select last poll(s)
last_polls <- acum %>% 
  filter(fpub == max(fpub))

new_data <- new_data %>% 
  filter(fpub >= max(last_polls$fpub)) %>%
  
  filter(!(fpub == max(last_polls$fpub) & pollster %in% unique(last_polls$pollster)))

print(new_data)

# add new data together
return_df <- bind_rows(acum, new_data) %>% 
  mutate(
  pollster = str_trim(pollster, side = "right"),
  pollster = recode(pollster,
                    "Instituto DYM" = "DYM",
                    "IMOP" = "IMOP Insights",
                    "IMOP insights" = "IMOP Insights",
                    "Imop insights" = "IMOP Insights",
                    "MyWord" = "40dB",
                    "Celeste-Tel" = "Celeste Tel",
                    "SocioMétrica" = "Sociométrica",
                    "Nc Report" = "NC Report"
  ),
  medio = str_trim(medio, side = "right"),
  medio = recode(medio,
                    "Eldiario.es" = "eldiario.es"
  ),
  modo = case_when(
    !is.na(modo) ~ modo,
    is.na(modo) & pollster == "CIS" ~ "Personal",
    is.na(modo) & pollster == "40dB" ~ "Online",
    TRUE ~ "Telefónica"
  )) %>% 
    filter(!(pollster %in% c("Sondaxe", "Top Position", "Demoscopia y Servicios", "Ipsos"))) %>% 
  arrange(desc(fpub))

return(return_df)

}


# 4. Save final data ------------------------------------------------------

save_acum <- function(acum, token){

library(googlesheets)
  
write_csv(acum, "temp/acum.csv")

gs_auth(token)  
gs_upload("temp/acum.csv", sheet_title = "Acumulador de encuestas" , overwrite = TRUE)  

}

