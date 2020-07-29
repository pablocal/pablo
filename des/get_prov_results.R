### function to get local results from official page ####

get_prov_results <- function(path, restart = FALSE){
  
  library(tidyverse)
  library(curl)
  
# 1. get escrutinio files -------------------------------------------------------------

  
# in the handle we can specify options available to the underlying libcurl system library.
# ?curl::curl_options() -> display all options
h <- curl::new_handle()
curl::handle_setopt(h, userpwd = "usuario533:re7ohV9e")
escrutinio <- read_rds("data/escrutinio.RDS")

# ?curl::curl_download()
hora <- Sys.time()
dest_file <- paste0("data/resultados/RESULTADOS_PROV_", str_replace_all(str_sub(hora, 12, 22), "\\:", "_"), ".gz")
curl::curl_download(path,
  destfile = dest_file, handle = h)

# check file is new
new_file_id <- hora

# 2. Extract data ---------------------------------------------------------
col_names <- c("ID", "CAUT", "CPROV", "DEL_", "NAME", "mesas", "censo", "censo_escrut", "p_censo_escrut", "votos_total", "votos_total_p",  "abst", "p_abst",  "blancos", "p_blancos",  "nulos", "p_nulos", "esc_total")

base <- c("CPARTIDO", "partido", "votos", "p_votos", "esc")
tabla <- c("CPARTIDO1", "partido1", "votos1", "p_votos1", "esc1")
for(i in 2:80){
  to_add <-  paste0(base, i)
  tabla <- c(tabla, to_add)
}

col_names <- c(col_names, tabla, "DEL_")

data <- read.csv2(gzfile(dest_file), header = F, col.names = col_names,  stringsAsFactors = F) 

# 3. Data esc -------------------------------------------------------------
escrutinio_add <- data %>% 
  select(-starts_with("DEL_"), -starts_with("p_votos"), -starts_with("CPARTIDO"))

votos <- escrutinio_add %>% 
  select(starts_with("votos")) %>% 
  gather(var, votos, votos1:votos80) %>% 
  select(votos)

esc <- escrutinio_add %>% 
  select(starts_with("esc"), -esc_total) %>% 
  gather(var, esc, esc1:esc80) %>% 
  select(esc)

escrutinio_add <- escrutinio_add %>% 
  select(ID:esc_total, starts_with("partido")) %>% 
  gather(var, partido, partido1:partido80) %>% 
  select(-var) %>% 
  bind_cols(votos) %>% 
  bind_cols(esc) %>% 
  filter(str_sub(partido, 1, 1) != "0") %>% 
  mutate(file_id = hora,
         validos = votos_total-nulos) %>% 
  filter(ID == "CI") %>% 
  arrange(CAUT, CPROV, -votos) 

escrutinio <- bind_rows(escrutinio_add, escrutinio)

if(restart == TRUE){
  escrutinio <- escrutinio_add[0,]
}

## save new file
write_rds(escrutinio, "data/escrutinio.RDS")

## output
return(escrutinio)

}



