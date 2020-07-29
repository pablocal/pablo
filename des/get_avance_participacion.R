### function to get avance participacion from official page ####



get_avance_participacion <- function(test = FALSE, restart = FALSE){
  
  library(tidyverse)
  library(curl)
  
# 1. get escrutinio files -------------------------------------------------------------

if(test == FALSE){
# in the handle we can specify options available to the underlying libcurl system library.
# ?curl::curl_options() -> display all options
h <- curl::new_handle()
curl::handle_setopt(h, userpwd = "usuario33:Yufeu4oh")

# ?curl::curl_download()
hora <- Sys.time()
dest_file <- paste0("data/resultados/AVANCE_", str_replace_all(str_sub(hora, 12, 22), "\\:", "_"), ".zip")
curl::curl_download("https://medios.eleccioneslocaleseuropeas19.es/dataLocales/AVANCES.zip",
  destfile = dest_file, handle = h)

zip::unzip(zipfile =  dest_file, exdir = "shiny/temp")
} 

# 2. get files for preparation  ----------------------------------------------------
mun_lookup <- read_rds("shiny/data/lookup_selector.RDS")
files <- list.files("shiny/temp")

# check file is new
new_file_id <- str_sub(files[1], str_length(files[1]) - 9, str_length(files[1]) - 4)
new_file_date <- str_sub(files[1], str_length(files[1]) - 18, str_length(files[1]) - 11)
new_date <- as.Date.character(new_file_date, format = "%Y%m%d")
new_time <- paste0(str_sub(new_file_id, 1, 2), ":", str_sub(new_file_id, 3, 4), ":", str_sub(new_file_id, 5, 6))
new_time <- hms::as.hms(new_time)

## prepare files
col_names <- c("CAUT", "CPROV", "DEL_", "CMUN", "CDISTMUN", "MUN", "DEL_", "DEL_", "mesas_totales", 
               "mesas_const", "mesas_suspendidas", "censo", "censo_const", "censo_susp", 
               "avance1_mesas", "avance1_censo", "avance1_part",
               "avance2_mesas", "avance2_censo", "avance2_part")


file_tenerife <- paste0("shiny/temp/", files[!is.na(str_locate(files, "_38_")[,1])])
file_palmas <- paste0("shiny/temp/", files[!is.na(str_locate(files, "_35_")[,1])])
pal <- read.csv2(file_palmas, header = F, col.names = col_names, stringsAsFactors = F) 
ten <- read.csv2(file_tenerife, header = F, col.names = col_names, stringsAsFactors = F) 
# pal <- read_csv2(file_palmas, col_names = col_names, col_types = cols(.default = "d")) 
# ten <-read_csv2(file_tenerife, col_names = col_names, col_types = cols(.default = "d")) 

avance <- bind_rows(pal, ten) %>% 
  select(-starts_with("DEL_"),  -MUN) %>% 
  left_join(mun_lookup, by = c("CPROV", "CMUN")) %>% 
  mutate(file_id = new_file_id,
         fecha = new_date,
         hora = new_time) %>% 
  select(file_id, fecha, hora, CPROV, PROV, CISLA, ISLA, CMUN, MUN, mesas_totales:avance2_part) %>% 
  arrange(CPROV, CMUN)


if(restart == TRUE){
  avance <- avance[0,]
}

## save new file
write_rds(avance, "shiny/data/avances_participacion.RDS")
write_csv(avance, "shiny/data/avances_participacion.csv")

## join with other participacion file
avance_to_join <- avance %>% 
  select(CPROV, CMUN, censo, avance1_part, avance2_part) %>% 
  rowwise() %>% 
  mutate(year = 2019,
         avance1_part = as.integer(ifelse(avance1_part > 100, (avance1_part/10000)*censo, avance1_part*censo)),
         avance2_part = as.integer(ifelse(avance2_part > 100, (avance2_part/10000)*censo, avance2_part*censo)))
        

if(restart == TRUE){
  avance_to_join <- avance_to_join[0,]
}
## save new file
write_rds(avance_to_join, "shiny/data/avance_to_join.RDS")
write_csv(avance_to_join, "shiny/data/avance_to_join.csv")

## upload to gdrive
write <- googlesheets::gs_upload("shiny/data/avance_to_join.csv", "avance_to_join", overwrite = T)

## clean temp dir
do.call(file.remove, list(list.files("shiny/temp", full.names = TRUE)))

## output
return(avance_to_join)

}



