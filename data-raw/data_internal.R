# Metadata ----------------------------------------------------------------
# Title: data_internal.R
# Purpose: Create internal data
# Author(s): @pablocal
# Date: 2020-06-05 15:42:54
#
# Comments ----------------------------------------------------------------
#
#
#
#
#
# Options and packages ----------------------------------------------------
library(tidyverse)
library(DBI)

# 1. Generate election id data --------------------------------------------

con <- dbConnect(odbc::odbc(),
                 server = "basilio.cvonxmf06tq5.eu-west-3.rds.amazonaws.com",
                 user = "elections",
                 password = "elections",
                 database = "store",
                 port = 3306,
                 timeout = 10,
                 .connection_string = "Driver={MySQL};")


elec_id_checks <- dbGetQuery(con, "SELECT elec_id, year, month FROM store.mir_elec_id;")
dbDisconnect(con)


# 2. Generate df with codes -----------------------------------------------

con <- dbConnect(odbc::odbc(),
                 server = "basilio.cvonxmf06tq5.eu-west-3.rds.amazonaws.com",
                 user = "elections",
                 password = "elections",
                 database = "codes",
                 port = 3306,
                 timeout = 10,
                 .connection_string = "Driver={MySQL};")


codes_ccaa <- dbGetQuery(con, "SELECT * FROM codes.ine_ccaa;") %>%
  rename(Code = caut, Area = autonomia) %>%
  mutate(Level = "Comunidad")
codes_prov <- dbGetQuery(con, "SELECT * FROM codes.ine_prov;") %>%
  rename(Area = provincia) %>%
  mutate(Level = "Provincia")
codes_isla <- dbGetQuery(con, "SELECT * FROM codes.ine_isla;") %>%
  rename(Code = cisla, Area = isla) %>%
  mutate(Level = "Isla")
codes_mun <- dbGetQuery(con, "SELECT * FROM codes.ine_mun;") %>%
  mutate(Code = paste0(caut, cprov, cmun)) %>%
  rename(Area = municipio) %>%
  mutate(Level = "Municipio")

caut_cprov <- codes_mun %>%
  group_by(cprov) %>%
  summarise(caut = first(caut)) %>%
  ungroup()

codes_isla <- codes_isla %>%
  left_join(caut_cprov, by = "cprov") %>%
  mutate(Code = paste0(caut, Code)) %>%
  select(-caut, -cprov, -cmun, -dc) %>%
  group_by(Code, Area) %>%
  summarise(Level = first(Level)) %>%
  ungroup()

codes_prov <- codes_prov %>%
  left_join(caut_cprov, by = "cprov") %>%
  mutate(Code = paste0(caut, cprov)) %>%
  select(-cprov, -caut)

codes_mun <- codes_mun %>%
  select(-caut, -cprov, -cmun, -dc)

ine_codes <- bind_rows(codes_ccaa, codes_prov, codes_isla, codes_mun) %>%
  select(Level, Area, Code) %>%
  as_tibble()


# 3. Lookup files ---------------------------------------------------------

lkup_ine_mir <- dbGetQuery(con, "SELECT * FROM codes.lkup_ine_mir_ccaa;") %>%
  as_tibble()
dbDisconnect(con)


usethis::use_data(ine_codes, elec_id_checks, lkup_ine_mir, internal = T, overwrite = T)


