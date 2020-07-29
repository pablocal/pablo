# Metadata ----------------------------------------------------------------
# Title: elections
# Purpose: Set of functions to retrieve election results
# Author(s): @pabmunicipio
# Date: 2020-06-04 11:13:57
#
# Comments ----------------------------------------------------------------



# 1. Arrange queries -----------------------------------------------------


# 1.1 Check inputs --------------------------------------------------------
check_input_output <- function(output){
  if(!all(output %in% c("all", "summary"))) stop("output must be 'summary' or 'all'")
}

check_input_by <- function(by){
  if(!is.null(by)){

    if(!(by %in% c("comunidad", "provincia", "municipio", "distrito", "seccion", "mesa"))) stop("by must be one of 'comunidad', 'provincia', 'municipio', 'distrito', 'seccion', 'mesa'")
    if(length(by) != 1) stop("by must be of length 1")
  }
}

check_input_ine_geo_code <- function(ine_geo_code){
  if(length(ine_geo_code) != 1 & !is.null(ine_geo_code)) stop("ine_geo_code must be of length 1")
  if(!is.null(ine_geo_code) & !is.character(ine_geo_code)) stop("ine_geo_code must a character vector of length 1")
}

check_input_month_year <- function(elec, month, year){
  if(length(year) != 1) stop("year must be of length 1")
  if(length(month) != 1 & !is.null(month)) stop("month must be of length 1")

  if(elec %in% c("congreso", "senado") & year == 2019 & is.null(month)) stop("for 2019 'congreso' or 'senado' elections provide a month: 4 (April) or 11 (November)")
  if(!(year %in% unique(pablo:::elec_id_checks$year))) stop("There was no election of this type in the input year or data is not available")
  }

check_input_elec <- function(elec){
  if(length(elec) > 1) stop("elec must be length 1")
  if(elec != "congreso" & elec != "senado" &
     elec != "locales" & elec != "referendum" &
     elec != "europeas" & elec != "cabildo") stop("elec is not correct select: 'congreso', 'senado', 'locales', 'referendum', 'europeas', 'cabildo'")
}

check_cross_month_year <- function(elec, year, month){

  year_month <- pablo:::elec_id_checks[pablo:::elec_id_checks$year == year & pablo:::elec_id_checks$elec_id == elec, ]
  elec_months <- year_month$month

  if(nrow(year_month) == 0)stop("There was no election of this type for the input year or data is not available")
  if(!(month %in% elec_months))stop("There was no election of the type for the input month or data is not avalable")

}

check_cross_by_geocode <- function(by, ine_geo_code_list){

  if(is.null(by) & !(is.na(ine_geo_code_list$caut) & is.na(ine_geo_code_list$cprov) & is.na(ine_geo_code_list$cmun) &
                             is.na(ine_geo_code_list$cdist) & is.na(ine_geo_code_list$csec) & is.na(ine_geo_code_list$cmesa))) stop("Check by and/or ine_geo_code: nacional results are only provided for the whole country")
  if(by == "comunidad" & !(is.na(ine_geo_code_list$cprov) & is.na(ine_geo_code_list$cmun) & is.na(ine_geo_code_list$cdist)
                             & is.na(ine_geo_code_list$csec) & is.na(ine_geo_code_list$cmesa))) stop("Check by and/or ine_geo_code: comunidad results cannot be shown for this area")
  if(by == "provincia" & !(is.na(ine_geo_code_list$cmun) & is.na(ine_geo_code_list$cdist) & is.na(ine_geo_code_list$csec) & is.na(ine_geo_code_list$cmesa))) stop("Check by and/or ine_geo_code: provincia results cannot be shown for this area")
  if(by == "municipio" & !(is.na(ine_geo_code_list$cdist) & is.na(ine_geo_code_list$csec) & is.na(ine_geo_code_list$cmesa))) stop("Check by and/or ine_geo_code: municipio results results cannot be shown for this area")
  if(by == "distrito" & !(is.na(ine_geo_code_list$csec) & is.na(ine_geo_code_list$cmesa))) stop("Check by and/or ine_geo_code: distrito results results cannot be shown for this area")
  if(by == "seccion" & !(is.na(ine_geo_code_list$cmesa))) stop("Check by and/or ine_geo_code: seccion results results cannot be shown for this area")

  }

# 1.2 Support functions for arrange_queries --------------------------------

impute_month <- function(elec, year){

  year_month <- pablo:::elec_id_checks[pablo:::elec_id_checks$year == year & pablo:::elec_id_checks$elec_id == elec, ]
  month <- year_month$month[1]
  return(month)

}

decompose_ine_geo_code <- function(ine_geo_code){

  if(is.null(ine_geo_code)){ine_geo_code <- NA}

    caut_ine <- substr(ine_geo_code, 1, 2)
    caut_mir <- pablo:::lkup_ine_mir$caut_mir[pablo:::lkup_ine_mir$caut_ine == caut_ine][1]

  return_df <- data.frame(
    caut = caut_mir,
    cprov = substr(ine_geo_code, 3, 4),
    cmun = substr(ine_geo_code, 5, 7),
    cdist = substr(ine_geo_code, 8, 9),
    csec = substr(ine_geo_code, 10, 12),
    cmesa = substr(ine_geo_code, 13, 14)
    )

  return_df[return_df == ""] <- NA

  return(return_df)
}

impute_by <- function(elec, by, ine_geo_code){

   if(elec == 3 & is.null(by)) {

    by <- "provincia"
    warning("senado results by provincia")

  } else if(elec == 3 & !is.null(by)) {

    if(elec == 3 & by == "comunidad"){
    by <- "provincia"
    warning("senado results by provincia")
    }

  } else if(is.null(by) & !is.null(ine_geo_code)){

    switch(as.character(nchar(ine_geo_code)),
           "2" = by <- "comunidad",
           "4" = by <- "provincia",
           "7" = by <- "municipio",
           "9" = by <- "distrito",
           "12" = by <- "seccion",
           "13" = by <- "mesa")

  } else if (is.null(by) & is.null(ine_geo_code)) {

    by <-  "nacional"

  }

  return(by)

}

# 1.3 Arrange queries --  ----------------------------------------------------
arrange_queries <- function(elec,
                            year,
                            month = NULL,
                            ine_geo_code = NULL,
                            by = NULL,
                            output = "summary"){

  ## check inputs
  check_input_output(output)
  check_input_by(by)
  check_input_ine_geo_code(ine_geo_code)
  check_input_month_year(elec, month, year)
  check_input_elec(elec)

  ## transform elec into code
  elec_type <- elec
  switch (elec,
          "congreso" = elec <- 2,
          "senado" = elec <- 3,
          "locales" = elec <- 4,
          "referendum" = elec <- 1,
          "europeas" = elec <- 7,
          "cabildo" = elec <- 6,
          "autonmicas" = elec <- 5
  )

  ## impute month if it's null
  if(is.null(month)) month <- impute_month(elec, year)

  ## check cross year-month
  check_cross_month_year(elec, year, month)
  by <- impute_by(elec, by, ine_geo_code)

  ## decompose ine_geo_code
  ine_geo_code_list <- decompose_ine_geo_code(ine_geo_code)

  ## check cross ine_geo_code-by
  check_cross_by_geocode(by, ine_geo_code_list)

  ## set data to extract
  data_to_extract <- data.frame(elec = elec,
                                elec_type = elec_type,
                                year = year,
                                month = month,
                                caut = ifelse(by == "nacional", 99, ine_geo_code_list$caut),
                                cprov = ine_geo_code_list$cprov,
                                cmun = ine_geo_code_list$cmun,
                                cdist = ine_geo_code_list$cdist,
                                csec = ine_geo_code_list$csec,
                                cmesa = ine_geo_code_list$cmesa,
                                by = by,
                                output = output,
                                data_orig = c("votes", "turn"),
                                stringsAsFactors = FALSE)


  ## finalise queries with table
  data_to_extract$table <- ifelse(data_to_extract$by %in% c("nacional", "comunidad", "provincia") & data_to_extract$data_orig == "turn", "store.mir_elec_upper_mun_turn",
                                      ifelse(data_to_extract$by %in% c("nacional", "comunidad", "provincia") & data_to_extract$data_orig == "votes", "store.mir_elec_upper_mun_votes",
                                             ifelse(data_to_extract$by == "municipio" & data_to_extract$data_orig == "turn", "store.mir_elec_mun_turn",
                                                    ifelse(data_to_extract$by == "municipio" & data_to_extract$data_orig == "votes", "store.mir_elec_mun_votes",
                                                           ifelse(data_to_extract$by %in% c("distrito", "seccion", "mesa") & data_to_extract$data_orig == "turn", "store.mir_elec_precint_turn",
                                                                  "store.mir_elec_precint_votes")))))


    return(data_to_extract)

}

# 2. Extract data ---------------------------------------------------------


# 2.1 Auxiliary functions for extrac data ---------------------------------
write_sql_query <- function(queries){

  if(queries$caut == 99 & queries$by == "nacional"){
    caut <- paste0('AND caut = "99"')
  } else if (!is.na(queries$caut) & queries$by == "comunidad") {
    caut <- paste0('AND caut = "', queries$caut, '" AND cprov = "99"')
  } else if (is.na(queries$caut) & queries$by == "comunidad") {
    caut <- paste0('AND caut = "99" AND cprov = "99"')
  } else if (!is.na(queries$caut)){
    caut <- paste0('AND caut = "', queries$caut, '"')
  } else {
    caut <- ""
  }

  if(is.na(queries$cprov) & queries$by == "provincia"){
    cprov <- paste0('AND cprov = "99" AND constituency_id =  9')
  } else if (!is.na(queries$cprov)) {
    cprov <- paste0('AND cprov = "', queries$cprov, '"')
  } else {
    cprov <- ""
  }

  if(is.na(queries$cmun)){
    cmun <- ""
  } else if(!is.na(queries$cmun) & queries$by == "municipio") {
    cmun <- paste0('AND cmun = "', queries$cmun, '"', ' AND dist_mun = 99')
  } else {
    cmun <- paste0('AND cmun = "', queries$cmun, '"')
  }

  if(is.na(queries$cdist)){
    cdist <- ""
  } else {
    cdist <- paste0('AND cdist = "', queries$cdist, '"')
  }

  if(is.na(queries$csec)){
    csec <- ""
  } else {
    csec <- paste0('AND csec = "', queries$csec, '"')
  }

  if(is.na(queries$cmesa)){
    cmesa <- ""
  } else {
    cmesa <- paste0('AND cmesa = "', queries$cmesa, '"')
  }


  query_where <- paste(caut, cprov, cmun, cdist, csec, cmesa)
  query_where <- trimws(query_where)


  query <- paste0("SELECT * FROM ",
                  queries$table,
                  " WHERE elec_id = ",
                  queries$elec,
                  " AND year = ",
                  queries$year,
                  " AND month = ",
                  queries$month,
                  " ",
                  query_where,
                  ";"
  )

  return(query)
}

list_sql_queries <- function(queries){

  sql_queries <- list(
    query_turn = write_sql_query(queries[queries$data_orig == "turn",]),
    query_votes = write_sql_query(queries[queries$data_orig == "votes",]),
    query_candidatures = paste0("SELECT candidature_acron, candidature_id, candidature_id_country FROM store.mir_elec_candidatures WHERE elec_id = ", queries$elec[1], " AND year = ", queries$year[1], " AND month = ", queries$month[1], ";")
  )

  if(queries$elec[1] == 3){
    sql_queries$query_candidates = paste0("SELECT constituency_id, cprov, cmun, candidature_id, candidate_name FROM store.mir_elec_candidates WHERE elec_id = ", queries$elec[1], " AND year = ", queries$year[1], " AND month = ", queries$month[1], ";")
  }

  return(sql_queries)
}



# 2.2 Extract data function -----------------------------------------------
extract_data <- function(queries){

  con <- dbConnect(odbc::odbc(),
                   server = "basilio.cvonxmf06tq5.eu-west-3.rds.amazonaws.com",
                   user = "elections",
                   password = "elections",
                   database = "store",
                   port = 3306,
                   timeout = 10,
                   .connection_string = "Driver={MySQL};")

  ## write queries
  sql_queries <- list_sql_queries(queries)

  ## extract data
  extract <- lapply(sql_queries, dbGetQuery, conn = con)
  dbDisconnect(con)
  if(nrow(extract$query_turn) == 0 | nrow(extract$query_votes) == 0 | nrow(extract$query_candidatures) == 0) stop("Empty query")

  return(extract)

}

# 3. Format data ----------------------------------------------------------


# 3.1 Auxiliary functions for format output -----------------------------------

clean_candidature_names <- function(elec, extract){

  if(elec == 3){
    extract$merge_votes_cand <- extract$merge_votes_cand %>%
      rowwise() %>%
      mutate(candidate_name = str_squish(candidate_name))
  }

  # generate list of nacional names for the parties
  party_names <- extract$query_candidatures %>%
    left_join(extract$query_votes, by = "candidature_id") %>%
    group_by(candidature_id_country, candidature_id, candidature_acron) %>%
    summarise(count = n(), votes = sum(votes_candidature)) %>%
    ungroup() %>%
    arrange(candidature_id_country, -votes) %>%
    group_by(candidature_id_country) %>%
    summarise(candidature_acron = first(candidature_acron)) %>%
    ungroup() %>%
    mutate(candidature_acron = str_trim(candidature_acron, side = "both"),
           candidature_acron = str_remove_all(candidature_acron, "\\,"),
           candidature_acron = str_remove_all(candidature_acron, "\\."),
           candidature_acron = str_remove_all(candidature_acron, "\\'"),
           candidature_acron = str_remove_all(candidature_acron, "\\´"),
           candidature_acron = str_remove_all(candidature_acron, "\\`"))

  extract$query_candidatures <- extract$query_candidatures %>%
    select(-candidature_acron) %>%
    left_join(party_names, by = "candidature_id_country") %>%
    select(-candidature_id_country)

  return(extract)
}


merge_votes_candidature <- function(elec, extract){

  if(elec == 3){
    ## create an equivalent candidate code to merge with votes
    extract$query_candidates <- rename(extract$query_candidates, candidature_id_party = candidature_id)
    extract$query_candidates <- mutate(extract$query_candidates, candidature_id = as.integer(cprov) * 10000 + as.integer(constituency_id) * 1000 + as.integer(cmun))
    extract$query_candidates <- select(extract$query_candidates, candidature_id, candidate_name, candidature_id_party)

    # merge candidate
    extract$merge_votes_cand <- left_join(extract$query_votes, extract$query_candidates, by = "candidature_id")

    # prepare merge candidature
    extract$query_candidatures <- rename(extract$query_candidatures, candidature_id_party = candidature_id)

    extract$merge_votes_cand <- left_join(extract$merge_votes_cand, extract$query_candidatures, by = "candidature_id_party")

  } else {

    # merge candidature
    extract$merge_votes_cand <- left_join(extract$query_votes, extract$query_candidatures, by = "candidature_id")

  }

  return(extract)

}

collapse_votes <-  function(queries, extract){

  switch (queries$by[1],
          "nacional" = group_vars <- c("elec_id", "caut", "candidature_acron"),
          "comunidad" = group_vars <- c("elec_id", "caut", "candidature_acron"),
          "provincia" = group_vars <- c("elec_id", "caut", "cprov", "candidature_acron"),
          "municipio" = group_vars <- c("elec_id", "caut", "cprov", "cmun", "candidature_acron"),
          "distrito" = group_vars <- c("elec_id", "caut", "cprov", "cmun", "cdist", "candidature_acron"),
          "seccion" = group_vars <- c("elec_id", "caut", "cprov", "cmun", "cdist", "csec", "candidature_acron"),
          "mesa" = group_vars <- c("elec_id", "caut", "cprov", "cmun", "cdist", "csec", "cmesa", "candidature_acron")
  )

  arrange_vars <- c(group_vars[-length(group_vars)], "votes_candidature")
  if(queries$elec[1] == 3) {c(group_vars, "candidate_name")}
  select_vars <- c(group_vars, "votes_candidature")
  if(queries$by[1] %in% c("nacional", "comunidad", "provincia", "municipio")) {select_vars <- c(select_vars, "candidates_elected")}

  extract$merge_votes_cand <- extract$merge_votes_cand %>%
    select(all_of(select_vars)) %>%
    group_by_at(group_vars) %>%
    summarise_all(sum) %>%
    arrange_at(arrange_vars)

  return(extract$merge_votes_cand)
}

collapse_turn <- function(queries, extract){

  base_vars_upper <- c("population", "n_precint", "census_ine", "census_escrut", "census_cere", "turnout_pre_1", "turnout_pre_2", "votes_blank", "votes_null", "votes_candidatures", "seats")
  base_vars_down <- c("census_ine", "census_escrut", "census_cere", "turnout_pre_1", "turnout_pre_2", "votes_blank", "votes_null", "votes_candidatures")

  switch (queries$by[1],
          "nacional" = group_vars <- c("elec_id", "year", "month", "caut"),
          "comunidad" = group_vars <- c("elec_id", "year", "month", "caut"),
          "provincia" = group_vars <- c("elec_id", "year", "month", "caut", "cprov"),
          "municipio" = group_vars <- c("elec_id", "year", "month", "caut", "cprov", "cmun"),
          "distrito" = group_vars <- c("elec_id", "year", "month", "caut", "cprov", "cmun", "cdist"),
          "seccion" = group_vars <- c("elec_id", "year", "month", "caut", "cprov", "cmun", "cdist", "csec"),
          "mesa" = group_vars <- c("elec_id", "year", "month", "caut", "cprov", "cmun", "cdist", "csec", "cmesa")
  )

  if(queries$by[1] %in% c("nacional", "comunidad", "provincia", "municipio")){

    select_vars <- c(group_vars, base_vars_upper)

  } else {

    select_vars <- c(group_vars, base_vars_down)

    }

  extract$query_turn <- extract$query_turn %>%
    select(all_of(select_vars)) %>%
    mutate(turnout_final = votes_candidatures + votes_blank + votes_null,
           votes_valid = votes_candidatures + votes_blank) %>%
    group_by_at(group_vars) %>%
    summarise_all(sum)

  return(extract$query_turn)
}

output_summary <- function(elec, queries){

  output_vars_upper <- c("population", "census_escrut", "turnout_final", "votes_blank", "votes_null", "votes_candidatures", "votes_valid", "seats", "candidature_acron", "votes_candidature", "candidates_elected")
  output_vars_down <- c("census_escrut", "turnout_final", "votes_blank", "votes_null", "votes_candidatures", "votes_valid", "candidature_acron", "votes_candidature")

  switch (queries$by[1],
          "nacional" = select_vars <- c("elec_id", "year", "month", "caut", output_vars_upper),
          "comunidad" = select_vars <- c("elec_id", "year", "month", "caut", output_vars_upper),
          "provincia" = select_vars <- c("elec_id", "year", "month", "caut", "cprov", output_vars_upper),
          "municipio" = select_vars <- c("elec_id", "year", "month", "caut", "cprov", "cmun", output_vars_upper),
          "distrito" = select_vars <- c("elec_id", "year", "month", "caut", "cprov", "cmun", "cdist", output_vars_down),
          "seccion" = select_vars <- c("elec_id", "year", "month", "caut", "cprov", "cmun", "cdist", "csec", output_vars_down),
          "mesa" = select_vars <- c("elec_id", "year", "month", "caut", "cprov", "cmun", "cdist", "csec", "cmesa", output_vars_down)
  )

  elec <- elec %>%
    select(all_of(select_vars)) %>%
    rename(party = candidature_acron,
           votes = votes_candidature,
           elec = elec_id) %>%
    mutate(elec = queries$elec_type[1])

  if(queries$by[1] != "nacional"){

    elec <- elec %>%
      rename(caut_mir = caut) %>%
      left_join(pablo:::lkup_ine_mir, by = "caut_mir") %>%
      mutate(caut_mir = caut_ine) %>%
      rename(caut = caut_mir) %>%
      select(-caut_ine, -autonomia)

    }

  if(queries$by[1] %in% c("nacional", "comunidad", "provincia", "municipio")){
    elec <- elec %>%
      rename(total_seats = seats,
             seats = candidates_elected)
  }

  return(elec)

}

output_all <- function(elec, queries){}

# 3.2 Format output -------------------------------------------------------

format_output <- function(queries, extract){

  ## clean the cnadidates and candidatures names
  extract <- clean_candidature_names(queries$elec[1], extract)

  ## merge candidate and candidatures with votes
  extract <- merge_votes_candidature(queries$elec[1], extract)

  ## collapse
  votes <- collapse_votes(queries, extract)
  turn <- collapse_turn(queries, extract)

  ## merge turn and votes
  switch(queries$by[1],
          "nacional" = join_vars <- c("elec_id", "caut"),
          "comunidad" = join_vars <- c("elec_id", "caut"),
          "provincia" = join_vars <- c("elec_id", "caut", "cprov"),
          "municipio" = join_vars <- c("elec_id", "caut", "cprov", "cmun"),
          "distrito" = join_vars <- c("elec_id", "caut", "cprov", "cmun", "cdist"),
          "seccion" = join_vars <- c("elec_id", "caut", "cprov", "cmun", "cdist", "csec"),
          "mesa" = join_vars <- c("elec_id", "caut", "cprov", "cmun", "cdist", "csec", "cmesa")
  )

    elec <- right_join(turn, votes, by = join_vars)

  ## prepare output
  if(queries$output[1] == "summary"){
    elec <- output_summary(elec, queries)
  } else {
    elec <- output_all(elec, queries)
  }

  return(as_tibble(elec))
}


# 4. Get election data ----------------------------------------------------

#' Get election data
#'
#' This function gets election data from a repository.
#'
#' @param elec Character. Type of election "cong", "sen", "municipio", "ref", "euro", "cabildo"
#' @param year Numeric. Year of the election
#' @param month Numeric. Month of the election only necessary for 2019 "congreso" and "senado". Default is NULL.
#' @param ine_geo_code Character. INE geographical code to retrieve the results from a given area.
#' @param by Character. Level at which data is retrieved: "nacional", "comunidad", "provincia", "municipio", "distrito", "seccion", "mesa".
#' @param output Character. Type of output "summary" or "all".
#' @return A data frames containing the election results.
#' @export



elections <- function(elec,
                      year,
                      month = NULL,
                      ine_geo_code = NULL,
                      by = NULL,
                      output = "summary"){

 queries <- arrange_queries(elec = elec, year = year, month = month, ine_geo_code = ine_geo_code, by = by, output = output)
 extract <- extract_data(queries = queries)
 return_list <- format_output(queries = queries, extract = extract)
 return(return_list)
}


# 5. Get ine codes --------------------------------------------------------

#' Get geographical codes
#'
#' This function gets INE geographical codes.
#'
#' @param geo_area Character. INE geographical code to retrieve the results from a given area.
#' @return A data frames containing the geographical codes.
#' @export

lookup_ine_geo_code <- function(geo_area){

  df <- pablo:::ine_codes[str_detect(pablo:::ine_codes$Area, geo_area), ] %>%
    mutate(caut = str_sub(Code, 1, 2),
           cprov = str_sub(Code, 3, 4),
           cmun = ifelse(Level == "Isla", "", str_sub(Code, 5, 7))) %>%
    rename('Code INE' = Code)

  return(df)

}


