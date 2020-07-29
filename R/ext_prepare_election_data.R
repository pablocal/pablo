# Metadata ----------------------------------------------------------------
# Title: ext_prepare_election_data.R
# Purpose: Prepare election data bf upload to SQL
# Author(s): @pablocal
# Date: 2020-06-04 18:10:48
#
# Comments ----------------------------------------------------------------
#
#
#
#
#
# Options and packages ----------------------------------------------------



# 1. Extract id data ------------------------------------------------------

get_id_data <- function(path){

return_df <- read_fwf(path, fwf_cols(elec_id = c(1, 2),
                                       year = c(3, 6),
                                       month = c(7, 8),
                                       round = c(9, 9),
                                       area_type = c(10, 10),
                                       area = c(11, 12),
                                       date_day = c(13, 14),
                                       date_month = c(15, 16),
                                       date_year = c(17, 20),
                                       start_time = c(21, 25),
                                       end_time = c(26, 30),
                                       time_turnout_pre_1 = c(31, 35),
                                       time_trunout_pre_2 = c(36, 40)),
                  col_types = cols(elec_id = col_integer(),
                                   year = col_integer(),
                                   month = col_integer(),
                                   round = col_integer(),
                                   area_type = col_character(),
                                   area = col_integer(),
                                   date_day = col_integer(),
                                   date_month = col_integer(),
                                   date_year = col_integer(),
                                   start_time = col_time(),
                                   end_time = col_time(),
                                   time_turnout_pre_1 = col_time(),
                                   time_trunout_pre_2 = col_time()))

return(return_df)

}

# 2. Extract candidatures -------------------------------------------------

get_candidature_data <- function(path){

  return_df <- read_fwf(path, fwf_cols(elec_id = c(1, 2),
                                                      year = c(3, 6),
                                                      month = c(7, 8),
                                                      candidature_id = c(9, 14),
                                                      candidature_acron = c(15, 64),
                                                      candidature_name = c(65, 214),
                                                      candidature_id_prov = c(215, 220),
                                                      candidature_id_aut = c(221, 226),
                                                      candidature_id_country = c(227, 232)),
                                      col_types = cols(elec_id = col_integer(),
                                                       year = col_integer(),
                                                       month = col_integer(),
                                                       candidature_id = col_integer(),
                                                       candidature_acron = col_character(),
                                                       candidature_name = col_character(),
                                                       candidature_id_prov = col_integer(),
                                                       candidature_id_aut = col_integer(),
                                                       candidature_id_country = col_integer()),
                                      locale = locale(encoding = "latin1"))

return(return_df)

}



# 3. Extract candidates ---------------------------------------------------

get_candidates_data <- function(path){
return_df <- read_fwf(path, fwf_cols(elec_id = c(1, 2),
                                                               year = c(3, 6),
                                                               month = c(7, 8),
                                                               round = c(9, 9),
                                                               cprov = c(10, 11),
                                                               constituency_id = c(12, 12),
                                                               cmun = c(13, 15),
                                                               candidature_id = c(16, 21),
                                                               candidate_position = c(22, 24),
                                                               candidate_type = c(25, 25),
                                                               candidate_name = c(26, 100),
                                                               candidate_gender = c(101, 101),
                                                               candidate_birth_day = c(102, 103),
                                                               candidate_birth_month = c(104, 105),
                                                               candidate_birth_year = c(106, 109),
                                                               candidate_elected = c(120, 120)),
                                                       col_types = cols(elec_id = col_integer(),
                                                                        year = col_integer(),
                                                                        month = col_integer(),
                                                                        round = col_integer(),
                                                                        cprov= col_character(),
                                                                        constituency_id = col_integer(),
                                                                        cmun = col_character(),
                                                                        candidature_id = col_integer(),
                                                                        candidate_position = col_integer(),
                                                                        candidate_type = col_character(),
                                                                        candidate_name = col_character(),
                                                                        candidate_gender = col_character(),
                                                                        candidate_birth_day = col_integer(),
                                                                        candidate_birth_month = col_integer(),
                                                                        candidate_birth_year = col_integer(),
                                                                        candidate_elected = col_character()),
                                                       locale = locale(encoding = "latin1"))
return(return_df)
}



# 4. Extract municipalities -----------------------------------------------

get_mun_data <- function(path){

return_df <- read_fwf(path, fwf_cols(elec_id = c(1, 2),
                                                               year = c(3, 6),
                                                               month = c(7, 8),
                                                               round = c(9, 9),
                                                               caut = c(10, 11),
                                                               cprov = c(12, 13),
                                                               cmun = c(14, 16),
                                                               dist_mun = c(17, 18),
                                                               mun_name = c(19, 118),
                                                               constituency_id = c(119, 119),
                                                               part_jud_id = c(120, 122),
                                                               diput_cprov = c(123, 125),
                                                               comarca_id = c(126, 128),
                                                               population = c(129, 136),
                                                               n_precint = c(137, 141),
                                                               census_ine = c(142, 149),
                                                               census_escrut = c(150, 157),
                                                               census_cere = c(158, 165),
                                                               turnout_cere = c(166, 173),
                                                               turnout_pre_1 = c(174, 181),
                                                               turnout_pre_2 = c(182, 189),
                                                               votes_blank = c(190, 197),
                                                               votes_null = c(198, 205),
                                                               votes_candidatures = c(206, 213),
                                                               seats = c(214, 216),
                                                               refer_yes = c(217, 224),
                                                               refer_no = c(225, 232),
                                                               official_data = c(233, 233)),
                                               col_types = cols(
                                                                elec_id = col_integer(),
                                                                year = col_integer(),
                                                                month = col_integer(),
                                                                round = col_integer(),
                                                                caut = col_character(),
                                                                cprov= col_character(),
                                                                cmun = col_character(),
                                                                dist_mun = col_integer(),
                                                                mun_name = col_character(),
                                                                constituency_id = col_integer(),
                                                                part_jud_id = col_integer(),
                                                                diput_cprov= col_character(),
                                                                comarca_id = col_integer(),
                                                                population = col_integer(),
                                                                n_precint = col_integer(),
                                                                census_ine = col_integer(),
                                                                census_escrut = col_integer(),
                                                                census_cere = col_integer(),
                                                                turnout_cere = col_integer(),
                                                                turnout_pre_1 = col_integer(),
                                                                turnout_pre_2 = col_integer(),
                                                                votes_blank = col_integer(),
                                                                votes_null = col_integer(),
                                                                votes_candidatures = col_integer(),
                                                                seats = col_integer(),
                                                                refer_yes = col_integer(),
                                                                refer_no = col_integer(),
                                                                official_data = col_character()),
                                               locale = locale(encoding = "latin1"))

return(return_df)
}




# 5. Extract candidatures at mun level ------------------------------------

get_candidature_mun <- function(path){

return_df <- read_fwf(path, fwf_cols(elec_id = c(1, 2),
                                                               year = c(3, 6),
                                                               month = c(7, 8),
                                                               round = c(9, 9),
                                                               cprov = c(10, 11),
                                                               cmun = c(12, 14),
                                                               dist_mun = c(15, 16),
                                                               candidature_id = c(17, 22),
                                                               votes_candidature = c(23, 30),
                                                               candidates_elected = c(31, 33)),
                                                     col_types = cols(elec_id = col_integer(),
                                                                      year = col_integer(),
                                                                      month = col_integer(),
                                                                      round = col_integer(),
                                                                      cprov= col_character(),
                                                                      cmun = col_character(),
                                                                      dist_mun = col_integer(),
                                                                      candidature_id = col_integer(),
                                                                      votes_candidature = col_integer(),
                                                                      candidates_elected = col_integer()),
                                                     locale = locale(encoding = "latin1"))

return(return_df)
}



# 6. Extract data upper mun -----------------------------------------------
get_upper_mun_data <- function(path){
return_df <- read_fwf(path, fwf_cols(elec_id = c(1, 2),
                                                               year = c(3, 6),
                                                               month = c(7, 8),
                                                               round = c(9, 9),
                                                               caut = c(10, 11),
                                                               cprov = c(12, 13),
                                                               constituency_id = c(14, 14),
                                                               area_name = c(15, 64),
                                                               population = c(65, 72),
                                                               n_precint = c(73, 77),
                                                               census_ine = c(78, 85),
                                                               census_escrut = c(86, 93),
                                                               census_cere = c(94, 101),
                                                               turnout_cere = c(102, 109),
                                                               turnout_pre_1 = c(110, 117),
                                                               turnout_pre_2 = c(118, 125),
                                                               votes_blank = c(126, 133),
                                                               votes_null = c(134, 141),
                                                               votes_candidatures = c(142, 149),
                                                               seats = c(150, 155),
                                                               refer_yes = c(156, 163),
                                                               refer_no = c(164, 171),
                                                               official_data = c(172, 172)),
                                                             col_types = cols(
                                                               elec_id = col_integer(),
                                                               year = col_integer(),
                                                               month = col_integer(),
                                                               round = col_integer(),
                                                               caut = col_character(),
                                                               cprov= col_character(),
                                                               constituency_id = col_integer(),
                                                               area_name = col_character(),
                                                               population = col_integer(),
                                                               n_precint = col_integer(),
                                                               census_ine = col_integer(),
                                                               census_escrut = col_integer(),
                                                               census_cere = col_integer(),
                                                               turnout_cere = col_integer(),
                                                               turnout_pre_1 = col_integer(),
                                                               turnout_pre_2 = col_integer(),
                                                               votes_blank = col_integer(),
                                                               votes_null = col_integer(),
                                                               votes_candidatures = col_integer(),
                                                               seats = col_integer(),
                                                               refer_yes = col_integer(),
                                                               refer_no = col_integer(),
                                                               official_data = col_character()),
                                                             locale = locale(encoding = "latin1"))

return(return_df)
}



# 7. Extract candidature data upper mun -----------------------------------
get_candidature_upper_mun <- function(path){

return_df <- read_fwf(path, fwf_cols(elec_id = c(1, 2),
                                                               year = c(3, 6),
                                                               month = c(7, 8),
                                                               round = c(9, 9),
                                                               caut = c(10, 11),
                                                               cprov = c(12, 13),
                                                               constituency_id = c(14, 14),
                                                               candidature_id = c(15, 20),
                                                               votes_candidature = c(21, 28),
                                                               candidates_elected = c(29, 33)),
                                               col_types = cols(elec_id = col_integer(),
                                                                year = col_integer(),
                                                                month = col_integer(),
                                                                round = col_integer(),
                                                                caut = col_character(),
                                                                cprov= col_character(),
                                                                constituency_id = col_integer(),
                                                                candidature_id = col_integer(),
                                                                votes_candidature = col_integer(),
                                                                candidates_elected = col_integer()),
                                               locale = locale(encoding = "latin1"))

return(return_df)
}


# 8. Extract data at precint level ----------------------------------------
get_precint_data <- function(path){

return_df <- read_fwf(path, fwf_cols(elec_id = c(1, 2),
                                                               year = c(3, 6),
                                                               month = c(7, 8),
                                                               round = c(9, 9),
                                                               caut = c(10, 11),
                                                               cprov = c(12, 13),
                                                               cmun = c(14, 16),
                                                               cdist = c(17, 18),
                                                               csec = c(19, 22),
                                                               cmesa = c(23, 23),
                                                               census_ine = c(24, 30),
                                                               census_escrut = c(31, 37),
                                                               census_cere = c(38, 44),
                                                               turnout_cere = c(45, 51),
                                                               turnout_pre_1 = c(52, 58),
                                                               turnout_pre_2 = c(59, 65),
                                                               votes_blank = c(66, 72),
                                                               votes_null = c(73, 79),
                                                               votes_candidatures = c(80, 86),
                                                               refer_yes = c(87, 93),
                                                               refer_no = c(94, 100),
                                                               official_data = c(101, 101)),
                                                             col_types = cols(
                                                               elec_id = col_integer(),
                                                               year = col_integer(),
                                                               month = col_integer(),
                                                               round = col_integer(),
                                                               caut = col_character(),
                                                               cprov= col_character(),
                                                               cmun = col_character(),
                                                               cdist = col_character(),
                                                               csec = col_character(),
                                                               cmesa = col_character(),
                                                               census_ine = col_integer(),
                                                               census_escrut = col_integer(),
                                                               census_cere = col_integer(),
                                                               turnout_cere = col_integer(),
                                                               turnout_pre_1 = col_integer(),
                                                               turnout_pre_2 = col_integer(),
                                                               votes_blank = col_integer(),
                                                               votes_null = col_integer(),
                                                               votes_candidatures = col_integer(),
                                                               refer_yes = col_integer(),
                                                               refer_no = col_integer(),
                                                               official_data = col_character()),
                                                             locale = locale(encoding = "latin1"))

return(return_df)
    }



# 9. Extract candidature data at precint ----------------------------------
get_candidature_precint <- function(path) {
return_df <- read_fwf(path, fwf_cols(elec_id = c(1, 2),
                                                               year = c(3, 6),
                                                               month = c(7, 8),
                                                               round = c(9, 9),
                                                               caut = c(10, 11),
                                                               cprov = c(12, 13),
                                                               cmun = c(14, 16),
                                                               cdist = c(17, 18),
                                                               csec = c(19, 22),
                                                               cmesa = c(23, 23),
                                                               candidature_id = c(24, 29),
                                                               votes_candidature = c(30, 36)),
                                                   col_types = cols(elec_id = col_integer(),
                                                                    year = col_integer(),
                                                                    month = col_integer(),
                                                                    round = col_integer(),
                                                                    caut = col_character(),
                                                                    cprov= col_character(),
                                                                    cmun = col_character(),
                                                                    cdist = col_character(),
                                                                    csec = col_character(),
                                                                    cmesa = col_character(),
                                                                    candidature_id = col_integer(),
                                                                    votes_candidature = col_integer()),
                                                   locale = locale(encoding = "latin1"))

return(return_df)
    }




# 10. Extract data mun less 250 inhab. ------------------------------------

get_mun_l250_data <- function(path){

return_df <- read_fwf(path, fwf_cols(type_mun = c(1, 2),
                                                               year = c(3, 6),
                                                               month = c(7, 8),
                                                               round = c(9, 9),
                                                               caut = c(10, 11),
                                                               cprov = c(12, 13),
                                                               cmun = c(14, 16),
                                                               mun_name = c(17, 116),
                                                               part_jud_id = c(117, 119),
                                                               diput_cprov = c(120, 122),
                                                               comarca_id = c(123, 125),
                                                               population = c(126, 128),
                                                               n_precint = c(129, 130),
                                                               census_ine = c(131, 133),
                                                               census_escrut = c(134, 136),
                                                               census_cere = c(137, 139),
                                                               turnout_cere = c(140, 142),
                                                               turnout_pre_1 = c(143, 145),
                                                               turnout_pre_2 = c(146, 148),
                                                               votes_blank = c(149, 151),
                                                               votes_null = c(152, 154),
                                                               votes_candidatures = c(155, 157),
                                                               seats = c(158, 159),
                                                               official_data = c(160, 160)),
                                                             col_types = cols(
                                                               type_mun = col_integer(),
                                                               year = col_integer(),
                                                               month = col_integer(),
                                                               round = col_integer(),
                                                               caut = col_character(),
                                                               cprov= col_character(),
                                                               cmun = col_character(),
                                                               mun_name = col_character(),
                                                               part_jud_id = col_integer(),
                                                               diput_cprov= col_character(),
                                                               comarca_id = col_integer(),
                                                               population = col_integer(),
                                                               n_precint = col_integer(),
                                                               census_ine = col_integer(),
                                                               census_escrut = col_integer(),
                                                               census_cere = col_integer(),
                                                               turnout_cere = col_integer(),
                                                               turnout_pre_1 = col_integer(),
                                                               turnout_pre_2 = col_integer(),
                                                               votes_blank = col_integer(),
                                                               votes_null = col_integer(),
                                                               votes_candidatures = col_integer(),
                                                               seats = col_integer(),
                                                               official_data = col_character()),
                                                             locale = locale(encoding = "latin1"))

    return(return_df)
    }



# 11. Extract candidature data for mun of less 250 inhab. -----------------
      get_candidature_mun_l250 <- function(path){

return_df <- read_fwf(path, fwf_cols(type_mun = c(1, 2),
                                                               year = c(3, 6),
                                                               month = c(7, 8),
                                                               round = c(9, 9),
                                                               cprov = c(10, 11),
                                                               cmun = c(12, 14),
                                                               candidature_id = c(15, 20),
                                                               votes_candidature = c(21, 23),
                                                               candidates_elected = c(24, 25),
                                                               candidate_name = c(26, 100),
                                                               candidate_gender = c(101, 101),
                                                               candidate_birth_day = c(102, 103),
                                                               candidate_birth_month = c(104, 105),
                                                               candidate_birth_year = c(106, 109),
                                                               votes_candidate = c(119, 121),
                                                               candidate_elected = c(122, 122)),
                                                             col_types = cols(
                                                               type_mun = col_integer(),
                                                               year = col_integer(),
                                                               month = col_integer(),
                                                               round = col_integer(),
                                                               cprov= col_character(),
                                                               cmun = col_character(),
                                                               candidature_id = col_integer(),
                                                               votes_candidature = col_integer(),
                                                               candidates_elected = col_integer(),
                                                               candidate_name = col_character(),
                                                               candidate_gender =  col_character(),
                                                               candidate_birth_day = col_integer(),
                                                               candidate_birth_month = col_integer(),
                                                               candidate_birth_year = col_integer(),
                                                               votes_candidate = col_integer(),
                                                               candidate_elected = col_character()),
                                                             locale = locale(encoding = "latin1"))

      return(return_df)
    }



# 12. Global function -----------------------------------------------------

#' Read and prepare election data
#'
#' This function gets election data from original DTA files
#'
#' @param path Path to the DTA file from MIR
#' @return A data frame with the election data
#' @export

read_elec_data <- function(path){

  id <- str_sub(path, length(path)-13, length(path)-12)

   return_df <- switch(id,
               "02" = get_id_data(path),
               "03" = get_candidature_data(path),
               "04" = get_candidates_data(path),
               "05" = get_mun_data(path),
               "06" = get_candidature_mun(path),
               "07" = get_upper_mun_data(path),
               "08" = get_candidature_upper_mun(path),
               "09" = get_precint_data(path),
               "10" = get_candidature_precint(path),
               "11" = get_mun_l250_data(path),
               "12" = get_candidature_mun_l250(path)
               )

   return(return_df)

}




