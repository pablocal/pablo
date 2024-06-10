# R-script - Metadata -----------------------------------------------------
# Title: data_wrangling.R
# Purpose:  Wrangling data functions
# Author(s): @pablocal
# Date: 2024-06-06
#
# Comments ----------------------------------------------------------------
#
#
#
#
#
# Options and packages ----------------------------------------------------

# 1. duplicates -----------------------------------------------------------

#' Identify duplicates and provide a summary
#'
#' This function can be used to assess the number of duplicates in a dataset based on one or several columns.
#'
#' @param df Data frame or tibble object.
#' @param cols Column names.
#' @param output Character. Can be either 'summary', 'flag' or 'all'. 'summary' provides a df with the number of duplicates based on the columns provided. 'flag' provides the original df with a new column 'dup_flag' identifying the number of times the row is in the df and 'rank_flag' which indicates the occurrence of the row. Default is summary.
#' @return A data frame with the summary or the data frame with the flags. If output is 'all' the return is a list with two data frames.
#' @export



duplicates <- function(df, ..., output = "summary"){

  # check the output
  if(!all(output %in% c("summary", "flag", "all"))) stop("output must be 'summary', 'flag' or 'all' ")

  # Capture the variables
  cols <- quos(...)


  if(output %in% c("flag", "all")){
    # duplicates flags
    duplicates_flag_df <- df %>%
      group_by(across(!!!cols)) %>%
      mutate(dup_flag = n(),
             dup_rank = row_number()) %>%
      ungroup()

      if(output == "flag"){ return(duplicates_flag_df)}

    }

    if(output %in% c("summary", "all")) {
      # summary duplicates
      duplicates_summary <- df %>%
        group_by(across(!!!cols)) %>%
        summarise(duplicates = n()) %>%
        ungroup() %>%
        group_by(duplicates) %>%
        summarise(freq = n())

      if(output == "summary"){
        return(duplicates_summary)
      } else {
         return(list(summary = duplicates_summary,
                     df_flags = duplicates_flag_df))
        }

    }

}



