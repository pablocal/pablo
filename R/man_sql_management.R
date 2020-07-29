# Metadata ----------------------------------------------------------------
# Title: sql_magement.R
# Purpose: Set of useful sql wraps
# Author(s): @pablocal
# Date: 2020-06-04 11:18:34
#
# Comments ----------------------------------------------------------------
#
#
#
#
#
# Options and packages ----------------------------------------------------


#' Upload df to table per batches
#'
#' It uploads the data to the MariaDB/MySQL using dbx
#'
#'
#' @param con Connection to db.
#' @param table_name Name of the db table to insert the cases.
#' @param df Data frame to be uploaded.
#' @param batch_size Size of the batch
#' @return Message to indicate the cases uploaded.
#' @export

sql_insert_batches <- function(con, table_name, df, batch_size){

  if(length(batch_size) != 1) stop("batch_size must be a vector of size 1")
  if(!is.numeric(batch_size)) stop("batch_size must be a integer")
  if(!is.data.frame(df)) stop("df must be a data frame")
  if(!is.character(table_name)) stop("table_name must be a character")

  n_rows <- 1:nrow(df)
  batches <- split(n_rows, ceiling(seq_along(n_rows)/batch_size))

  for(b in batches){
    dbxInsert(con, table_name, df[b,])
  }

}



