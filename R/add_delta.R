#' Add delta variable
#'
#' This function will create a delta variable given a subject id, variable name
#' and dataframe.
#'
#' @param data the dataframe to be called
#' @param var the variable name
#' @param id the name of the id variable
#'
#'
#'
#' @export
#'
#' @importFrom data.table .N .I ':='
#'

add_delta <-function(data, cols, id, time, fill = 0, n = 1L, type = "lag", prefix = paste("delta", type, n, sep = "_")){

  # convert data to datatable
  data_tbl <- data.table::data.table(data)

  for(col in cols){
    # calculate deltas for variable defined by col -----------------------------
    # delta variable name
    col_delta <- paste(prefix, col, sep = "_")
    # na varialbe name
    col_na <- paste(col, "na", sep = "_")

    # data table chain ---------------------------------------------------------
    # create col_na that is true if col is NA and false otherwise
    data_tbl <- data_tbl[, (col_na) := lapply(.SD, is.na), .SDcols = col
                         # order dt by time so that rolling differences are taken properly
    ][order(data_tbl[[time]])
      # create delta variable using n and type arguments specified in function call.
      # the dt is grouped by id and col_na so that delta calculation will "skip"
      # dates when there is NAs
    ][, (col_delta) := .SD - data.table::shift(x = .SD, n = n, fill = NA, type = type), keyby = c(id, col_na), .SDcols = col
      # if there are NA's in col_delta and no NAs in col, we know that the NA
      # in col_delta is from lag window. We want to replace these NA's. However,
      # any NA's in col_delta from due to an NA in col, we want to leave these NA.
    ][, (col_delta) := ifelse(is.na(.SD) & isFALSE(get(col_na)), ..fill, .SD), .SDcols = col]

  }

  # reorder by id and time because currently order by time, id, and col_na
  data_tbl <- data.table::setorderv(x = data_tbl, col = c(id, time))
  # convert data.table to dataframe
  data <-as.data.frame(data_tbl)

  #return data
  data
}
