#' Add delta variable
#'
#' This function will create a delta variable given a subject id, variable name
#' and dataframe.
#'
#' @param data the dataframe to be called
#' @param cols the character string or character vector of columns to apply this
#' function to
#' @param id the name of the id variable
#' @param time variable denoting the time variable
#' @param fill numeric or NA to
#' @param n number of steps to alter roling difference window
#' @param type character string whether lead or lag should be used
#' @param prefix prefix denoting type of delta
#'
#'
#'
#' @export
#'
#' @importFrom data.table ':=' .SD
#'

add_delta <-function(data, cols, id, time, fill = 0, n = 1L, type = "lag", prefix = paste("delta", type, n, sep = "_")){

  # convert fill to numeric
  fill <- as.numeric(fill)
  # convert data to datatable
  dt <- data.table::data.table(data)

  # loop through cols ----------------------------------------------------------
  for(col in cols){
    # calculate deltas for variable defined by col -----------------------------
    # delta variable name
    col_delta <- paste(prefix, col, sep = "_")
    # na varialbe name
    col_na <- paste(col, "na", sep = "_")

    # data table chain ---------------------------------------------------------
    # create col_na that is true if col is NA and false otherwise
    dt <- dt[, (col_na) := lapply(.SD, is.na), .SDcols = col
             # order dt by time so that rolling differences are taken properly
    ][order(dt[[time]])
      # create delta variable using n and type arguments specified in function call.
      # the dt is grouped by id and col_na so that delta calculation will "skip"
      # dates when there is NAs
    ][, (col_delta) := .SD - data.table::shift(x = .SD, n = n, fill = NA, type = type), keyby = c(id, col_na), .SDcols = col]
    # if there are NA's in col_delta and no NAs in col, we know that the NA
    # in col_delta is from lag window. We want to replace these NA's. However,
    # any NA's in col_delta from due to an NA in col, we want to leave these NA.
    dt <- dt[is.na(dt[[col_delta]]) & !dt[[col_na]], (col_delta) := (fill)
             # Drop the na column
    ][, (col_na) := NULL]

  }
  # ----------------------------------------------------------------------------

  # reorder by id and time because currently order by time, id, and col_na
  dt <- data.table::setorderv(x = dt, col = c(id, time))
  # convert data.table to dataframe
  data <-as.data.frame(dt)

  #return data
  data
}
