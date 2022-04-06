#' Add delta variable
#'
#' This function will create a delta variable given a subject id, variable name
#' and dataframe. It will also skip NA values in the calculation of the delta.
#'
#' @param data the dataframe to be called
#' @param cols the character string or character vector of columns to apply this
#' function to
#' @param id a character string specifying the name of the id variable
#' @param time a character stirng the time variable
#' @param fill numeric or NA to specifying what to fill the values that fall
#' outside the offset window. For example, if we ar taking lagged differences,
#' this will set the value of the firt observation.
#' @param n number of steps to alter rolling difference window
#' @param type character string whether lead or lag should be used
#' @param prefix a character string denothing the prefix of the delta column.
#' The column name for the new delta column will be "prefix_oldname"
#'
#' @author Jorge Martinez Romero, William Mueller
#'
#' @examples
#'
#' # Example ------------------------------------------------------------------
#'
#' # dplyr must be in NAMESPACE
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'
#'   # merge nmr with SLAM census
#'   data <- dplyr::left_join(nmr, census, by = "idno")
#'
#'   # checkout data
#'   head(data)
#'
#'   # add delta variables with 1 time lagged differences and fill with 0
#'   data <- add_delta(
#'     data = data,
#'     cols = c("bw", "fat", "lean", "fluid"),
#'     id = "idno",
#'     time = "date"
#'   )
#'
#'   # add delta variable with 2 time lagged differences and fill with 0
#'   data <- add_delta(
#'     data = data,
#'     cols = c("bw", "fat", "lean", "fluid"),
#'     id = "idno",
#'     time = "date",
#'     n = 2
#'   )
#' }
#' @export
#'
#' @importFrom data.table ':=' .SD
#'

add_delta <- function(data,
                      cols,
                      id,
                      time,
                      fill = 0,
                      n = 1L,
                      type = "lag",
                      prefix = paste("delta", type, n, sep = "_")) {

  # convert fill to numeric
  fill <- as.numeric(fill)
  # convert data to datatable
  dt <- data.table::data.table(data)

  # loop through cols ----------------------------------------------------------
  for (col in cols) {
    dt <- mutate_delta_one_col(
      dt = dt,
      col = col,
      id = id,
      time = time,
      fill = fill,
      n = n,
      type = type,
      prefix = prefix
    )
  }
  # ----------------------------------------------------------------------------

  # reorder by id and time because currently order by time, id, and col_na
  data.table::setorderv(x = dt, col = c(id, time))
  # convert data.table to dataframe
  data <- as.data.frame(dt)

  # return data
  data
}

# This function will fill the NA's created by lagged differences given, the
# lag column

fill_lag_na <- function(dt, col_delta, col_na, fill) {
  replace_rows <- which(is.na(dt[[col_delta]]) & !dt[[col_na]])

  data.table::set(dt,
    i = eval(replace_rows),
    j = eval(col_delta),
    value = eval(fill)
  )

  dt
}

# This function creates a lagged value column for a data.table determined by
# n and type

mutate_lag_skip_na <- function(dt, col, id, col_na, col_name, n, type) {
  data.table::setkeyv(dt, cols = c(id, col_na))

  data.table::set(dt,
    i = NULL,
    j = eval(col_name),
    value = dt[[col]] - data.table::shift(
      x = dt[[col]],
      n = n,
      fill = NA,
      type = type
    )
  )

  # remove key
  data.table::setkey(dt, NULL)

  dt
}

mutate_delta_one_col <- function(dt, col, id, time, fill, n, type, prefix) {

  # calculate deltas for variable defined by col -----------------------------
  # delta variable name
  col_delta <- paste(prefix, col, sep = "_")
  # na varialbe name
  col_na <- paste(col, "na", sep = "_")

  # data table chain ---------------------------------------------------------
  # create col_na that is true if col is NA and false otherwise
  data.table::set(dt,
    i = NULL,
    j = eval(col_na),
    value = sapply(dt[[col]], is.na)
  )

  # order dt by time so that rolling differences are taken properly
  data.table::setorderv(dt, cols = eval(time))

  # create delta variable using n and type args specified in function call.
  # the dt is grouped by id and col_na so that delta calculation will "skip"
  # dates when there is NAs
  dt <- mutate_lag_skip_na(dt, col, id, col_na, col_delta, n, type)

  # if there are NA's in col_delta and no NAs in col, we know that the NA
  # in col_delta is from lag window. We want to replace these NA's. However,
  # any NA's in col_delta from due to an NA in col, we want to leave these NA.
  dt <- fill_lag_na(dt, col_delta, col_na, fill)

  # Drop the na column
  data.table::set(dt, i = NULL, j = eval(col_na), value = NULL)

  dt
}
