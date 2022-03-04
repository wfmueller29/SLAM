#' Merge Difdate
#'
#' Functions that merges two dataframes that have measurements at different dates
#' It matches the closest measurement in data2 before, after, or before or after the
#' measurement in data1. If a threshold is specified, it will ensure the closest measurement
#' falls within that threshold or it will set it to missing.
#'
#' @param data1 a dataframe that has the ids and dates of measurement that will serve
#' as a reference for matching with data2
#' @param data2 a dataframe that has the ids and dates of measurement we would like to
#' match with the reference id's and dates and data1
#' @param id either a character string, or a character vector of length 2. If the id variable
#' in data1 and data2 have the same name, id is a character string. If the id variable of
#' data1 and data2 are different, then id is a character vector, with the first value being the
#' id variable of data1 and the second being the id variable for data2.
#' @param date either a character string, or a character vector of length 2. If the date variable
#' in data1 and data2 have the same name, date is a character string. If the date variable of
#' data1 and data2 are different, then date is a character vector, with the first value being the
#' date variable of data1 and the second being the date variable for data2.
#' @param threshold a named numeric value that provides the difference in time
#' between measurements allowed. The units of the threshold are specified by the
#' name of threshold. Valid units are can be found in the \link[base]{difftime}
#' units argument.
#' @param vars optional character vector that provides the variable names for which this should
#' be applied over. If specified, for each variable specified by var, only non-missing values will be considered
#' when merging with the closest measurement date.
#' @param where a character string that specifies where to look for the closest
#' observation in data2 relative to data1. "before" means that merge_difdate
#' will look before the reference observation in data1. "after" means the same
#' except after. "both" means that merge_difdate will match using observations
#' on either side of the reference observation in data1.
#' @param suffixes specifies the suffix for non-unique variables between the two
#' dataframes. However, even if the date and id variables are unique between the
#' two dataframes, they will be assigned a suffix.
#' @param clean_vars default is true. Will return all data1 columns and remove
#' any duplicate columns from data2 with the exception of the date column.
#'
#' @return a dataframe where each unique subject and date measurement in data1 is matched with the
#' closest dated measurement for that subject in data2.
#'
#' @author William Mueller, Jorge Martinez Romero
#'
#' @export

merge_difdate <- function(data1, data2, id, date, threshold = c("weeks" = Inf), vars = NULL, where = "both", suffixes = c(".1", ".2"), clean_vars = TRUE) {

  # for R CMD Check
  dif_ef <- NULL

  # prep data1 and data2 -------------------------------------------------------
  # convert data1 and data2 to data.tables
  dt1 <- data.table::as.data.table(data1)
  dt2 <- data.table::as.data.table(data2)

  # manually add suffixes so easy to identify which column came from which dataframe
  names(dt1) <- paste0(names(dt1), suffixes[1])
  names(dt2) <- paste0(names(dt2), suffixes[2])

  # create id and date vars if length id = 1 or 2 -------------------------------
  # do this for consistency of function regardless of date or id input
  # create corresponding id vars
  if (length(id) == 1) {
    id1 <- paste0(id, suffixes[1])
    id2 <- paste0(id, suffixes[2])
  } else if (length(id) == 2) {
    id1 <- paste0(id[1], suffixes[1])
    id2 <- paste0(id[2], suffixes[2])
  } else {
    stop("id must be length 1 or 2")
  }

  # create corresponding date vars
  if (length(date) == 1) {
    date1 <- paste0(date, suffixes[1])
    date2 <- paste0(date, suffixes[2])
  } else if (length(date) == 2) {
    date1 <- paste0(date[1], suffixes[1])
    date2 <- paste0(date[2], suffixes[2])
  } else {
    stop("date must be length 1 or 2")
  }

  # if vars specified ----------------------------------------------------------
  # if vars are specified ensure complete cases for those vars
  if (is.null(vars)) {
    dt2 <- dt2
  } else if (!is.null(vars)) {
    vars_suf <- paste0(vars, suffixes[2])
    dt2 <- dt2[stats::complete.cases(dt2[, .SD, .SDcols = vars_suf])]
  }

  # merge and create dif -------------------------------------------------------
  eval(as.call(list(str2lang("data.table::setkey"), x = as.symbol("dt1"), id1)))
  eval(as.call(list(str2lang("data.table::setkey"), x = as.symbol("dt2"), id2)))

  dtm <- merge(dt1, dt2, by.x = id1, by.y = id2, all.x = TRUE, allow.cartesian = TRUE)

  # dif tells use time difference and direction
  dtm$dif <- difftime(dtm[[date2]], dtm[[date1]], units = names(threshold))

  # set dif_ef (effective dif) depending upon where we are looking for nearest
  # measurement
  if (where == "both") {dtm$dif_ef <- abs(dtm$dif)
  } else if (where == "before") {dtm$dif_ef <- dtm$dif * -1
  } else if (where == "after") {dtm$dif_ef <- dtm$dif
  } else stop("where must be both, before, or after")

  dtm <- dtm[eval(call(name = "order", as.symbol(id1), as.symbol("dif_ef")))]
  eval(as.call(list(str2lang("data.table::setkey"), x = as.symbol("dtm"), id1, date1)))
  dtm <- eval(as.call(list(unique, x = as.symbol("dtm"), by = c(id1, date1))))

  # threshold
  dt2_cols <- names(dtm)[grepl(paste0(suffixes[2], "$"), names(dtm))]
  dt2_cols_not_date <- dt2_cols[sapply(dt2_cols, function(col) class(dtm[[col]])) != "Date"]
  dtm <- dtm[dif_ef > eval(threshold[[1]]), (dt2_cols_not_date) := NA]


  # clean vars -----------------------------------------------------------------
  if (isTRUE(clean_vars)) {
    suf1_regx <- paste0(suffixes[1], "$")
    suf2_regx <- paste0(suffixes[2], "$")
    col1 <- names(dtm)[grepl(suf1_regx, names(dtm))]
    col2 <- names(dtm)[grepl(suf2_regx, names(dtm))]

    col1_clean <- gsub(suf1_regx, replacement = "", x = col1)
    col2_clean <- gsub(suf2_regx, replacement = "", x = col2)

    if (length(date) == 1) {
      col1_clean <- gsub(date, replacement = date1, x = col1_clean)
      col2_clean <- gsub(date, replacement = date2, x = col2_clean)
    } else if (length(date) == 2) {
      col1_clean <- gsub(date[1], replacement = date1, x = col1_clean)
      col2_clean <- gsub(date[2], replacement = date2, x = col2_clean)
    }

    col2_var_keep <- col2[!(col2_clean %in% col1_clean)]
    col2_clean_var_keep <- col2_clean[!(col2_clean %in% col1_clean)]
    var_keep <- c(col1, col2_var_keep, "dif")

    dtm <- dtm[, .SD, .SDcols = var_keep]
    names(dtm) <- c(col1_clean, col2_clean_var_keep, paste0("dif_", names(threshold)))
  }

  # return as.data.frame-------------------------------------------------------
  as.data.frame(dtm)
}

