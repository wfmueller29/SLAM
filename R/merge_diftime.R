#' Merge Diftime
#'
#' Functions that merges two dataframes that have measurements at different
#' ages. It matches the closest measurement in data2 before, after, or before
#' or after the measurement in data1. If a threshold is specified, it will
#' ensure the closest measurement falls within that threshold or it will set
#' it to missing.
#'
#' @param data1 a dataframe that has the ids and ages of measurement that will
#' serve as a reference for matching with data2
#' @param data2 a dataframe that has the ids and ages of measurement we would
#' like to match with the reference id's and dates and data1
#' @param id either a character string, or a character vector of length 2. If
#' the id variable in data1 and data2 have the same name, id is a character
#' string. If the id variable of data1 and data2 are different, then id is a
#' character vector, with the first value being the id variable of data1 and
#' the second being the id variable for data2.
#' @param age either a character string, or a character vector of length 2.
#' If the age variable in data1 and data2 have the same name, age is a character
#' string. If the age variable of data1 and data2 are different, then age is a
#' character vector, with the first value being the age variable of data1 and
#' the second being the age variable for data2.
#' @param threshold a numeric value that provides the difference in time
#' between measurements allowed. The numeric should be in the same units of
#' age.
#' @param vars optional character vector that provides the variable names for
#' which this should be applied over. If specified, for each variable specified
#' by var, only non-missing values will be considered when merging with the
#' closest measurement date.
#' @param where a character string that specifies where to look for the closest
#' observation in data2 relative to data1. "before" means that merge_diftime
#' will look before the reference observation in data1. "after" means the same
#' except after. "both" means that merge_ditime will match using observations
#' on either side of the reference observation in data1.
#' @param suffixes specifies the suffix for non-unique variables between the
#' two dataframes. However, even if the age and id variables are unique between
#' the two dataframes, they will be assigned a suffix.
#' @param clean_vars default is true. Will return all data1 columns and remove
#' any duplicate columns from data2 with the exception of the age column.
#'
#' @return a dataframe where each unique subject and age measurement in data1
#' is matched with the closest aged measurement for that subject in data2.
#'
#' @author William Mueller, Jorge Martinez Romero
#'
#' @examples
#'
#' # Example Merging clostest NMR to Glucose ----------------------------------
#'
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#' # Checkout data ------------------------------------------------------------
#' # Checkout census
#' head(data_SLAM_census)
#'
#' # Checkout glucose
#' head(data_SLAM_gluc)
#'
#' # Checkout nmr
#' head(data_SLAM_nmr)
#'
#' # Create gluc --------------------------------------------------------------
#' # join glucose and census for dob and other infor
#' gluc <- dplyr::left_join(data_SLAM_gluc, data_SLAM_census, by = "idno")
#' # drop useless vars
#' gluc <- dplyr::select(
#'   gluc,
#'   -c(
#'     lact,
#'     cohort,
#'     animal_id,
#'     tag,
#'     taghistory,
#'     cage,
#'     eartag,
#'     name,
#'     X
#'   )
#' )
#' # create age for merging and format data so it makes sense
#' gluc <- dplyr::mutate(gluc,
#'   age_wk = difftime(date, dob, units = "weeks"),
#'   date = as.Date(date, "%m%d%Y")
#' )
#'
#' # Create nmr ---------------------------------------------------------------
#' # join nmr with census for dob and other info
#' nmr <- dplyr::left_join(data_SLAM_nmr, data_SLAM_census, by = "idno")
#' # drop useless columns
#' nmr <- dplyr::select(
#'   nmr,
#'   -c(
#'     cohort,
#'     animal_id,
#'     tag,
#'     taghistory,
#'     cage,
#'     eartag,
#'     name,
#'     X
#'   )
#' )
#' # create age for merging and format data so it makes sense
#' nmr <- dplyr::mutate(nmr,
#'   age_wk = difftime(date, dob, units = "weeks"),
#'   date = as.Date(date, "%m%d%Y")
#' )
#'
#' # Use merge_diftime --------------------------------------------------------
#' gluc_nmr <- merge_diftime(
#'   data1 = gluc,
#'   data2 = nmr,
#'   id = "idno",
#'   age = "age_wk",
#'   vars = c("bw", "lean", "fluid", "fat"),
#'   clean_vars = FALSE
#' )
#'
#' # Checkout results
#' head(gluc_nmr)
#' } else {
#'   message("Install dplyr to run this example")
#' }
#'
#' @export


merge_diftime <- function(data1,
                          data2,
                          id,
                          age,
                          threshold = Inf,
                          vars = NULL,
                          where = "both",
                          suffixes = c(".1", ".2"),
                          clean_vars = TRUE) {

  # prep data1 and data2 -------------------------------------------------------
  # convert data1 and data2 to data.tables
  dt1 <- data.table::as.data.table(data1)
  dt2 <- data.table::as.data.table(data2)

  # manually add suffixes to identify which column came from which dataframe
  names(dt1) <- paste0(names(dt1), suffixes[1])
  names(dt2) <- paste0(names(dt2), suffixes[2])

  # create id and age vars if length id = 1 or 2 -------------------------------
  # do this for consistency of function regardless of age or id input
  # create corresponding id vars
  if (length(id) == 1) {
    id1 <- paste0(id, suffixes[1])
    id2 <- paste0(id, suffixes[2])
  } else if (length(id) == 2) {
    id1 <- paste0(id[1], suffixes[1])
    id2 <- paste0(id[2], suffixes[1])
  } else {
    stop("id must be length 1 or 2")
  }

  # create corresponding age vars
  if (length(age) == 1) {
    age1 <- paste0(age, suffixes[1])
    age2 <- paste0(age, suffixes[2])
  } else if (length(age) == 2) {
    age1 <- paste0(age[1], suffixes[1])
    age2 <- paste0(age[2], suffixes[1])
  } else {
    stop("age must be length 1 or 2")
  }

  # if vars specified ----------------------------------------------------------
  # if vars are specified ensure complete cases for those vars
  if (is.null(vars)) {
    dt2 <- dt2
  } else if (!is.null(vars)) {
    vars_suf <- paste0(vars, suffixes[2])
    complete_cases <- stats::complete.cases(dt2[, vars_suf, with = FALSE])
    dt2 <- dt2[complete_cases]
  }

  # merge and create dif -------------------------------------------------------
  dtm <- merge(dt1,
               dt2,
               by.x = id1,
               by.y = id2,
               all.x = TRUE,
               allow.cartesian = TRUE)

  # dif tells use time difference and direction
  dtm$dif <- dtm[[age2]] - dtm[[age1]]

  # set dif_ef (effective dif) depending upon where we are looking for nearest
  # measurement
  if (where == "both") {
    dtm$dif_ef <- abs(dtm$dif)
  } else if (where == "before") {
    dtm$dif_ef <- dtm$dif * -1
  } else if (where == "after") {
    dtm$dif_ef <- dtm$dif
  } else {
    stop("where must be both, before, or after")
  }

  data.table::setorderv(dtm, cols = c(id1, "dif_ef"))

  dtm <- base::unique(dtm, by = c(eval(id1), eval(age1)))

  # threshold
  get_class <- function(col) class(dtm[[col]])
  dt2_cols <- names(dtm)[grepl(paste0(suffixes[2], "$"), names(dtm))]
  dt2_cols_not_date <- dt2_cols[sapply(dt2_cols, get_class) != "Date"]
  set_na_row <- which(dtm$dif_ef > threshold[[1]])
  data.table::set(dtm,
                  i = eval(set_na_row),
                  j = dt2_cols_not_date,
                  value = NA
                  )

  # clean vars -----------------------------------------------------------------
  if (isTRUE(clean_vars)) {
    suf1_regx <- paste0(suffixes[1], "$")
    suf2_regx <- paste0(suffixes[2], "$")
    col1 <- names(dtm)[grepl(suf1_regx, names(dtm))]
    col2 <- names(dtm)[grepl(suf2_regx, names(dtm))]

    col1_clean <- gsub(suf1_regx, replacement = "", x = col1)
    col2_clean <- gsub(suf2_regx, replacement = "", x = col2)

    if (length(age) == 1) {
      col1_clean <- gsub(age, replacement = age1, x = col1_clean)
      col2_clean <- gsub(age, replacement = age2, x = col2_clean)
    } else if (length(age) == 2) {
      col1_clean <- gsub(age[1], replacement = age1, x = col1_clean)
      col2_clean <- gsub(age[2], replacement = age2, x = col2_clean)
    }

    col2_var_keep <- col2[!(col2_clean %in% col1_clean)]
    col2_clean_var_keep <- col2_clean[!(col2_clean %in% col1_clean)]
    var_keep <- c(col1, col2_var_keep, "dif")

    dtm <- dtm[, var_keep, with = FALSE]
    names(dtm) <- c(col1_clean,
                    col2_clean_var_keep,
                    paste0("dif_", names(threshold)))
  }

  # return as.data.frame-------------------------------------------------------
  as.data.frame(dtm)
}

