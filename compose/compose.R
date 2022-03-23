## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup--------------------------------------------------------------------
library(SLAM)
library(dplyr)


## -----------------------------------------------------------------------------
# Example Merging clostest NMR to Glucose --------------------------------------

# Checkout data --------------------------------------------------------------
# Checkout census
head(data_SLAM_census)

# Checkout glucose
head(data_SLAM_gluc)

# Checkout nmr
head(data_SLAM_nmr)

# Create gluc ----------------------------------------------------------------
# join glucose and census for dob and other infor
gluc <- dplyr::left_join(data_SLAM_gluc, data_SLAM_census, by = "idno")
# drop useless vars
gluc <- dplyr::select(
  gluc,
  -c(
    lact,
    cohort,
    animal_id,
    tag,
    taghistory,
    cage,
    eartag,
    name,
    X
  )
)
# create age for merging and format data so it makes sense
gluc <- dplyr::mutate(gluc,
  age_wk = difftime(date, dob, units = "weeks"),
  date = as.Date(date, "%m%d%Y")
)

# Create nmr -----------------------------------------------------------------
# join nmr with census for dob and other info
nmr <- dplyr::left_join(data_SLAM_nmr, data_SLAM_census, by = "idno")
# drop useless columns
nmr <- dplyr::select(
  nmr,
  -c(
    cohort,
    animal_id,
    tag,
    taghistory,
    cage,
    eartag,
    name,
    X
  )
)
# create age for merging and format data so it makes sense
nmr <- dplyr::mutate(nmr,
  age_wk = difftime(date, dob, units = "weeks"),
  date = as.Date(date, "%m%d%Y")
)

# Use merge_diftime ----------------------------------------------------------
gluc_nmr <- merge_diftime(
  data1 = gluc,
  data2 = nmr,
  id = "idno",
  age = "age_wk",
  vars = c("bw", "lean", "fluid", "fat"),
  clean_vars = FALSE
)

# Checkout results
head(gluc_nmr)

# test ------------------------------------------------------------------------
data1 <- gluc
data2 <- nmr
id <- "idno"
age <- "age_wk"
vars <- c("bw", "lean", "fluid", "fat")
suffixes <- c(".1", ".2")
clean_vars <- "both"
threshold <- Inf
where <- "both"


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
  # manually add suffixes so easy to identify which column came from which
  # dataframe
  names(data1) <- paste0(names(data1), suffixes[1])
  names(data2) <- paste0(names(data2), suffixes[2])

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
    data2 <- data2
  } else if (!is.null(vars)) {
    vars_suf <- paste0(vars, suffixes[2])
    data2 <- data2[complete.cases(data2[vars_suf]), ]
  }

  # merge and create dif -------------------------------------------------------
  data_m <- merge(data1,
    data2,
    by.x = id1,
    by.y = id2,
    all.x = TRUE,
    suffixes = suffixes
  )

  # dif tells use time difference and direction
  data_m$dif <- data_m[[age2]] - data_m[[age1]]

  # look for closest date ------------------------------------------------------
  # where to check for closest date
  if (where == "both") {
    data_m <- data_m[order(data_m[[id1]], abs(data_m$dif)), , drop = FALSE]
    df_dups <- data_m[c(id1, age1)]
    data_m <- data_m[!duplicated(df_dups), , drop = FALSE]
    # threshold
    data2_cols <- names(data_m)[grepl(paste0(suffixes[2], "$"), names(data_m))]
    data2_cols <- data2_cols[sapply(data2_cols, function(col) class(data_m[[col]])) != "Date"]
    data_m[, data2_cols] <- lapply(data2_cols, function(name) {
      if (class(data_m[, name]) != "Date") {
        ifelse(abs(data_m$dif) > threshold, NA, data_m[, name])
      } else {}
    })
  } else if (where == "before") {
    data_m <- data_m[data_m$dif <= 0, ]
    data_m <- data_m[order(data_m[[id1]], -1 * data_m$dif), , drop = FALSE]
    df_dups <- data_m[c(id1, age1)]
    data_m <- data_m[!duplicated(df_dups), , drop = FALSE]
    # threshold
    data2_cols <- names(data_m)[grepl(paste0(suffixes[2], "$"), names(data_m))]
    data2_cols <- data2_cols[sapply(data2_cols, function(col) class(data_m[[col]])) != "Date"]
    data_m[, data2_cols] <- lapply(data2_cols, function(name) {
      ifelse(data_m$dif * -1 > threshold, NA, data_m[, name])
    })
  } else if (where == "after") {
    data_m <- data_m[data_m$dif >= 0, ]
    data_m <- data_m[order(data_m[[id1]], data_m$dif), , drop = FALSE]
    df_dups <- data_m[c(id1, age1)]
    data_m <- data_m[!duplicated(df_dups), , drop = FALSE]
    # threshold
    data2_cols <- names(data_m)[grepl(paste0(suffixes[2], "$"), names(data_m))]
    data2_cols <- data2_cols[sapply(data2_cols, function(col) class(data_m[[col]])) != "Date"]
    data_m[, data2_cols] <- lapply(data2_cols, function(name) {
      ifelse(data_m$dif > threshold, NA, data_m[, name])
    })
  } else {
    stop("where must be both, before, or after")
  }

  # clean vars -----------------------------------------------------------------
  if (isTRUE(clean_vars)) {
    suf1_regx <- paste0(suffixes[1], "$")
    suf2_regx <- paste0(suffixes[2], "$")
    col1 <- names(data_m)[grepl(suf1_regx, names(data_m))]
    col2 <- names(data_m)[grepl(suf2_regx, names(data_m))]

    col1_clean <- gsub(suf1_regx, replacement = "", x = col1)
    col2_clean <- gsub(suf2_regx, replacement = "", x = col2)

    if (length(age) == 1) {
      col1_clean <- gsub(age, replacement = age1, x = col1_clean)
      col2_clean <- gsub(age, replacement = age2, x = col2_clean)
    } else if (length(age) == 2) {
      col1_clean <- gsub(age[1], replacement = age1, x = col1_clean)
      col2_clean <- gsub(age[2], replacement = age2, x = col2_clean)
    }

    var_keep <- col2[!col2_clean %in% col1_clean]
    var_keep <- c(col1, var_keep, "dif")

    data_m <- data_m[, var_keep]
    names(data_m)[names(data_m) %in% col1] <- col1_clean
  }

  data_m
}


## ----add_delta----------------------------------------------------------------
# test ---------------------------------
data <- data_SLAM_nmr %>%
  dplyr::left_join(data_SLAM_census, by = "idno")

data <- data[data$cohort %in% c(1, 2, 3, 4, 5, 6), ]
cols <- c("bw", "fat")
id <- "idno"
type <- "lag"
fill <- 0
n <- 1L
time <- "date"
prefix <- paste("delta", type, n, sep = "_")

add_delta_test <- function(data,
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
    # calculate deltas for variable defined by col -----------------------------
    # delta variable name
    col_delta <- paste(prefix, col, sep = "_")
    # na varialbe name
    col_na <- paste(col, "na", sep = "_")

    # data table chain ---------------------------------------------------------
    # create col_na that is true if col is NA and false otherwise
    dt <- dt[, (col_na) := lapply(.SD, is.na),
      .SDcols = col
      # order dt by time so that rolling differences are taken properly
    ][
      order(dt[[time]])
      # create delta variable using n and type arguments specified in function call.
      # the dt is grouped by id and col_na so that delta calculation will "skip"
      # dates when there is NAs
    ][, (col_delta) := .SD - data.table::shift(x = .SD, n = n, fill = NA, type = type), keyby = c(id, col_na), .SDcols = col]
    # if there are NA's in col_delta and no NAs in col, we know that the NA
    # in col_delta is from lag window. We want to replace these NA's. However,
    # any NA's in col_delta from due to an NA in col, we want to leave these NA.
    dt <- dt[
      is.na(dt[[col_delta]]) & !dt[[col_na]], (col_delta) := (fill)
      # Drop the na column
    ][, (col_na) := NULL]
  }
  # ----------------------------------------------------------------------------

  # reorder by id and time because currently order by time, id, and col_na
  dt <- data.table::setorderv(x = dt, col = c(id, time))
  # convert data.table to dataframe
  data <- as.data.frame(dt)

  # return data
  data
}

data <- add_delta(data = data,
                  cols = c("bw", "fat", "lean", "fluid"),
                  id = id,
                  time = time,
                  fill = 0)

# Example ----------------------------------------------------------------------

# dplyr must be in NAMESPACE
if (requireNamespace("dplyr", quietly = TRUE)) {

  # merge nmr with SLAM census
  data <- dplyr::left_join(data_SLAM_nmr, data_SLAM_census, by = "idno")

  # checkout data
  head(data)

  # add delta variables with 1 time lagged differences and fill with 0
  data <- add_delta(
    data = data,
    cols = c("bw", "fat", "lean", "fluid"),
    id = "idno",
    time = "date"
  )

  # add delta variable with 2 time lagged differences to same dataset and fill with 0
  data <- add_delta(
    data = data,
    cols = c("bw", "fat", "lean", "fluid"),
    id = "idno",
    time = "date",
    n = 2
  )
}


## ----merge_difdate------------------------------------------------------------

# Example Merging clostest NMR to Glucose --------------------------------------

# Checkout data --------------------------------------------------------------
# Checkout census
head(data_SLAM_census)

# Checkout glucose
head(data_SLAM_gluc)

# Checkout nmr
head(data_SLAM_nmr)

# Create gluc ----------------------------------------------------------------
# join glucose and census for dob and other infor
gluc <- dplyr::left_join(data_SLAM_gluc, data_SLAM_census, by = "idno")
# drop useless vars
gluc <- dplyr::select(gluc, -c(
                               lact,
                               cohort,
                               animal_id,
                               tag,
                               taghistory,
                               cage,
                               eartag,
                               name,
                               X
                               )
)
# create age for merging and format data so it makes sense
gluc <- dplyr::mutate(gluc,
  age_wk = difftime(date, dob, units = "weeks"),
  date = as.Date(date, "%m%d%Y")
)

# Create nmr -----------------------------------------------------------------
# join nmr with census for dob and other info
nmr <- dplyr::left_join(data_SLAM_nmr, data_SLAM_census, by = "idno")
# drop useless columns
nmr <- dplyr::select(nmr, -c(
                             cohort,
                             animal_id,
                             tag,
                             taghistory,
                             cage,
                             eartag,
                             name,
                             X
                             )
)
# create age for merging and format data so it makes sense
nmr <- dplyr::mutate(nmr,
  age_wk = difftime(date, dob, units = "weeks"),
  date = as.Date(date, "%m%d%Y")
)

# test 1------------------------------------------------------------------------
data1 <- gluc
data2 <- nmr
id <- "idno"
date <- "date"
vars <- c("bw", "lean", "fluid", "fat")
suffixes <- c(".1", ".2")
clean_vars <- "both"
threshold <- c("weeks" = 20)
where <- "both"
clean_vars <- TRUE
# test 2 ------------------------------------------------------------------------
# when there are two different date variables
data1 <- merge_difdate(data1 = gluc, data2 = nmr, id = "idno", date = "date")
data2 <- nmr
id <- "idno"
date <- c("date.1", "date")
vars <- c("bw", "lean", "fluid", "fat")
suffixes <- c(".1", ".2")
threshold <- c("weeks" = Inf)
where <- "both"
clean_vars <- TRUE


merge_difdate <- function(data1,
                          data2,
                          id,
                          date,
                          threshold = c("weeks" = Inf),
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

  # create id and date vars if length id = 1 or 2 -----------------------------
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

  dtm <- merge(dt1,
               dt2,
               by.x = id1,
               by.y = id2,
               all.x = TRUE,
               allow.cartesian = TRUE)

  # dif tells use time difference and direction
  dtm$dif <- difftime(dtm[[date2]], dtm[[date1]], units = names(threshold))

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

  dtm <- dtm[eval(call(name = "order", as.symbol(id1), as.symbol("dif_ef")))]
  eval(
       as.call(
               list(
                    str2lang("data.table::setkey"),
                    x = as.symbol("dtm"),
                    id1,
                    date1
                    )
               )
  )
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

