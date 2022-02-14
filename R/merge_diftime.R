#' Merge Diftime
#'
#' Functions that merges two dataframes that have measurements at different ages.
#' It matches the closest measurement in data2 before, after, or before or after the
#' measurement in data1. If a threshold is specified
#' @param data1 a dataframe that has the ids and ages of measurement that will serve
#' as a reference for matching with data2
#' @param data2 a dataframe that has the ids and ages of measurement we would like to
#' match with the reference id's and dates and data1
#' @param id either a character string, or a character vector of length 2. If the id variable
#' in data1 and data2 have the same name, id is a character string. If the id variable of
#' data1 and data2 are different, then id is a character vector, with the first value being the
#' id variable of data1 and the seoncd being the id variable for data2.
#' @param age either a character string, or a character vector of length 2. If the age variable
#' in data1 and data2 have the same name, age is a character string. If the age variable of
#' data1 and data2 are different, then age is a character vector, with the first value being the
#' age variable of data1 and the seoncd being the age variable for data2.
#' @param threshold a numeric value that provides the difference in time between measurements
#' allowed. The numeric should be in the same units of age.
#' @param vars optional character vector that provides the variable names for which this should
#' be applied over. If specified, for each variable specified by var, only non-na value will be considered
#' when merging with the closest measurment date.
#' @param suffixes
#'
#' @return a dataframe where each unique subject and age measurement in data1 is matched with the
#' closest aged measurement for that subject in data2.
#'
#' @examples
#'
#' ## Example Merging clostest NMR to Glucose
#'
#' #load libs
#' library(SLAM)
#' library(dplyr)
#'
#' ## Checkout census
#' head(data_SLAM_census)
#'
#' ## Checkout glucose
#' head(data_SLAM_gluc)
#'
#' ## Checkout nmr
#' head(data_SLAM_nmr)
#'
#' ## Create gluc
#' gluc <- data_SLAM_gluc %>%
#'   left_join(data_SLAM_census, by = "idno") %>%
#'   select(-c(lact, cohort, animal_id, tag, taghistory, cage, eartag, name, X)) %>%
#'   mutate(age_wk = difftime(date, dob, units = "weeks"),
#'          date = as.Date(date, "%m%d%Y"))
#'
#' ## Create nmr
#' nmr <- data_SLAM_nmr %>%
#'   left_join(data_SLAM_census, by = "idno") %>%
#'   select(-c(cohort, animal_id, tag, taghistory, cage, eartag, name, X))%>%
#'   mutate(age_wk = difftime(date, dob, units = "weeks"),
#'          date = as.Date(date, "%m%d%Y"))
#'
#' gluc_nmr <- merge_diftime(data1 = gluc,
#'                           data2 = nmr,
#'                           id = "idno",
#'                           age = "age_wk",
#'                           vars = c("bw","lean","fluid","fat"))
#'
#' ## Checkout results
#' head(gluc_nmr)
#'
#' @export



merge_diftime <- function(data1, data2, id, age, threshold = Inf, vars = NULL, where = "both", suffixes = c(".1",".2")){
  ## create corresponding id vars
  if(length(id) == 1){
    id1 <- paste0(id, suffixes[1])
    id2 <- paste0(id, suffixes[2])
    names(data1)[names(data1) == id] <- id1
    names(data2)[names(data2) == id] <- id2
  } else if(length(id) == 2){
    id1 <- paste0(id[1], suffixes[1])
    id2 <- paste0(id[2], suffixes[1])
    names(data1)[names(data1) == id[1]] <- id1
    names(data2)[names(data2) == id[2]] <- id2
  } else{
    stop("id must be length 1 or 2")
  }

  ## create corresponding age vars
  if(length(age) == 1){
    age1 <- paste0(age, suffixes[1])
    age2 <- paste0(age, suffixes[2])
    names(data1)[names(data1) == age] <- age1
    names(data2)[names(data2) == age] <- age2
  } else if(length(age) == 2){
    age1 <- paste0(age[1], suffixes[1])
    age2 <- paste0(age[2], suffixes[1])
    names(data1)[names(data1) == age[1]] <- age1
    names(data2)[names(data2) == age[2]] <- age2
  } else{
    stop("age must be length 1 or 2")
  }

  ## if vars are specified ensure complete cases for those vars
  if(is.null(vars)){
    data2 <- data2
  }else if(!is.null(vars)){
    data2 <- data2[complete.cases(data2[vars]),]
  }


  data_m <- merge(data1, data2, by.x = id1, by.y = id2, all.x = TRUE, suffixes = suffixes)

  data_m$dif <- data_m[[age2]] - data_m[[age1]]


  # where to check for closest date
  if(where == "both"){
    data_m <- data_m[order(data_m[[id1]], abs(data_m$dif)), , drop = FALSE]
    df_dups <- data_m[c(id1,age1)]
    data_m <- data_m[!duplicated(df_dups), , drop = FALSE]
    ## threshold
    data2_cols <- names(data_m)[grepl(paste0(suffixes[2],"$"), names(data_m))]
    data_m[,data2_cols] <- lapply(data2_cols, function(name){
      ifelse(abs(data_m$dif) > threshold, NA, data_m[,name] )
    })


  } else if(where == "before"){
    data_m <- data_m[data_m$dif <= 0,]
    data_m <- data_m[order(data_m[[id1]], -1*data_m$dif), , drop = FALSE]
    df_dups <- data_m[c(id1,age1)]
    data_m <- data_m[!duplicated(df_dups), , drop = FALSE]
    ## threshold
    data2_cols <- names(data_m)[grepl(paste0(suffixes[2],"$"), names(data_m))]
    data_m[,data2_cols] <- lapply(data2_cols, function(name){
      ifelse(data_m$dif*-1 > threshold, NA, data_m[,name] )
    })

  } else if(where == "after"){
    data_m <- data_m[data_m$dif >= 0,]
    data_m <- data_m[order(data_m[[id1]], data_m$dif), , drop = FALSE]
    df_dups <- data_m[c(id1,age1)]
    data_m <- data_m[!duplicated(df_dups), , drop = FALSE]
    ## threshold
    data2_cols <- names(data_m)[grepl(paste0(suffixes[2],"$"), names(data_m))]
    data_m[,data2_cols] <- lapply(data2_cols, function(name){
      ifelse(data_m$dif > threshold, NA, data_m[,name] )
    })

  } else{
    stop("where must be both, before, or after")
  }

  return(data_m)
}
