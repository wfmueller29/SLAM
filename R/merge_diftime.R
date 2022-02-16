#' Merge Diftime
#'
#' Functions that merges two dataframes that have measurements at different ages.
#' It matches the closest measurement in data2 before, after, or before or after the
#' measurement in data1. If a threshold is specified, it will ensure the closest measurement
#' falls within that threshold or it will set it to missing.
#' @param data1 a dataframe that has the ids and ages of measurement that will serve
#' as a reference for matching with data2
#' @param data2 a dataframe that has the ids and ages of measurement we would like to
#' match with the reference id's and dates and data1
#' @param id either a character string, or a character vector of length 2. If the id variable
#' in data1 and data2 have the same name, id is a character string. If the id variable of
#' data1 and data2 are different, then id is a character vector, with the first value being the
#' id variable of data1 and the second being the id variable for data2.
#' @param age either a character string, or a character vector of length 2. If the age variable
#' in data1 and data2 have the same name, age is a character string. If the age variable of
#' data1 and data2 are different, then age is a character vector, with the first value being the
#' age variable of data1 and the second being the age variable for data2.
#' @param threshold a numeric value that provides the difference in time between measurements
#' allowed. The numeric should be in the same units of age.
#' @param vars optional character vector that provides the variable names for which this should
#' be applied over. If specified, for each variable specified by var, only non-missing values will be considered
#' when merging with the closest measurement date.
#' @param where a character string that specifies where to look for the closest observation in data2
#' relative to data1. "before" means that merge_diftime will look before the reference observation in data1. "after" means the same except after. "both"
#' means that merge_ditime will match using observations on either side of the reference observation in data1.
#' @param suffixes specifies the suffix for non-unique variables between the two dataframes. However,
#' even if the age and id variables are unique between the two dataframes, they will be assigned
#' a suffix.
#' @param clean_vars default is true. Will return all data1 columns and remove any duplicate columns from
#' data2 with the exception of the age column.
#'
#' @return a dataframe where each unique subject and age measurement in data1 is matched with the
#' closest aged measurement for that subject in data2.
#'
#' @examples
#'
#' ## Example Merging closest NMR to Glucose
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
#' @importFrom stats as.formula complete.cases
#'
#' @export



merge_diftime <- function(data1, data2, id, age, threshold = Inf, vars = NULL, where = "both", suffixes = c(".1",".2"), clean_vars = TRUE){
  ## manually add suffixes
  names(data1) <- paste0(names(data1), suffixes[1])
  names(data2) <- paste0(names(data2), suffixes[2])

  ## create corresponding id vars
  if(length(id) == 1){
    id1 <- paste0(id, suffixes[1])
    id2 <- paste0(id, suffixes[2])
  } else if(length(id) == 2){
    id1 <- paste0(id[1], suffixes[1])
    id2 <- paste0(id[2], suffixes[1])
  } else{
    stop("id must be length 1 or 2")
  }

  ## create corresponding age vars
  if(length(age) == 1){
    age1 <- paste0(age, suffixes[1])
    age2 <- paste0(age, suffixes[2])
  } else if(length(age) == 2){
    age1 <- paste0(age[1], suffixes[1])
    age2 <- paste0(age[2], suffixes[1])
  } else{
    stop("age must be length 1 or 2")
  }

  ## if vars are specified ensure complete cases for those vars
  if(is.null(vars)){
    data2 <- data2
  }else if(!is.null(vars)){
    vars_suf <- paste0(vars, suffixes[2])
    data2 <- data2[complete.cases(data2[vars_suf]),]
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

  if(clean_vars){
    suf1_regx <- paste0(suffixes[1],"$")
    suf2_regx <- paste0(suffixes[2],"$")
    col1 <- names(data_m)[grepl(suf1_regx, names(data_m))]
    col2 <- names(data_m)[grepl(suf2_regx, names(data_m))]

    col1_clean <- gsub(suf1_regx, replacement = "", x = col1)
    col2_clean <- gsub(suf2_regx, replacement = "", x = col2)

    if(length(age) == 1){
      col1_clean <- gsub(age, replacement = age1, x = col1_clean)
      col2_clean <- gsub(age, replacement = age2, x = col2_clean)
    } else if(length(age) == 2){
      col1_clean <- gsub(age[1], replacement = age1, x = col1_clean)
      col2_clean <- gsub(age[2], replacement = age2, x = col2_clean)
    }

    var_keep <- col2[!col2_clean %in% col1_clean]
    var_keep <- c(col1, var_keep, "dif")

    data_m <- data_m[,var_keep]
    names(data_m)[names(data_m) %in% col1] <- col1_clean
  }

  return(data_m)
}
