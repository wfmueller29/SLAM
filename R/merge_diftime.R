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
#' when merging with the closest measurment date. See

merge_diftime <- function(data1, data2, id, age, threshold = Inf, vars = NULL, where = "both", suffixes = c(".1",".2")){
  data1 <- gluc
  data2 <- nmr
  id <- "idno"
  age <- "age_wk"
  suffixes <- c(".gluc", ".nmr")
  vars <- c("bw","lean","fat","fluid")
  where <- "before"
  threshold <- 20

  ## manually add suffixes
  names(data1) <- paste0(names(data1), suffixes[1])
  names(data2) <- paste0(names(data2), suffixes[2])

  ## create corresponding id vars
  if(length(id) == 1){
    id1 <- paste0(id, suffixes[1])
    id2 <- paste0(id, suffixes[2])
  } else if(length(id) == 2){
    id1 <- id[1]
    id2 <- id[2]
  } else{
    stop("id must be length 1 or 2")
  }

  ## create corresponding age vars
  if(length(age) == 1){
    age1 <- paste0(age, suffixes[1])
    age2 <- paste0(age, suffixes[2])
  } else if(length(age) == 2){
    age1 <- age[1]
    age2 <- age[2]
  } else{
    stop("age must be length 1 or 2")
  }

  ## if specific vars are not provided merge entire dataframe based on age and idno
  if(is.null(vars)){

    data_m <- merge(data1, data2, by.x = id1, by.y = id2, all.x = TRUE, suffixes = suffixes)

    data_m$dif <- data_m[[age2]] - data_m[[age1]]

    # where to check for closest date
    if(where == "both"){
      data_m <- data_m[order(data_m[[id1]], abs(data_m$dif)), , drop = FALSE]
      data_m <- data_m[!duplicated(data_m[[id1]]), , drop = FALSE]
      ## threshold
      data2_cols <- names(data_m)[grepl(paste0(suffixes[2],"$"), names(data_m))]
      data_m[,data2_cols] <- lapply(data2_cols, function(name){
        ifelse(abs(data_m$dif) > threshold, NA, data_m[,name] )
      })


    } else if(where == "before"){
      data_m <- data_m[data_m$dif <= 0,]
      data_m <- data_m[order(data_m[[id1]], -1*data_m$dif), , drop = FALSE]
      data_m <- data_m[!duplicated(data_m[[id1]]), , drop = FALSE]
      ## threshold
      data2_cols <- names(data_m)[grepl(paste0(suffixes[2],"$"), names(data_m))]
      data_m[,data2_cols] <- lapply(data2_cols, function(name){
        ifelse(data_m$dif*-1 > threshold, NA, data_m[,name] )
      })

    } else if(where == "after"){
      data_m <- data_m[data_m$dif >= 0,]
      data_m <- data_m[order(data_m[[id1]], data_m$dif), , drop = FALSE]
      data_m <- data_m[!duplicated(data_m[[id1]]), , drop = FALSE]
      ## threshold
      data2_cols <- names(data_m)[grepl(paste0(suffixes[2],"$"), names(data_m))]
      data_m[,data2_cols] <- lapply(data2_cols, function(name){
        ifelse(data_m$dif > threshold, NA, data_m[,name] )
      })

    } else{
      stop("where must be both, before, or after")
    }

  } else if(length(vars) >= 1){
    vars <- paste0(vars, suffixes[2])
    data_m <- data1
    for(var in vars){
      data2_sub <- data2[, c(id2, age2, var)]
      data2_sub <- subset(data2_sub, subset = !is.na(var))
      age2_new <- paste0(age2,"_", var)
      dif_new <- paste0("dif_", var)
      names(data2_sub) <- c(id2, age2_new, var)

      data_m <- merge(data_m, data2_sub, by.x = id1, by.y = id2, all.x = TRUE, suffixes = suffixes)

      data_m[[dif_new]] <- data_m[[age2_new]] - data_m[[age1]]

      # where to check for closest date
      if(where == "both"){
        data_m <- data_m[order(data_m[[id1]], abs(data_m[[dif_new]])), , drop = FALSE]
        data_m <- data_m[!duplicated(data_m[[id1]]), , drop = FALSE]
        ## threshold
        data2_cols <- names(data_m)[grepl(paste0(suffixes[2],"$"), names(data_m))]
        data_m[,data2_cols] <- lapply(data2_cols, function(name){
          ifelse(abs(data_m[[dif_new]]) > threshold, NA, data_m[,name] )
        })
      } else if(where == "before"){
        data_m <- data_m[data_m[[dif_new]] <= 0,]
        data_m <- data_m[order(data_m[[id1]], -1*data_m[[dif_new]]), , drop = FALSE]
        data_m <- data_m[!duplicated(data_m[[id1]]), , drop = FALSE]
        ## threshold
        data2_cols <- names(data_m)[grepl(paste0(suffixes[2],"$"), names(data_m))]
        data_m[,data2_cols] <- lapply(data2_cols, function(name){
          ifelse(data_m[[dif_new]]*-1 > threshold, NA, data_m[,name] )
        })
      } else if(where == "after"){
        data_m <- data_m[data_m[[dif_new]] >= 0,]
        data_m <- data_m[order(data_m[[id1]], data_m[[dif_new]]), , drop = FALSE]
        data_m <- data_m[!duplicated(data_m[[id1]]), , drop = FALSE]
        ## threshold
        data2_cols <- names(data_m)[grepl(paste0(suffixes[2],"$"), names(data_m))]
        data_m[,data2_cols] <- lapply(data2_cols, function(name){
          ifelse(data_m[[dif_new]] > threshold, NA, data_m[,name] )
        })

      }  else{
        stop("where must be both, before, or after")
      }
    }
  }
}
