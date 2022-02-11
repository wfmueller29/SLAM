#' Impute using missForest
#'
#' This function will allow the user to specify factor variables and variables they do not want
#' in the imputation. It will then create a data matrix to impute using missForest. It will add drop columns
#' back after imputation and relabel the factor variables.
#' @param df the dataframe that we would like to impute NAs.
#' @param df_true optional complete dataframe of the dataframe provided as df argument
#' @param facotrs a character vector of factor variables that we would like specfiy as factors when imputing
#' These factors variables can be numeric or character variables in the df provided
#' @param drop a character vector of variables that will not be included in the imputation.
#' These variables will be added to the outputted dataframe, even though they are not inclued in the imputation.
#' @param ntree number of trees to grow in each forest. Default is set to 500.
#' @return A list of output similar to missForest
#'     \item{ximp}{
#'     a dataframe with no missing values. The resulting dataframe will be of the same size as the original df
#'     provided, but with all of the NA's imputed. However, if there are NA's in the drop columns, these values wil
#'     not be imputed.
#'     }
#'     \item{OOBerror}{
#'     estimated OOB imputation error. For the set of continuous
#'     variables in 'xmis' the NRMSE and for the set of categorical variables
#'     the proportion of falsely classified entries is returned. See Details
#'     for the exact definition of these error measures. If 'variablewise'
#'     is set to 'TRUE' then this will be a vector of length 'p' where 'p' is
#'     the number of variables and the entries will be the OOB error for each
#'     variable separately.
#'     }
#'     \item{error}{
#'     true imputation error. This is only available if 'xtrue'
#'     was supplied. The error measures are the same as for 'OOBerror'.
#'     }
#' @param ... Other arguments to be passed to missForest imputation, besides ntree
#'
#' @export


impute_mf <- function(df, df_true = NULL, factors = NULL, drop = NULL, ntree = 500, ...){
  df_drop <- df[,drop] # save drop variables
  # convert to be factor variables to character, then to factor. This insures no excess factor levels if the variable is originally numeric
  df[,factors] <- lapply(factors, function(factor)as.factor(as.character(df[,factor])))

  dm_prep <- df[!(names(df) %in% drop)] # drop drop variables
  dm_prep <- data.matrix(dm_prep) # create data matrix

  if(!is.null(df_true)){
    # convert to be factor variables to character, then to factor. This insures no excess factor levels if the variable is originally numeric
    df_true[,factors] <- lapply(factors, function(factor)as.factor(as.character(df_true[,factor])))

    dm_true_prep <- df_true[!(names(df_true) %in% drop)] # drop drop variables
    dm_true_prep <- data.matrix(dm_true_prep) # create data matrix
  }

  set.seed(365)

  imp <- missForest::missForest(dm_prep, ntree = ntree, xtrue=dm_true_prep, ...) # impute with missed forest

  imp$ximp <- as.data.frame(imp$ximp)
  imp$ximp <- cbind(imp$ximp, df_drop) # bind imputed dataframe with dropped columns
  imp$ximp[,factors] <- lapply(factors, function(fact){ # use old factor levels to relabel factor variables
    factor(as.character(round(imp$ximp[,fact])), labels = levels(df[,fact]))
  })

  imp$ximp <- imp$ximp

  return(imp)

}
