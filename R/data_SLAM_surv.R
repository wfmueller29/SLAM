#' SLAM Survival Dataset
#'
#' A dataset that contains the age of death and cause of death within the SLAM study
#'
#' @docType data
#' @usage data(data_SLAM_surv)
#' @format a dataframe 1775 obs and 4 variables
#' \describe{
#'   \item{tag}{unique identifier of each mouse}
#'   \item{cod}{Cause of death for each mouse}
#'   \item{died}{date of death for each mouse}
#'   \item{dead_censor}{indicates whether the mouse died a natural death (1) or
#'   if the mouse should be censored (0)}
#' }
"data_SLAM_surv"
