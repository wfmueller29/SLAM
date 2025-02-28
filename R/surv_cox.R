#' Cox Function that can either be time dependent or time independent
#'
#' Function that calls Survival Object and Coxph in one function call.
#' This function can be used if your dataset is cross sectional (one
#' observation per individual) and longitudianl (multiple observations per
#' individual). It can also be used if your hazard ratios are time
#' independent (hazard is constant with time) or if you hazard ratios are time
#' depedent (hazard changes with time).
#'
#' @param data dataframe that includes the covariates we would like to model.
#' If your dataset is longitudinal, the recommendation is to use
#' SLAM::surv_tmerge to create an interval dataframe.
#' @param covariates on sided formula starting with "~" that provides the
#' formula for the cox model. If the coefficients for your covariates are time
#' dependent, that covariate should be wrapped with the tt() function, for
#' example tt(age_wk). Then in tt argument of this function define the function
#' that will time transform your variable.
#' @param time A character string. In cross sectional data, it should be the
#' name of the variable for age of observation in data. Inlongitudinal datasets
#' created by SLAM::surv_tmerge, this will be "tstart"
#' @param time2 A optional character string only used when working with
#' longitudinal datasets. This string provides the variable that defines theend
#' point of interval in longitudinal datasets. If the dataset is created by
#' SLAM::surv_tmerge, it would be "tstop".
#' @param death A character string. in cross-sectional datasets, this is the
#' variable that determines death censorship, where a natural death is 1 and a
#' censored subject is 0. In longitudinal datasets, this variable specifies
#' whether or not a subject died at the end of the interval. If your
#' longitudinal dataset was created by SLAM::surv_tmerge, this would be "tstop".
#' @param id optional variable that identifies subjects. Only necessary
#' when a subject can have multiple rows in the data, and there is
#' more than one event type.  This variable will normally be found
#' in ‘data’. However, it appears that the survival packge has not yet
#' gotten this subject based cox model to work
#' @param tt a function that defines the time transformation that will be
#' applied when a covariate is wrapped with the tt(). By default tt = NULL.
#' @param type A character string that specifies the type of censoring.
#'
#' @return returns coxph object
#'
#' @example R\examples\surv.R
#'
#' @author William Mueller
#'
#' @seealso \link[survival]{Surv} \link[survival]{coxph}
#'
#' @export

surv_cox <- function(data,
                     covariates,
                     time,
                     time2 = NULL,
                     death,
                     id = NULL,
                     tt = NULL,
                     type = c(
                       "right",
                       "left",
                       "interval",
                       "counting",
                       "interval2",
                       "mstate"
                     )) {
  if (is.null(time2)) {
    surv_object <- survival::Surv(
      time = data[[time]],
      event = data[[death]],
      type = type
    )
  } else {
    surv_object <- survival::Surv(
      time = data[[time]],
      time2 = data[[time2]],
      event = data[[death]]
    )
  }
  cox_form <- stats::as.formula(paste0("surv_object", deparse1(covariates)))

  namespace_call <- call("::", as.symbol("survival"), as.symbol("coxph"))
  args <- list(formula = cox_form, data = as.symbol("data"), id = id, id = tt)
  args <- args[!unlist(lapply(args, is.null))]
  call <- as.call(c(namespace_call, args))
  fit <- eval(call)

  return(fit)
}
