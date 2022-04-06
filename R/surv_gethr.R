#' Get Hazard Ratio from Cox Model
#'
#' The purpose of this function is so that the user can easily extract hazard
#' ratios, hazard ratio confidence intervals, and hazard ratio pvalues easily.
#' This function also outputs Hazard Ratios that can easily be added plots
#' @param fit coxph model
#' @param vars character vector including the covariates fo the cox model that
#' we would like to extract hazard ratios for. Note: if a covariate was
#' wrapped, for example tt(age_wk), the wrapping and the variable should be
#' included
#' @param names character vector of same length as vars that provides
#' formal names for the variables in place of variable name.
#' @param ndec numeric that specifies the number of decimal places to round
#' the hazard ratios
#' @return
#'  \item{hr_table}{
#'  a dataframe whos rows correspond the variables inputted by the vars
#'  argument and whos names are  determined by the names argument. If the names
#'  argument is not given the names are the same  as the variable names.
#'  This dataframe has the following columns (1) value - provides the hazard
#'  ratio
#'  (2) lower - lower bound of 95% confidence interval for hazard ratio
#'  (3) upper - upper bound of 95% confidence interval for hazard ratio
#'  (4) pval - the p-value for the hazard ratio
#'  (5) final - a character stirng that includes columns 1-4 for table output
#'  (6) with_names - character string like final, but the variable name is
#'  included
#'  }
#'  \item{hr_text}{
#'  a character string that has the with names for each variable separated by
#'  \\n, so that the hazard ratios for vars can be included in a kaplan meier
#'  plot.
#'  }
#'
#' @author William Mueller, Eric J. Shiroma
#'
#' @example R\examples\surv.R
#'
#' @export


surv_gethr <- function(fit, vars, names = NULL, ndec = 4) {
  sum_fit <- summary(fit) ## get summary of fit
  # save coeffecients as dataframe
  coef <- data.frame(sum_fit$coefficients, check.names = F)
  # save confidence interval as dataframe
  conf <- data.frame(sum_fit$conf.int, check.names = F)
  # initialize hr dataframe
  hr <- data.frame()
  # loop through vars
  for (var in vars) {
    # save hr
    value <- format(round(conf[var, "exp(coef)"], ndec), nsmall = 2)
    # save lower HR bound
    lower <- format(round(conf[var, "lower .95"], ndec), nsmall = 2)
    # save upper HR bound
    upper <- format(round(conf[var, "upper .95"], ndec), nsmall = 2)
    # save pval
    pval <- coef[var, "Pr(>|z|)"]

    ## add stars depending on significance
    if (pval < .001) {
      final <- paste0("HR = ", value, " (", lower, ", ", upper, ")***")
    } else if (pval < .005) {
      final <- paste0("HR = ", value, " (", lower, ", ", upper, ")**")
    } else if (pval < 0.050) {
      final <- paste0("HR = ", value, " (", lower, ", ", upper, ")*")
    } else {
      final <- paste0("HR = ", value, " (", lower, ", ", upper, ")")
    }

    ## save stats above to hr dataframe
    hr[var, c("value", "lower", "upper", "pval", "final")] <- c(
      value,
      lower,
      upper,
      pval,
      final
    )
  }

  ## if names are provided, rename variables
  if (!is.null(names)) {
    rownames(hr) <- names
  }

  ## create a column that has the rownames printed before HR
  hr[, "with_names"] <- unlist(lapply(rownames(hr), function(name) {
    el <- paste(name, hr[name, "final"], sep = ": ")
  }))

  ## create text that can be used to easily add to KM plot in place of pval
  one_text <- paste(as.vector(hr[, "with_names"]), collapse = "\n")

  return(list(hr_table = hr, hr_text = one_text))
}
