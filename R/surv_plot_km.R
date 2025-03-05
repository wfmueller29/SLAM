#' This function plots KM curves for a categorical variable of interest
#'
#' Calls survminer::ggsurvplot to create KM curves
#'
#' @param data the dataframe that will be plotted
#' @param var a string specifying the variable name to stratify the plots
#' @param age_death a string specifying the age of death variable
#' @param event a string specifying the status of the event 0=alive, 1=dead
#' @param ... other named arguments to pass to ggsurvplot function
#'
#' @return a km ggplot
#'
#' @author William Mueller, Eric Shiroma
#'
#' @export


surv_plot_km <- function(data,
                         var,
                         age_death,
                         event,
                         ...) {
  data[[var]] <- factor(data[[var]])
  if (length(unique(data[[var]])) == 1) {
    hr <- NULL
  } else {
    hr <- surv_kmcox(data, var, age_death = age_death, event = event)
  }
  surv_object <- survival::Surv(time = data[[age_death]], event = data[[event]])
  formula <- as.formula(paste0("surv_object ~ ", var))
  fit <- survminer::surv_fit(formula, data = data)

  args <- list(...)
  default_args <- list(
    conf.int = FALSE,
    pval = hr$text,
    xlab = "Age (Weeks) ",
    ylab = "Survival Probability",
    surv.median.line = "hv",
    legend = "right",
    size = 2,
    #             font.title = c(12, "bold", "black"),
    ggtheme = ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        color = "black",
        size = 16,
        hjust = .5
      ),
      plot.margin = ggplot2::unit(c(2, 4, 25, 2), "pt")
    ),
    ptitle = "",
    pval.size = 3.5,
    font.subtitle = c(8),
    risk.table = FALSE,
    cumevents = FALSE,
    cumcensor = FALSE
  )
  default_args <- default_args[!names(default_args) %in% names(args)]
  args <- c(args, default_args)
  fun <- call("::", as.symbol("survminer"), as.symbol("ggsurvplot"))
  plot_call <- as.call(c(fun,
    fit = as.symbol("fit"),
    data = as.symbol("data"),
    args
  ))
  plot <- eval(plot_call)


  return(list(plot = plot, hr = hr))
}


surv_kmcox <- function(data, var, age_death, event) {
  cselect <- length(unique(data[[var]])) - 1
  cox_form <- as.formula(paste0("surv_object ~ ", var))
  surv_object <- survival::Surv(time = data[[age_death]], event = data[[event]])
  fit <- survival::coxph(cox_form, data = data)

  vars <- names(fit$coefficients)
  hr_results <- SLAM::surv_gethr(fit, vars)

  return(hr_results)
}
