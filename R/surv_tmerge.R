#' Tmerge Long Data
#'
#' This will take data in long form and create a tmerge dataframe
#' that can be used in time dependent survival analysis
#'
#' @param data dataframe in long form for outcome measurements
#' @param id character string specifying subject in data
#' @param age character string specifying age in data
#' @param age_death character string specifiying age of death in data
#' @param death_censor character string specifying censor in data.
#' A death is represented by 1 and censorship is represented by 0
#' @param outcomes vector or list of strings specifying the outcome variables
#' at each timepoint
#'
#' @return outputs dataframe with tstart, tstop, death that can be used for time
#' dependent survival analysis
#'
#' @examples
#' # Repeated Measures (Longitudinal) Example
#' # Lets see how glucose predicts mortaility in SLAM
#'
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   # Checkout dataframes --------------------------------------------------------
#'   # Checkout census
#'   head(data_SLAM_census)
#'
#'   # Checkout glucose
#'   head(data_SLAM_gluc)
#'
#'   # Checkout survival data
#'   head(data_SLAM_surv)
#'
#'   # Create dataframe with everything -------------------------------------------
#'   # drop lactate to simplify
#'   main <- dplyr::select(data_SLAM_gluc, -lact)
#'   # obtain census info for dob
#'   main <- dplyr::left_join(main, data_SLAM_census, by = "idno")
#'   # obtain survival info for dod
#'   main <- dplyr::left_join(main, data_SLAM_surv, by = "tag")
#'   # filter mice without date of death
#'   main <- dplyr::filter(main, !is.na(died))
#'   # create age, age of death, and difference between age and age of death
#'   main <- dplyr::mutate(main,
#'     age_wk = as.numeric(difftime(date, dob, units = "weeks")),
#'     age_wk_death = as.numeric(difftime(died, dob, units = "weeks")),
#'     dif = age_wk_death - age_wk
#'   )
#'   # filter mice measured after death because tmerge will throw error
#'   main <- dplyr::filter(main, age_wk <= age_wk_death)
#'   # filter mice that were measured same day as death because tmerge with throw an error
#'   main <- dplyr::filter(main, !(age_wk == age_wk_death))
#'
#'   # Checkout main --------------------------------------------------------------
#'   # Table death censor. 0 means death was not natural and 1 means natural deat
#'   table(main$dead_censor)
#'
#'   # Checkout main
#'   head(main)
#'   # Checkout main NA's
#'   apply(apply(main, 2, is.na), 2, sum)
#'
#'   # Now use surv_tmerge --------------------------------------------------------
#'   main_tmerge <- surv_tmerge(
#'     data = main,
#'     id = "idno",
#'     age = "age_wk",
#'     age_death = "age_wk_death",
#'     death_censor = "dead_censor",
#'     outcomes = c("gluc")
#'   )
#'
#'   # Now lets make a cox model with our now time dependent dataframe ------------
#'   fit <- surv_cox(
#'     data = main_tmerge,
#'     covariates = ~ gluc + age_wk + sex + strain,
#'     time = "tstart",
#'     time2 = "tstop",
#'     death = "death"
#'   )
#'
#'   # Now lets extract Hazard Ratios ------------------------------------------
#'   hrs <- surv_gethr(
#'     fit = fit,
#'     vars = c("gluc", "age_wk"),
#'     names = c("Glucose", "Age (weeks)"),
#'     ndec = 4
#'   )
#'
#'   # Lets look at final HR table
#'   dplyr::select(hrs$hr_table, final)
#'
#'   # Lets make predictions on other data ---------------------------------------
#'   # create new data for 4 mice
#'   pred_df <- data.frame(
#'     age_wk = c(40, 80, 20, 100),
#'     gluc = c(180, 200, 150, 120),
#'     sex = c("M", "M", "F", "F"),
#'     strain = c("B6", "HET3", "B6", "HET3")
#'   )
#'   # use predict function to get HR for each mouse
#'   predict(fit, newdata = pred_df, type = "risk")
#' } else {
#'   print("Install dplyr to run this example")
#' }
#' @importFrom survival tmerge
#'
#' @importFrom rlang call2
#'
#' @seealso \link[survival]{tmerge}
#'
#' @export


surv_tmerge <- function(data, id, age, age_death, death_censor, outcomes) {
  # Create baseline data -------------------------------------------------------

  # Reoder Dataset by Id and Age
  data_baseline <- data[order(data[[id]], data[[age]]), , drop = FALSE]

  ## take first observation for each id
  data_baseline <- data_baseline[!duplicated(data_baseline[[id]]), , drop = FALSE]

  # First tmerge ---------------------------------------------------------------
  # Create call for first tmerge
  event <- call("event", as.symbol(age_death), as.symbol(death_censor))
  cl_tmerge1 <- rlang::call2("tmerge",
    data1 = as.symbol("data_baseline"),
    data2 = as.symbol("data_baseline"),
    id = as.symbol(id),
    tstart = as.symbol(age),
    tstop = as.symbol(age_death),
    death = event,
    .ns = "survival"
  )

  # Call first tmerge, this will create a tstart, tstop, and death column
  # tstart is the time of first observation
  # tstop is the time of last observation
  # death is weather or not the individual died following the time of last observation
  data1 <- eval(cl_tmerge1)

  # Second tmerge --------------------------------------------------------------
  # Create call for second tmerge,
  # This will fill in all the times between the first and last observation for each subject
  # and make death column 0
  # Create tmerge ... arguments and store them as args. age is included so it will
  # be constant
  args <- lapply(c(outcomes, age), function(outcome) {
    call("tdc", as.symbol(age), as.symbol(outcome))
  })
  # name args
  names(args) <- outcomes
  # Create call
  cl_tmerge2 <- rlang::call2("tmerge",
    data1 = as.symbol("data1"),
    data2 = as.symbol("data"),
    id = as.symbol(id),
    !!!args,
    .ns = "survival"
  )

  # Call second tmerge
  data2 <- eval(cl_tmerge2)

  return(data2)
}
