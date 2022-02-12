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
#' ### Repeated Measures (Longitudinal) Example
#' ## Lets see how glucose predicts mortaility in SLAM
#'
#' library(dplyr)
#' library(SLAM)
#'
#' ## Checkout census
#' head(data_SLAM_census)
#'
#' ## Checkout glucose
#' head(data_SLAM_gluc)
#'
#' ## Checkout survival data
#' head(data_SLAM_surv)
#'
#' ## Create dataframe with everything
#' main <- data_SLAM_gluc %>%
#'   select(-lact) %>% ## drop lactate
#'   left_join(data_SLAM_census, by = "idno") %>% ## merge with census
#'   left_join(data_SLAM_surv, by = "tag") %>% ## merge with survival
#'   filter(!is.na(died)) %>% ## filter mice that have not died
#'   mutate(age_wk = as.numeric(difftime(date, dob, units = "weeks")),  ## create age from date and dob
#'          age_wk_death = as.numeric(difftime(died, dob, units = "weeks")), ## create age_wk_death from dob and died
#'          dif = age_wk_death - age_wk) %>%## Create dif, the time between when the mouse was measured and it died
#'   filter(age_wk <= age_wk_death) %>%## filter mice that were measured after their death
#'   filter(!(age_wk == age_wk_death)) ## filter mice that were measured same day as death
#'
#' ## Table death censor. 0 means death was not natural and 1 means natural deat
#' table(main$dead_censor)
#'
#' ## Checkout main
#' head(main)
#' ## Checkout main NA's
#' apply(apply(main,2,is.na),2,sum)
#'
#' ### Now use surv_tmerge
#' main_tmerge <- surv_tmerge(data = main, id = "idno", age = "age_wk", age_death = "age_wk_death", death_censor = "dead_censor", outcomes = c("gluc"))
#'
#' ### Now lets make a cox model with our now time dependent dataframe
#' fit <- surv_cox(data = main_tmerge, covariates = ~gluc+age_wk+sex+strain, time = "tstart", time2 = "tstop", death = "death")
#'
#' ### Now lets extract Hazard Ratios
#' hrs <- surv_gethr(fit, c("gluc", "age_wk"), names = c("Glucose", "Age (weeks)"), ndec = 4)
#'
#' ## Lets look at final HR table
#' hrs$hr_table %>%
#'   select(final)
#'
#' @importFrom survival tmerge
#'
#' @importFrom rlang call2
#'
#' @seealso \link[survival]{tmerge}
#'
#' @export


surv_tmerge <- function(data, id, age, age_death, death_censor, outcomes){
  ## Reoder Dataset by Id and Age
  data_baseline <- data[order(data[[id]], data[[age]]), , drop = FALSE]

  ## take first observation for each id
  data_baseline <- data_baseline[!duplicated(data_baseline[[id]]), , drop = FALSE]


  ## Create call for first tmerge
  event <- call("event", as.symbol(age_death), as.symbol(death_censor))
  cl_tmerge1 <- rlang::call2("tmerge",
                 data1 = as.symbol("data_baseline"),
                 data2 = as.symbol("data_baseline"),
                 id = as.symbol(id),
                 tstart = as.symbol(age),
                 tstop = as.symbol(age_death),
                 death = event,
                 .ns = "survival")

  ## Call first tmerge, this will create a tstart, tstop, and death column
    # tstart is the time of first observation
    # tstop is the time of last observation
    # death is weather or not the individual died following the time of last observation
  data1 <- eval(cl_tmerge1)

  ## Create call for second tmerge,
  ## This will fill in all the times between the first and last observation for each subject and make death column 0
    # Create tmerge ... arguments and store them as args
  args <- lapply(outcomes, function(outcome){
    call("tdc", as.symbol(age), as.symbol(outcome))
  })
    # name args
  names(args) <- outcomes
  ## Create call
  cl_tmerge2 <- rlang::call2("tmerge",
                             data1 = as.symbol("data1"),
                             data2 = as.symbol("data"),
                             id = as.symbol(id),
                             !!!args,
                             .ns = "survival")

  ## Call second tmerge
  data2 <- eval(cl_tmerge2)

  return(data2)
}



