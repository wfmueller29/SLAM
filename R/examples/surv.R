
# Repeated Measures (Longitudinal) Example
# Lets see how glucose predicts mortaility in SLAM

if (requireNamespace("dplyr", quietly = TRUE)) {
  # Checkout glucose ---------------------------------------------------------
  head(gluc)

  # Create dataframe with everything -------------------------------------------
  # filter mice without date of death
  main <- dplyr::filter(gluc, !is.na(died))
  # create age, age of death, and difference between age and age of death
  main <- dplyr::mutate(main,
    age_wk = as.numeric(difftime(date, dob, units = "weeks")),
    age_wk_death = as.numeric(difftime(died, dob, units = "weeks")),
  )
  # filter mice measured after death because tmerge will throw error
  main <- dplyr::filter(main, age_wk <= age_wk_death)
  # filter mice that were measured same day as death because tmerge with throw an error
  main <- dplyr::filter(main, !(age_wk == age_wk_death))

  # Checkout main --------------------------------------------------------------
  # Table death censor. 0 means death was not natural and 1 means natural deat
  table(main$dead_censor)

  # Checkout main
  head(main)
  # Checkout main NA's
  apply(apply(main, 2, is.na), 2, sum)

  # Now use surv_tmerge --------------------------------------------------------
  main_tmerge <- surv_tmerge(
    data = main,
    id = "idno",
    age = "age_wk",
    age_death = "age_wk_death",
    dead_censor = "dead_censor",
    outcomes = c("gluc")
  )

  # Now lets make a cox model with our now time dependent dataframe ------------
  fit <- surv_cox(
    data = main_tmerge,
    covariates = ~ gluc + age_wk + sex + strain,
    time = "tstart",
    time2 = "tstop",
    death = "death"
  )

  # Now lets extract Hazard Ratios ------------------------------------------
  hrs <- surv_gethr(
    fit = fit,
    vars = c("gluc", "age_wk"),
    names = c("Glucose", "Age (weeks)"),
    ndec = 4
  )

  # Lets look at final HR table
  dplyr::select(hrs$hr_table, final)

  # Lets make predictions on other data ---------------------------------------
  # create new data for 4 mice
  pred_df <- data.frame(
    age_wk = c(40, 80, 20, 100),
    gluc = c(180, 200, 150, 120),
    sex = c("M", "M", "F", "F"),
    strain = c("B6", "HET3", "B6", "HET3")
  )
  # use predict function to get HR for each mouse
  predict(fit, newdata = pred_df, type = "risk")
} else {
  print("Install dplyr to run this example")
}
