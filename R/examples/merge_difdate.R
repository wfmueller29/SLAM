
# Merging clostest NMR to Glucose ---------------------------------------------

if (requireNamespace("dplyr", quietly = TRUE)) {
  # Checkout data ------------------------------------------------------------
  # Checkout glucose
  head(gluc)

  # Checkout nmr
  head(nmr)

  # create age for merging and format data so it makes sense
  gluc <- dplyr::mutate(gluc,
    age_wk = difftime(date, dob, units = "weeks"),
    date = as.Date(date, "%m%d%Y")
  )

  # Create nmr ---------------------------------------------------------------
  # create age for merging and format data so it makes sense
  nmr <- dplyr::mutate(nmr,
    age_wk = difftime(date, dob, units = "weeks"),
    date = as.Date(date, "%m%d%Y")
  )

  # Use merge_diftime --------------------------------------------------------
  gluc_nmr <- merge_difdate(
    data1 = gluc,
    data2 = nmr,
    id = "idno",
    date = "date",
    vars = c("bw", "lean", "fluid", "fat"),
    clean_vars = TRUE
  )

  # Checkout results
  head(gluc_nmr)
} else {
  message("Install dplyr to run this example")
}
