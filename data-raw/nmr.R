library(dplyr)

source("data-raw/read_census_surv.R")

nmr <- read.csv("data-raw/NMR.csv") %>%
  mutate(
    idno = as.numeric(idno),
    date = as.Date(date, "%m/%d/%Y"),
    bw = as.numeric(bw)
  ) %>%
  filter(!is.na(idno)) %>%
  select(-Time, -Operator, -Raw.Location) %>%
  left_join(census_surv, by = "idno") %>%
  filter(cohort %in% c(1:3))

str(nmr)
apply(apply(nmr, 2, is.na), 2, sum)

usethis::use_data(nmr, overwrite = TRUE)
