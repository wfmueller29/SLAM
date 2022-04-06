## code to prepare `DATASET` dataset goes here
library(dplyr)

source_path <- normalizePath("data-raw/read_census_surv.R")
source(source_path)

gluc <- read.csv("data-raw/Glucose_Lactate.csv") %>%
  rename(
    bleeder = Bleeder,
    gluc = Glucose,
    lact = Lactate,
    notes = Stress.Notes
  ) %>%
  mutate(
    date = as.Date(date, "%m/%d/%Y"),
    gluc = as.numeric(gluc),
    lact = as.numeric(lact)
  ) %>%
  filter(!is.na(gluc)) %>%
  select(-bleeder, -notes) %>%
  left_join(census_surv, by = "idno") %>%
  filter(cohort %in% c(1:3))

str(gluc)
apply(apply(gluc, 2, is.na), 2, sum)


usethis::use_data(gluc, overwrite = TRUE)
