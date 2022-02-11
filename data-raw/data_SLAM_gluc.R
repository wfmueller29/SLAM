## code to prepare `DATASET` dataset goes here
library(dplyr)

data_SLAM_gluc <- read.csv("data-raw/Glucose_Lactate.csv") %>%
  rename(bleeder = Bleeder, gluc = Glucose, lact = Lactate, notes = Stress.Notes) %>%
  mutate(date = as.Date(date, "%m/%d/%Y"),
         gluc = as.numeric(gluc),
         lact = as.numeric(lact)) %>%
  filter(!is.na(gluc)) %>%
  select(-bleeder, -notes)

str(data_SLAM_gluc)
apply(apply(data_SLAM_gluc,2,is.na),2,sum)


usethis::use_data(data_SLAM_gluc, overwrite = TRUE)

