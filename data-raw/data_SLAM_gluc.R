## code to prepare `DATASET` dataset goes here
library(dplyr)

data_SLAM_gluc <- read.csv("data-raw/Glucose_Lactate.csv") %>%
  rename(bleeder = Bleeder, gluc = Glucose, lact = Lactate, notes = Stress.Notes) %>%
  mutate(date = as.Date(date, "%m/%d/%Y"),
         gluc = as.numeric(gluc),
         lact = as.numeric(lact))

str(data_SLAM_gluc)

usethis::use_data(data_SLAM_gluc, overwrite = TRUE)

