library(dplyr)
data_SLAM_census <- read.csv("data-raw/census.csv") %>%
  rename(animal_id = Animal_ID) %>%
  mutate(dob = as.Date(dob, format = "%m/%d/%Y"))

str(data_SLAM_census)

usethis::use_data(data_SLAM_census, overwrite = TRUE)
