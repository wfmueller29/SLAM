library(dplyr)
census <- read.csv("data-raw/census.csv") %>%
  rename(animal_id = Animal_ID) %>%
  mutate(dob = as.Date(dob, format = "%m/%d/%Y"))

str(census)
apply(apply(census, 2, is.na), 2, sum)


usethis::use_data(census, overwrite = TRUE)
