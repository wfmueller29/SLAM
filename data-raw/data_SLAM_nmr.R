library(dplyr)
data_SLAM_nmr <- read.csv("data-raw/NMR.csv") %>%
  mutate(idno = as.numeric(idno),
         date = as.Date(date, "%m/%d/%Y"),
         bw = as.numeric(bw)) %>%
  filter(!is.na(idno)) %>%
  select(-Time, -Operator, -Raw.Location)

str(data_SLAM_nmr)
apply(apply(data_SLAM_nmr,2,is.na),2,sum)

usethis::use_data(data_SLAM_nmr, overwrite = TRUE)
