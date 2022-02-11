data_SLAM_surv <- read.csv("data-raw/NMR.csv") %>%
  mutate(idno = as.numeric(idno),
         date = as.Date(date, "%m/%d/%Y"),
         bw = as.numeric(bw))

str(data_SLAM_surv)

usethis::use_data(data_SLAM_surv, overwrite = TRUE)
