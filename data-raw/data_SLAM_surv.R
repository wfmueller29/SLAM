library(dplyr)
data_SLAM_surv <- read.csv("data-raw/Survival.csv") %>%
  rename(cod = CoD) %>%
  mutate(died = as.Date(Died, "%m/%d/%Y")) %>%
  select(-Died) %>%
  mutate(dead_censor = ifelse(cod == "Found dead", 1,
                              ifelse(cod == "Per PI", 1,
                                     ifelse(cod == "Per Vet", 1,
                                            ifelse(cod == "Culled", 1,
                                                   ifelse(cod == "DVR or Pathology", 1,
                                                          ifelse(cod == "Culled Per Vet", 1,0)))))))


str(data_SLAM_surv)
unique(data_SLAM_surv$cod)
apply(apply(data_SLAM_surv,2,is.na),2,sum)


usethis::use_data(data_SLAM_census, overwrite = TRUE)
