library(dplyr)

census <- read.csv("data-raw/census.csv") %>%
  rename(animal_id = Animal_ID) %>%
  mutate(dob = as.Date(dob, format = "%m/%d/%Y"))

surv <- read.csv("data-raw/Survival.csv") %>%
  rename(cod = CoD) %>%
  mutate(died = as.Date(Died, "%m/%d/%Y")) %>%
  select(-Died) %>%
  mutate(dead_censor = ifelse(cod == "Found dead", 1,
    ifelse(cod == "Per PI", 1,
      ifelse(cod == "Per Vet", 1,
        ifelse(cod == "Culled", 1,
          ifelse(cod == "DVR or Pathology", 1,
            ifelse(cod == "Culled Per Vet", 1, 0)
          )
        )
      )
    )
  )) %>%
  select(-X)

census_surv <- census %>%
  left_join(surv, by = "tag")
