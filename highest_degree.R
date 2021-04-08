library(tidyverse)
load(here::here("data", "acs2019.RData"))

acs2019 <- acs2019 %>%
  mutate(highest_deg = case_when(educd < 62 ~ "Less than HS",
                                 educd < 81 ~ "High School",
                                 educd < 101 ~ "Associates",
                                 educd < 114 ~ "Bachelors",
                                 TRUE ~ "Grad"),
         highest_deg = factor(highest_deg),
         incwage = case_when(incwage == 999999 ~ NA_real_,
                             incwage == 999998 ~ NA_real_,
                             TRUE ~ incwage))


acs2019 %>% 
  group_by(highest_deg) %>% 
  summarize(wage = mean(incwage, na.rm = TRUE))
