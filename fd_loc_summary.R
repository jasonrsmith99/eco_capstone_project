library(tidyverse)
library(xtable)

load(here::here("data", "fd_loc.RData"))


#most common employer location of recent undergraduates
fd_loc %>% 
  filter(!is.na(employerlocation) & employerlocation != "Unknown" & level == "Undergraduate") %>%
  group_by(employerlocation) %>% 
  summarise(n = n()) %>% 
  slice_max(order_by = n, n = 10) %>% 
  xtable(caption = "Most Common Employer Locations of Recent Undergradutes") %>% #not sure how I feel about the term 'recent undergraduate' since the data spans many years
  print(file = "output/common_states.tex")


#which majors have the most graduates working for employers out of state

fd_loc %>%
  filter(!is.na(employerlocation) & employerlocation != "Unknown" & level == "Undergraduate") %>% 
  mutate(out_state = case_when(employerlocation != "Michigan" ~ 1,
                               TRUE ~ 0)) %>% 
  group_by(major) %>% 
  summarise(out_state = sum(out_state)) %>% 
  slice_max(order_by = out_state, n = 10) %>% 
  rename(`Employer out of State` = out_state) %>% 
  xtable(caption = "Majors with the most undergraduates employed by out of state firms") %>% 
  print(file = "output/out_state_major.tex")


#probably worth lumping the majors into larger groups like business, STEM....
  