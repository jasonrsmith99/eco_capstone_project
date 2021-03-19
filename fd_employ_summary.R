library(tidyverse)
library(xtable)

load(here::here("data", "fd_employ.RData"))

#filter by undergraduate students
fd_employ <- fd_employ %>% 
  filter(level == "Undergraduate")

#top majors
fd_employ %>% 
  group_by(major) %>% 
  summarise(n = n()) %>% 
  slice_max(order_by = n, n = 10) %>% 
  xtable(caption = "Majors of respondents") %>% 
  print(file = "output/common_majors.tex")

#make employment status either employed or unemployed
fd_employ$employstatus <- fct_other(fd_employ$employstatus, drop = c("Seeking employment", "Not seeking employment or continuing education"),
          other_level = "Unemployed")

fd_employ$employstatus <- fct_other(fd_employ$employstatus, drop = c("Employed (Full or Part) or Military", "Enrolled or Planning to Enroll in Continue Ed",
                                                                     "Participating in a volunteer or service program"), other_level = "Employed")

# majors with highest employment rates (at least 100 responses of employment question)
fd_employ %>%
  filter(!is.na(employstatus)) %>% 
  group_by(major, employstatus) %>% 
  summarize(n = n()) %>%
  mutate(freq = (n / sum(n)) * 100) %>%
  ungroup() %>% 
  filter(employstatus == "Employed" & n >= 100) %>% 
  select(major, freq, n) %>%
  slice_max(order_by = freq, n = 10) %>%
  mutate(freq = round(freq, 1),
         freq = paste(freq, "%", sep = "")) %>% 
  rename(`Employment Rate` = freq) %>% 
  xtable(caption = "Majors with highest employment rate (at least 100 responses)") %>% 
  print(file = "output/top_emp.tex")

