df <- tibble(group = c("Kent County", "Grand Valley"), wage = c(41437.05, 53862.98), num = c("$41,437", "$53,863"))
major_graphs <- read_csv("data/major_graphs.csv")


df %>% 
  ggplot(aes(x = group, y = wage, fill = group))+
  geom_col(width = .5, color = "black")+
  geom_text(aes(label = num), position = position_dodge(width = .9), vjust = -.5)+
  scale_fill_manual(values = c("#0065a4", "#a46200"))+
  scale_y_continuous(labels = scales::dollar, expand = expansion(mult = c(0,.05)))+
  labs(title = "Average Wage of Grand Valley Gradutes and Graduates in Kent County",
       subtitle = "Bachelor's degree, income less than $200k",
       x = NULL,
       y = "Average Wage",
       caption = "Source: 2019 ACS, 2020 EMSI")+
  theme_classic()+
  theme(legend.position = "none")
  

major_graphs %>% 
  ggplot(aes(x = group, y = wage, fill = area))+
  geom_col(position = "dodge", color = "black")+
  geom_text(aes(label = label), position = position_dodge(width = .9), vjust = -.5)+
  scale_fill_manual(values = c("#0065a4", "#a46200"))+
  scale_y_continuous(labels = scales::dollar, expand = expansion(mult = c(0,.05)))+
  labs(title = "Average wage of Grand Valley graduates and Graduates in Kent County by Major",
       subtitle = "Bachelor's degree, income less than $200k",
       x = NULL,
       y = "Average Wage",
       caption = "Source: 2019 ACS, 2020 EMSI")+
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank())
