print(xtable(kent %>% 
  group_by(highest_deg) %>% 
  summarise(mean = weighted.mean(incwage, perwt, na.rm = TRUE),
            sd = w.sd(incwage, perwt),
            n = n())), file = "paper_tables/table2_kent_educ_wage.tex")


print(xtable(summary(county_aov)), file = "paper_tables/table5_cityanova.tex")

print(xtable(summary(pairs(emmeans(county_aov, "county")), adjust = "holm")), file = "paper_tables/table6_pairwisecomp.tex")

print(xtable(summary(counties_majors_aov)), file = "paper_tables/table7_interactanova.tex")

fig1 <- counties %>% 
  ggplot(aes(x = incwage))+
  geom_boxplot(aes(weight = perwt, color = county))+
  scale_x_continuous(labels = scales::dollar)+
  labs(title = "Figure 1: Income by County",
       subtitle = "Income capped at $200,000, weighted",
       x = "Income",
       color = "County",
       caption = "Source: 2019 ACS")+
  theme_classic()+
  theme(axis.text.y = element_blank())

fig1
