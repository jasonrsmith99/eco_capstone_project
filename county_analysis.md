average wage select counties
================

``` r
library(tidyverse)
```

``` r
load(here::here("data", "acs2019.RData"))
```

joining fips codes into one code so we can use the codes given by fact
finder

``` r
counties <- acs2019 %>% 
  unite("state_county", statefip, countyfip, sep = "", na.rm = TRUE, remove = FALSE)
```

filtering counties of interest

``` r
counties <- counties %>% 
  filter(state_county %in% c(2681, 39153, 183, 1743, 5525))

counties$state_county <- factor(counties$state_county, labels = c("Dupage", "Allen", "Kent", "Summit", "Dane"))

counties <- counties %>% 
  mutate(highest_deg = case_when(educd < 62 ~ "Less than HS",
                                 educd < 81 ~ "High School",
                                 educd < 101 ~ "Associates",
                                 educd < 114 ~ "Bachelors",
                                 TRUE ~ "Grad"),  #will need to reconsider these blocks
         highest_deg = factor(highest_deg),
         incwage = case_when(incwage == 999999 ~ NA_real_,
                             incwage == 999998 ~ NA_real_,
                             TRUE ~ incwage))

counties <- counties %>% 
  rename(county = state_county)
```

As a reminder, table below shows what city a county is a proxy for

| County Proxy | City             |
|--------------|------------------|
| Dupage       | Naperville, IL   |
| Allen        | Fort Wayne, IN   |
| Kent         | Grand Rapids, MI |
| Summit       | Akron, OH        |
| Dane         | Madison, WI      |

summary table. The data is so skewed right this probably isn’t very
useful. Maybe do weighted median?

``` r
counties %>% 
  group_by(county) %>% 
  summarize(mean_wage = weighted.mean(incwage, perwt, na.rm = TRUE), n = n()) %>% 
  knitr::kable()
```

| county | mean\_wage |    n |
|:-------|-----------:|-----:|
| Dupage |   45681.55 | 9374 |
| Allen  |   29314.59 | 3872 |
| Kent   |   32517.88 | 4465 |
| Summit |   31972.94 | 4875 |
| Dane   |   40938.63 | 3792 |

graphing

``` r
#when am I supposed to use weights
counties %>% 
  ggplot(aes(x = incwage))+
  geom_boxplot(aes(weight = perwt, color = county))+
  labs(x = "Income",
       color = "County",
       title = "Income by county",
       subtitle = "weighted?")+
  scale_x_continuous(labels = scales::dollar,
                     expand = expansion(mult = c(.01, .06)))+
  ggpubr::theme_pubr()+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
```

    ## Warning: Removed 4769 rows containing non-finite values (stat_boxplot).

![](county_analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

ANOVA to see if there is a significant difference between the counties
Normality condition technically violated but we can maybe rely on CLT?
Could also remove outliers or focus on IQR. Or use KW test. There are
also unequal variances, unsure if largest variance is more than 4 times
the smallest. Regardless, Welch’s Anova from Rstatix may be the way to
go.

``` r
anova <- aov(incwage ~ county, weights = perwt, data = counties)
summary(anova)
```

    ##                Df    Sum Sq   Mean Sq F value Pr(>F)    
    ## county          4 1.008e+14 2.520e+13   60.74 <2e-16 ***
    ## Residuals   21604 8.962e+15 4.148e+11                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 4769 observations deleted due to missingness

We conclude from the ANOVA that there is a significant difference in
earnings between the counties. We can use Tukey to find out which
differences specifically are different. (Could use Bonferroni to be more
conservative)

``` r
TukeyHSD(anova)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = incwage ~ county, data = counties, weights = perwt)
    ## 
    ## $county
    ##                      diff        lwr        upr     p adj
    ## Allen-Dupage  -16366.9641 -20080.153 -12653.775 0.0000000
    ## Kent-Dupage   -13163.6714 -16687.343  -9640.000 0.0000000
    ## Summit-Dupage -13708.6050 -17088.649 -10328.561 0.0000000
    ## Dane-Dupage    -4742.9233  -8427.247  -1058.600 0.0040751
    ## Kent-Allen      3203.2927  -1068.558   7475.143 0.2442808
    ## Summit-Allen    2658.3591  -1495.813   6812.531 0.4058392
    ## Dane-Allen     11624.0408   7218.739  16029.343 0.0000000
    ## Summit-Kent     -544.9335  -4530.613   3440.745 0.9958893
    ## Dane-Kent       8420.7482   4173.964  12667.532 0.0000005
    ## Dane-Summit     8965.6817   4837.290  13094.073 0.0000000

**The significant differences at 5%**

-   Allen - Dupage (Dupage pays more)
-   Kent - Dupage (Dupage pays more)
-   Summit - Dupage (Dupage pays more)
-   Dane - Dupage (Dupage pays more)
-   Dane - Allen (Dane pays more)
-   Dane - Kent (Dane pays more)
-   Dane - Summit (Dane pays more)

The cities may also not have been chosen well to be compared to Kent
County (which is what we’re really interested in)

## Just looking at people with Bachelors

``` r
undergrad <- counties %>% 
  filter(highest_deg == "Bachelors")
```

``` r
undergrad %>% 
  group_by(county) %>% 
  summarize(mean_wage = weighted.mean(incwage, perwt, na.rm = TRUE), n = n()) %>% 
  knitr::kable()
```

| county | mean\_wage |    n |
|:-------|-----------:|-----:|
| Dupage |   58363.92 | 2280 |
| Allen  |   46219.66 |  562 |
| Kent   |   48795.92 |  805 |
| Summit |   53221.90 |  861 |
| Dane   |   49225.89 |  894 |

``` r
undergrad %>% 
  ggplot(aes(x = incwage))+
  geom_boxplot(aes(weight = perwt, color = county))+
  labs(x = "Income",
       color = "County",
       title = "Income by county",
       subtitle = "weighted? Bachelor's degree only")+
  scale_x_continuous(labels = scales::dollar,
                     expand = expansion(mult = c(.01, .06)))+
  ggpubr::theme_pubr()+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
```

![](county_analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Normality and variance assumptions likely violated. KW or Welch anova
may be best. but I’ll continue with ANOVA for now.

``` r
under_anova <- aov(incwage ~ county, weights = perwt, data = undergrad)
summary(under_anova)
```

    ##               Df    Sum Sq   Mean Sq F value   Pr(>F)    
    ## county         4 1.258e+13 3.146e+12    6.03 7.73e-05 ***
    ## Residuals   5397 2.815e+15 5.217e+11                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

significant difference between counties tukey to do pair-wise

``` r
TukeyHSD(under_anova)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = incwage ~ county, data = undergrad, weights = perwt)
    ## 
    ## $county
    ##                      diff        lwr       upr     p adj
    ## Allen-Dupage  -12144.2674 -21409.037 -2879.498 0.0032301
    ## Kent-Dupage    -9568.0006 -17633.298 -1502.704 0.0106629
    ## Summit-Dupage  -5142.0253 -13011.090  2727.039 0.3835533
    ## Dane-Dupage    -9138.0329 -16900.959 -1375.107 0.0115861
    ## Kent-Allen      2576.2667  -8237.483 13390.016 0.9666693
    ## Summit-Allen    7002.2420  -3665.951 17670.435 0.3788557
    ## Dane-Allen      3006.2345  -7583.911 13596.380 0.9379639
    ## Summit-Kent     4425.9753  -5218.889 14070.840 0.7205417
    ## Dane-Kent        429.9678  -9128.497  9988.432 0.9999489
    ## Dane-Summit    -3996.0075 -13389.484  5397.469 0.7737387

**Significant differences at 5%**

-   Allen - Dupage (Dupage pays more)
-   Kent - Dupage (Dupage pays more)
-   Dane Dupage (Dupage pays more)

## Conclusion

At least with the counties chosen, Grand Rapids doesn’t pay
significantly less than most of the counties. The counties chosen were
intended to be similar to Grand Rapids but there may be value in
including larger cities nearby like Detroit or Chicago. It may also be
worthwhile to include counties outside of the midwest as difference in
pay may be more of a regional thing.

It’s also important to note that Dupage County has quite a few more
respondents than the other counties. The means are weighted so that
shouldn’t be a large problem but it’s good to note.

We should also make age-earnings profiles for the counties to see if
there’s a difference on age. We could do that with ANOVA but that sounds
painful… Maybe with some form of factor (early/mid/late career).

We’ll also want to break it down by majors
