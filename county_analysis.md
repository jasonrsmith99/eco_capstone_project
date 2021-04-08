average wage select counties
================

``` r
library(tidyverse)
library(rstatix)
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

We’ll preform Welch’s ANOVA to determine if there is a difference in
wages between the counties. We use Welch’s to control for
hetroskedasticity. We’ll assume normality comes from the Central Limits
Theorem.

``` r
welch_anova_test(counties, incwage ~ county)
```

    ## # A tibble: 1 x 7
    ##   .y.         n statistic   DFn   DFd        p method     
    ## * <chr>   <int>     <dbl> <dbl> <dbl>    <dbl> <chr>      
    ## 1 incwage 26378      77.6     4 9654. 6.33e-65 Welch ANOVA

We conclude that there is a significant difference in earnings between
the counties. We can use Games Howell to see which counties have a
difference

``` r
games_howell_test(counties, incwage~county) #output is group2 - group 1
```

    ## # A tibble: 10 x 8
    ##    .y.     group1 group2 estimate conf.low conf.high         p.adj p.adj.signif
    ##  * <chr>   <chr>  <chr>     <dbl>    <dbl>     <dbl>         <dbl> <chr>       
    ##  1 incwage Dupage Allen   -19453.  -22756.   -16151. 0             ****        
    ##  2 incwage Dupage Kent    -16766.  -20129.   -13403. 0             ****        
    ##  3 incwage Dupage Summit  -14386.  -17888.   -10885. 0             ****        
    ##  4 incwage Dupage Dane     -8589.  -12401.    -4777. 0.00000000828 ****        
    ##  5 incwage Allen  Kent      2687.    -521.     5895. 0.15          ns          
    ##  6 incwage Allen  Summit    5067.    1714.     8419. 0.000363      ***         
    ##  7 incwage Allen  Dane     10864.    7188.    14540. 0             ****        
    ##  8 incwage Kent   Summit    2379.   -1033.     5791. 0.316         ns          
    ##  9 incwage Kent   Dane      8177.    4447.    11907. 0.0000000234  ****        
    ## 10 incwage Summit Dane      5798.    1942.     9653. 0.000397      ***

**The significant differences at 5%**

-   Allen - Dupage (Dupage pays more)
-   Kent - Dupage (Dupage pays more)
-   Summit - Dupage (Dupage pays more)
-   Dane - Dupage (Dupage pays more)
-   Summit - Allen (Summit pays more)
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
welch_anova_test(undergrad, incwage ~ county)
```

    ## # A tibble: 1 x 7
    ##   .y.         n statistic   DFn   DFd             p method     
    ## * <chr>   <int>     <dbl> <dbl> <dbl>         <dbl> <chr>      
    ## 1 incwage  5402      12.0     4 2087. 0.00000000133 Welch ANOVA

significant difference between counties tukey to do pair-wise

``` r
games_howell_test(undergrad, incwage ~ county)
```

    ## # A tibble: 10 x 8
    ##    .y.     group1 group2 estimate conf.low conf.high       p.adj p.adj.signif
    ##  * <chr>   <chr>  <chr>     <dbl>    <dbl>     <dbl>       <dbl> <chr>       
    ##  1 incwage Dupage Allen   -15552.  -23547.    -7556. 0.00000127  ****        
    ##  2 incwage Dupage Kent    -15057.  -22385.    -7729. 0.000000229 ****        
    ##  3 incwage Dupage Summit   -5103.  -13700.     3495. 0.484       ns          
    ##  4 incwage Dupage Dane    -11862.  -18769.    -4955. 0.0000287   ****        
    ##  5 incwage Allen  Kent       495.   -8004.     8993. 1           ns          
    ##  6 incwage Allen  Summit   10449.     836.    20062. 0.025       *           
    ##  7 incwage Allen  Dane      3690.   -4450.    11829. 0.729       ns          
    ##  8 incwage Kent   Summit    9954.     887.    19021. 0.023       *           
    ##  9 incwage Kent   Dane      3195.   -4291.    10680. 0.771       ns          
    ## 10 incwage Summit Dane     -6760.  -15491.     1972. 0.214       ns

**Significant differences at 5%**

-   Allen - Dupage (Dupage pays more)
-   Kent - Dupage (Dupage pays more)
-   Dane - Dupage (Dupage pays more)
-   Summit - Allen (Summit pays more)
-   Summit - Kent (Summit pays more)

## Conclusion

At least with the counties chosen, Grand Rapids doesn’t pay
significantly less than most of the counties. The counties chosen were
intended to be similar to Grand Rapids but there may be value in
including larger cities nearby like Detroit or Chicago. It may also be
worthwhile to include counties outside of the midwest as difference in
pay may be more of a regional thing.

It’s also important to note that Dupage County has quite a few more
respondents than the other counties. The means are weighted so that
shouldn’t be a large problem but it’s good to note. Also Dupage county
is a proxy for Naperville Illnois which is a suburb of Chicago. The fact
the wages are higher here isn’t super surprising. All these high earners
are probably employed in the city.

We should also make age-earnings profiles for the counties to see if
there’s a difference on age. We could do that with ANOVA but that sounds
painful… Maybe with some form of factor (early/mid/late career).

We’ll also want to break it down by majors

\#\#TODO

-   compare mean earnings of GVSU graduates (emsi data) with Kent County
    in general.
-   major breakdown
-   compare out of state GVSU with in state GVSU?
