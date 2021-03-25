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
  rename(county = state_county)
```

summary table. The data is so skewed right this probably isn’t very
useful. Maybe do weighted median?

``` r
counties %>% 
  group_by(county) %>% 
  summarize(mean_wage = weighted.mean(incwage, perwt), n = n()) %>% 
  knitr::kable()
```

| county | mean\_wage |    n |
|:-------|-----------:|-----:|
| Dupage |   235086.1 | 9374 |
| Allen  |   248912.1 | 3872 |
| Kent   |   238468.4 | 4465 |
| Summit |   208130.9 | 4875 |
| Dane   |   216662.9 | 3792 |

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

![](county_analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

ANOVA to see if there is a significant difference between the counties

``` r
anova <- aov(incwage ~ county, weights = perwt, data = counties)
summary(anova)
```

    ##                Df    Sum Sq   Mean Sq F value   Pr(>F)    
    ## county          4 5.612e+14 1.403e+14   8.064 1.71e-06 ***
    ## Residuals   26373 4.588e+17 1.740e+13                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

We conclude from the ANOVA that there is a significant difference in
earnings between the counties. We can use Tukey to find out which
differences specifically are different.

``` r
TukeyHSD(anova)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = incwage ~ county, data = counties, weights = perwt)
    ## 
    ## $county
    ##                     diff        lwr         upr     p adj
    ## Allen-Dupage   13826.000  -5706.258  33358.2566 0.3008243
    ## Kent-Dupage     3382.259 -15209.451  21973.9702 0.9877394
    ## Summit-Dupage -26955.181 -45009.562  -8900.7995 0.0004468
    ## Dane-Dupage   -18423.209 -38100.734   1254.3172 0.0791484
    ## Kent-Allen    -10443.740 -32896.343  12008.8630 0.7104439
    ## Summit-Allen  -40781.181 -62790.914 -18771.4470 0.0000043
    ## Dane-Allen    -32249.208 -55608.846  -8889.5698 0.0015587
    ## Summit-Kent   -30337.440 -51516.935  -9157.9454 0.0008869
    ## Dane-Kent     -21805.468 -44384.559    773.6228 0.0642933
    ## Dane-Summit     8531.972 -13606.779  30670.7240 0.8313053

The significant differences are

-   Summit and Dupage
-   Summit and Allen
-   Dane and Allen
-   Summit and Kent

Looking closely at the Tukey output, the outliers are really skewing the
results. It may be beneficial to either remove outliers or look only
within IQR.

The cities may also not have been chosen well to be compared to Kent
County (which is what we’re really interested in)

## Two-way anova with county and educ

I have no idea if this is valid or not

``` r
county_2 <- counties

county_2 <- county_2 %>% 
  mutate(highest_deg = case_when(educd < 62 ~ "Less than HS",
                                 educd < 81 ~ "High School",
                                 educd < 101 ~ "Associates",
                                 educd < 114 ~ "Bachelors",
                                 TRUE ~ "Grad")) #will need to reconsider these blocks

county_2$highest_deg <- factor(county_2$highest_deg)

anova2 <- aov(incwage ~ highest_deg, weights = perwt, data = county_2)
summary(anova2)
```

    ##                Df    Sum Sq   Mean Sq F value Pr(>F)    
    ## highest_deg     4 2.794e+17 6.986e+16   10239 <2e-16 ***
    ## Residuals   26373 1.799e+17 6.823e+12                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
TukeyHSD(anova2)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = incwage ~ highest_deg, data = county_2, weights = perwt)
    ## 
    ## $highest_deg
    ##                               diff         lwr        upr     p adj
    ## Bachelors-Associates      17623.51   -485.1403  35732.155 0.0609174
    ## Grad-Associates           40794.43  21242.2368  60346.618 0.0000001
    ## High School-Associates   -11275.72 -28461.4616   5910.021 0.3795161
    ## Less than HS-Associates  678906.89 661246.5091 696567.271 0.0000000
    ## Grad-Bachelors            23170.92   8700.9558  37640.885 0.0001223
    ## High School-Bachelors    -28899.23 -39964.5526 -17833.903 0.0000000
    ## Less than HS-Bachelors   661283.38 649494.3723 673072.394 0.0000000
    ## High School-Grad         -52070.15 -65367.0068 -38773.289 0.0000000
    ## Less than HS-Grad        638112.46 624207.5782 652017.347 0.0000000
    ## Less than HS-High School 690182.61 679867.2160 700498.005 0.0000000

All differences are significant except for high school and associates
degrees. This may be due to poor model specification.

It may be better yet to do some interaction model

``` r
anova3 <- aov(incwage ~ county * highest_deg, weights = perwt, data = county_2)
summary(anova3)
```

    ##                       Df    Sum Sq   Mean Sq  F value Pr(>F)    
    ## county                 4 5.612e+14 1.403e+14    20.73 <2e-16 ***
    ## highest_deg            4 2.793e+17 6.983e+16 10315.11 <2e-16 ***
    ## county:highest_deg    16 1.106e+15 6.913e+13    10.21 <2e-16 ***
    ## Residuals          26353 1.784e+17 6.770e+12                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
TukeyHSD(anova3)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = incwage ~ county * highest_deg, data = county_2, weights = perwt)
    ## 
    ## $county
    ##                     diff        lwr        upr     p adj
    ## Allen-Dupage   13826.000   1502.554  26149.445 0.0187912
    ## Kent-Dupage     3382.259  -8347.769  15112.288 0.9346093
    ## Summit-Dupage -26955.181 -38346.194 -15564.168 0.0000000
    ## Dane-Dupage   -18423.209 -30838.308  -6008.109 0.0004959
    ## Kent-Allen    -10443.740 -24609.713   3722.233 0.2605521
    ## Summit-Allen  -40781.181 -54667.735 -26894.626 0.0000000
    ## Dane-Allen    -32249.208 -46987.455 -17510.961 0.0000000
    ## Summit-Kent   -30337.440 -43700.174 -16974.706 0.0000000
    ## Dane-Kent     -21805.468 -36051.246  -7559.690 0.0002876
    ## Dane-Summit     8531.972  -5435.983  22499.928 0.4550500
    ## 
    ## $highest_deg
    ##                                diff         lwr        upr     p adj
    ## Bachelors-Associates      18364.551    311.2332  36417.869 0.0438840
    ## Grad-Associates           41480.385  21987.9345  60972.835 0.0000001
    ## High School-Associates    -9884.766 -27017.9973   7248.466 0.5143197
    ## Less than HS-Associates  678800.740 661194.3190 696407.161 0.0000000
    ## Grad-Bachelors            23115.834   8690.0812  37541.586 0.0001206
    ## High School-Bachelors    -28249.317 -39280.8326 -17217.801 0.0000000
    ## Less than HS-Bachelors   660436.189 648683.1988 672189.179 0.0000000
    ## High School-Grad         -51365.151 -64621.3820 -38108.919 0.0000000
    ## Less than HS-Grad        637320.355 623457.9560 651182.755 0.0000000
    ## Less than HS-High School 688685.506 678401.6293 698969.382 0.0000000
    ## 
    ## $`county:highest_deg`
    ##                                                diff         lwr        upr
    ## Allen:Associates-Dupage:Associates      -11887.1889 -73289.9166  49515.539
    ## Kent:Associates-Dupage:Associates        -8888.2707 -72575.7361  54799.195
    ## Summit:Associates-Dupage:Associates      -7109.4761 -70468.6038  56249.652
    ## Dane:Associates-Dupage:Associates        -4822.2259 -72037.1139  62392.662
    ## Dupage:Bachelors-Dupage:Associates       17153.7620 -26029.0893  60336.613
    ## Allen:Bachelors-Dupage:Associates         5009.4946 -48544.7312  58563.720
    ## Kent:Bachelors-Dupage:Associates          7585.7614 -42074.3256  57245.848
    ## Summit:Bachelors-Dupage:Associates       12011.7367 -37035.8371  61059.310
    ## Dane:Bachelors-Dupage:Associates          8015.7291 -40703.7054  56735.164
    ## Dupage:Grad-Dupage:Associates            44783.1776   -343.5761  89909.931
    ## Allen:Grad-Dupage:Associates             22310.0219 -44642.4174  89262.461
    ## Kent:Grad-Dupage:Associates              25016.6725 -33079.2582  83112.603
    ## Summit:Grad-Dupage:Associates            24752.1062 -31152.3542  80656.567
    ## Dane:Grad-Dupage:Associates              34251.2107 -19666.0722  88168.494
    ## Dupage:High School-Dupage:Associates    -15116.0865 -57683.3365  27451.163
    ## Allen:High School-Dupage:Associates     -17615.0839 -62545.1351  27314.967
    ## Kent:High School-Dupage:Associates      -18523.5867 -62940.7434  25893.570
    ## Summit:High School-Dupage:Associates    -18141.1231 -61779.0884  25496.842
    ## Dane:High School-Dupage:Associates      -17770.4994 -63733.7051  28192.706
    ## Dupage:Less than HS-Dupage:Associates   678423.7420 635420.7123 721426.772
    ## Allen:Less than HS-Dupage:Associates    634039.5671 587384.6878 680694.446
    ## Kent:Less than HS-Dupage:Associates     663698.5187 617443.3640 709953.673
    ## Summit:Less than HS-Dupage:Associates   631859.3772 585415.4448 678303.310
    ## Dane:Less than HS-Dupage:Associates     763980.9021 714285.8176 813675.987
    ## Kent:Associates-Allen:Associates          2998.9182 -65945.0935  71942.930
    ## Summit:Associates-Allen:Associates        4777.7128 -63863.1100  73418.536
    ## Dane:Associates-Allen:Associates          7064.9630 -65150.1646  79280.091
    ## Dupage:Bachelors-Allen:Associates        29040.9509 -21574.6806  79656.582
    ## Allen:Bachelors-Allen:Associates         16896.6835 -42812.9354  76606.303
    ## Kent:Bachelors-Allen:Associates          19472.9503 -36770.3403  75716.241
    ## Summit:Bachelors-Allen:Associates        23898.9256 -31804.2879  79602.139
    ## Dane:Bachelors-Allen:Associates          19902.9180 -35511.5819  75317.418
    ## Dupage:Grad-Allen:Associates             56670.3665   4386.4550 108954.278
    ## Allen:Grad-Allen:Associates              34197.2108 -37773.7043 106168.126
    ## Kent:Grad-Allen:Associates               36903.8614 -26910.8756 100718.599
    ## Summit:Grad-Allen:Associates             36639.2951 -25187.0115  98465.602
    ## Dane:Grad-Allen:Associates               46138.3996 -13897.0640 106173.863
    ## Dupage:High School-Allen:Associates      -3228.8976 -53320.3566  46862.561
    ## Allen:High School-Allen:Associates       -5727.8950 -57842.1253  46386.335
    ## Kent:High School-Allen:Associates        -6636.3978 -58309.0918  45036.296
    ## Summit:High School-Allen:Associates      -6253.9342 -57258.3999  44750.532
    ## Dane:High School-Allen:Associates        -5883.3105 -58890.8550  47124.234
    ## Dupage:Less than HS-Allen:Associates    690310.9309 639848.6276 740773.234
    ## Allen:Less than HS-Allen:Associates     645926.7560 592318.3494 699535.163
    ## Kent:Less than HS-Allen:Associates      675585.7076 622324.8136 728846.602
    ## Summit:Less than HS-Allen:Associates    643746.5661 590321.6435 697171.489
    ## Dane:Less than HS-Allen:Associates      775868.0910 719593.8969 832142.285
    ## Summit:Associates-Kent:Associates         1778.7945 -68913.2195  72470.809
    ## Dane:Associates-Kent:Associates           4066.0448 -70101.4882  78233.578
    ## Dupage:Bachelors-Kent:Associates         26042.0326 -27322.1916  79406.257
    ## Allen:Bachelors-Kent:Associates          13897.7653 -48158.9578  75954.488
    ## Kent:Bachelors-Kent:Associates           16474.0320 -42255.0581  75203.122
    ## Summit:Bachelors-Kent:Associates         20900.0073 -37312.0729  79112.088
    ## Dane:Bachelors-Kent:Associates           16903.9998 -41031.8707  74839.870
    ## Dupage:Grad-Kent:Associates              53671.4483  -1277.6708 108620.567
    ## Allen:Grad-Kent:Associates               31198.2926 -42731.4776 105128.063
    ## Kent:Grad-Kent:Associates                33904.9432 -32111.1075  99920.994
    ## Summit:Grad-Kent:Associates              33640.3769 -30455.5705  97736.324
    ## Dane:Grad-Kent:Associates                43139.4814 -19230.8255 105509.788
    ## Dupage:High School-Kent:Associates       -6227.8158 -59095.1267  46639.495
    ## Allen:High School-Kent:Associates        -8726.8133 -63514.5061  46060.880
    ## Kent:High School-Kent:Associates         -9635.3161 -64003.1888  44732.557
    ## Summit:High School-Kent:Associates       -9252.8524 -62986.0247  44480.320
    ## Dane:High School-Kent:Associates         -8882.2287 -64520.3277  46755.870
    ## Dupage:Less than HS-Kent:Associates     687312.0127 634093.1970 740530.828
    ## Allen:Less than HS-Kent:Associates      642927.8377 586716.9888 699138.687
    ## Kent:Less than HS-Kent:Associates       672586.7893 616707.2661 728466.313
    ## Summit:Less than HS-Kent:Associates     640747.6478 584711.7608 696783.535
    ## Dane:Less than HS-Kent:Associates       772869.1728 714110.4865 831627.859
    ## Dane:Associates-Summit:Associates         2287.2503 -71598.5315  76173.032
    ## Dupage:Bachelors-Summit:Associates       24263.2381 -28708.7001  77235.176
    ## Allen:Bachelors-Summit:Associates        12118.9708 -49600.7399  73838.681
    ## Kent:Bachelors-Summit:Associates         14695.2375 -43677.6314  73068.106
    ## Summit:Bachelors-Summit:Associates       19121.2128 -38731.4629  76973.888
    ## Dane:Bachelors-Summit:Associates         15125.2053 -42449.5365  72699.947
    ## Dupage:Grad-Summit:Associates            51892.6537  -2675.5741 106460.882
    ## Allen:Grad-Summit:Associates             29419.4981 -44227.6113 103066.607
    ## Kent:Grad-Summit:Associates              32126.1487 -33573.2026  97825.500
    ## Summit:Grad-Summit:Associates            31861.5824 -31908.1305  95631.295
    ## Dane:Grad-Summit:Associates              41360.6869 -20674.3112 103395.685
    ## Dupage:High School-Summit:Associates     -8006.6104 -60477.9202  44464.699
    ## Allen:High School-Summit:Associates     -10505.6078 -64911.2793  43900.064
    ## Kent:High School-Summit:Associates      -11414.1106 -65396.9911  42568.770
    ## Summit:High School-Summit:Associates    -11031.6469 -64375.2464  42311.953
    ## Dane:High School-Summit:Associates      -10661.0232 -65922.9800  44600.934
    ## Dupage:Less than HS-Summit:Associates   685533.2181 632707.7683 738358.668
    ## Allen:Less than HS-Summit:Associates    641149.0432 585310.4782 696987.608
    ## Kent:Less than HS-Summit:Associates     670807.9948 615302.9778 726313.012
    ## Summit:Less than HS-Summit:Associates   638968.8533 583306.4203 694631.286
    ## Dane:Less than HS-Summit:Associates     771090.3782 712687.7326 829493.024
    ## Dupage:Bachelors-Dane:Associates         21975.9879 -35552.1648  79504.140
    ## Allen:Bachelors-Dane:Associates           9831.7205 -55840.0756  75503.517
    ## Kent:Bachelors-Dane:Associates           12407.9872 -50128.8334  74944.808
    ## Summit:Bachelors-Dane:Associates         16833.9625 -45217.5821  78885.507
    ## Dane:Bachelors-Dane:Associates           12837.9550 -48954.5445  74630.455
    ## Dupage:Grad-Dane:Associates              49605.4035  -9395.8982 108606.705
    ## Allen:Grad-Dane:Associates               27132.2478 -49857.0945 104121.590
    ## Kent:Grad-Dane:Associates                29838.8984 -39586.3626  99264.159
    ## Summit:Grad-Dane:Associates              29574.3321 -38027.7270  97176.391
    ## Dane:Grad-Dane:Associates                39073.4366 -26894.7610 105041.634
    ## Dupage:High School-Dane:Associates      -10293.8606 -67361.3686  46773.647
    ## Allen:High School-Dane:Associates       -12792.8581 -71643.8495  46058.133
    ## Kent:High School-Dane:Associates        -13701.3609 -72161.7192  44758.998
    ## Summit:High School-Dane:Associates      -13318.8972 -71189.4572  44551.663
    ## Dane:High School-Dane:Associates        -12948.2735 -72591.7641  46695.217
    ## Dupage:Less than HS-Dane:Associates     683245.9679 625852.6733 740639.262
    ## Allen:Less than HS-Dane:Associates      638861.7930 578683.6619 699039.924
    ## Kent:Less than HS-Dane:Associates       668520.7446 608651.9795 728389.510
    ## Summit:Less than HS-Dane:Associates     636681.6030 576666.8669 696696.339
    ## Dane:Less than HS-Dane:Associates       768803.1280 706238.5124 831367.744
    ## Allen:Bachelors-Dupage:Bachelors        -12144.2674 -52883.7111  28595.176
    ## Kent:Bachelors-Dupage:Bachelors          -9568.0006 -45033.0738  25897.073
    ## Summit:Bachelors-Dupage:Bachelors        -5142.0253 -39744.2171  29460.166
    ## Dane:Bachelors-Dupage:Bachelors          -9138.0329 -43273.5053  24997.440
    ## Dupage:Grad-Dupage:Bachelors             27629.4156  -1146.8086  56405.640
    ## Allen:Grad-Dupage:Bachelors               5156.2600 -52065.0325  62377.552
    ## Kent:Grad-Dupage:Bachelors                7862.9105 -38685.8203  54411.641
    ## Summit:Grad-Dupage:Bachelors              7598.3442 -36184.7104  51381.399
    ## Dane:Grad-Dupage:Bachelors               17097.4487 -24118.0893  58312.987
    ## Dupage:High School-Dupage:Bachelors     -32269.8485 -56838.4859  -7701.211
    ## Allen:High School-Dupage:Bachelors      -34768.8459 -63235.6104  -6302.081
    ## Kent:High School-Dupage:Bachelors       -35677.3487 -63327.5022  -8027.195
    ## Summit:High School-Dupage:Bachelors     -35294.8851 -61675.1656  -8914.605
    ## Dane:High School-Dupage:Bachelors       -34924.2614 -64995.2350  -4853.288
    ## Dupage:Less than HS-Dupage:Bachelors    661269.9800 635953.8243 686586.136
    ## Allen:Less than HS-Dupage:Bachelors     616885.8051 585767.8853 648003.725
    ## Kent:Less than HS-Dupage:Bachelors      646544.7567 616029.4079 677060.105
    ## Summit:Less than HS-Dupage:Bachelors    614705.6152 583904.8679 645506.362
    ## Dane:Less than HS-Dupage:Bachelors      746827.1401 711313.0781 782341.202
    ## Kent:Bachelors-Allen:Bachelors            2576.2667 -44974.4206  50126.954
    ## Summit:Bachelors-Allen:Bachelors          7002.2420 -39908.3978  53912.882
    ## Dane:Bachelors-Allen:Bachelors            3006.2345 -43561.2105  49573.679
    ## Dupage:Grad-Allen:Bachelors              39773.6830  -3020.7972  82568.163
    ## Allen:Grad-Allen:Bachelors               17300.5273 -48102.6283  82703.683
    ## Kent:Grad-Allen:Bachelors                20007.1779 -36296.2927  76310.648
    ## Summit:Grad-Allen:Bachelors              19742.6116 -34296.7474  73781.971
    ## Dane:Grad-Allen:Bachelors                29241.7161 -22739.2134  81222.646
    ## Dupage:High School-Allen:Bachelors      -20125.5811 -60211.9179  19960.756
    ## Allen:High School-Allen:Bachelors       -22624.5786 -65211.5851  19962.428
    ## Kent:High School-Allen:Bachelors        -23533.0813 -65578.6214  18512.459
    ## Summit:High School-Allen:Bachelors      -23150.6177 -64372.1612  18070.926
    ## Dane:High School-Allen:Bachelors        -22779.9940 -66455.6154  20895.627
    ## Dupage:Less than HS-Allen:Bachelors     673414.2474 632865.4596 713963.035
    ## Allen:Less than HS-Allen:Bachelors      629030.0725 584627.1290 673433.016
    ## Kent:Less than HS-Allen:Bachelors       658689.0241 614706.2665 702671.782
    ## Summit:Less than HS-Allen:Bachelors     626849.8825 582668.6367 671031.128
    ## Dane:Less than HS-Allen:Bachelors       758971.4075 711384.1712 806558.644
    ## Summit:Bachelors-Kent:Bachelors           4425.9753 -37984.8401  46836.791
    ## Dane:Bachelors-Kent:Bachelors              429.9678 -41600.9264  42460.862
    ## Dupage:Grad-Kent:Bachelors               37197.4163   -610.4753  75005.308
    ## Allen:Grad-Kent:Bachelors                14724.2606 -47530.3931  76978.914
    ## Kent:Grad-Kent:Bachelors                 17430.9112 -35182.2995  70044.122
    ## Summit:Grad-Kent:Bachelors               17166.3449 -33016.5353  67349.225
    ## Dane:Grad-Kent:Bachelors                 26665.4494 -21293.7642  74624.663
    ## Dupage:High School-Kent:Bachelors       -22701.8479 -57414.7206  12011.025
    ## Allen:High School-Kent:Bachelors        -25200.8453 -62773.7380  12372.047
    ## Kent:High School-Kent:Bachelors         -26109.3481 -63067.3861  10848.690
    ## Summit:High School-Kent:Bachelors       -25726.8844 -61744.7243  10290.955
    ## Dane:High School-Kent:Bachelors         -25356.2607 -64158.6968  13446.175
    ## Dupage:Less than HS-Kent:Bachelors      670837.9806 635592.0827 706083.879
    ## Allen:Less than HS-Kent:Bachelors       626453.8057 586834.4857 666073.126
    ## Kent:Less than HS-Kent:Bachelors        656112.7573 616964.9337 695260.581
    ## Summit:Less than HS-Kent:Bachelors      624273.6158 584902.9209 663644.311
    ## Dane:Less than HS-Kent:Bachelors        756395.1407 713237.1265 799553.155
    ## Dane:Bachelors-Summit:Bachelors          -3996.0075 -45301.4107  37309.396
    ## Dupage:Grad-Summit:Bachelors             32771.4409  -4228.2473  69771.129
    ## Allen:Grad-Summit:Bachelors              10298.2853 -51468.8756  72065.446
    ## Kent:Grad-Summit:Bachelors               13004.9359 -39030.5347  65040.406
    ## Summit:Grad-Summit:Bachelors             12740.3696 -36836.4569  62317.196
    ## Dane:Grad-Summit:Bachelors               22239.4741 -25085.2174  69564.166
    ## Dupage:High School-Summit:Bachelors     -27127.8232 -60958.6345   6702.988
    ## Allen:High School-Summit:Bachelors      -29626.8206 -66386.3435   7132.702
    ## Kent:High School-Summit:Bachelors       -30535.3234 -66666.1528   5595.506
    ## Summit:High School-Summit:Bachelors     -30152.8597 -65321.3833   5015.664
    ## Dane:High School-Summit:Bachelors       -29782.2361 -67797.6185   8233.146
    ## Dupage:Less than HS-Summit:Bachelors    666412.0053 632034.4896 700789.521
    ## Allen:Less than HS-Summit:Bachelors     622027.8304 583179.0108 660876.650
    ## Kent:Less than HS-Summit:Bachelors      651686.7820 613318.9262 690054.638
    ## Summit:Less than HS-Summit:Bachelors    619847.6405 581252.4092 658442.872
    ## Dane:Less than HS-Summit:Bachelors      751969.1654 709517.3757 794420.955
    ## Dupage:Grad-Dane:Bachelors               36767.4485    203.8637  73331.033
    ## Allen:Grad-Dane:Bachelors                14294.2928 -47212.6252  75801.211
    ## Kent:Grad-Dane:Bachelors                 17000.9434 -34725.3458  68727.233
    ## Summit:Grad-Dane:Bachelors               16736.3771 -32515.8363  65988.590
    ## Dane:Grad-Dane:Bachelors                 26235.4816 -20749.0395  73220.003
    ## Dupage:High School-Dane:Bachelors       -23131.8156 -56485.1153  10221.484
    ## Allen:High School-Dane:Bachelors        -25630.8131 -61951.3490  10689.723
    ## Kent:High School-Dane:Bachelors         -26539.3159 -62223.4249   9144.793
    ## Summit:High School-Dane:Bachelors       -26156.8522 -60866.2724   8552.568
    ## Dane:High School-Dane:Bachelors         -25786.2285 -63377.2927  11804.836
    ## Dupage:Less than HS-Dane:Bachelors      670408.0129 636500.3089 704315.717
    ## Allen:Less than HS-Dane:Bachelors       626023.8380 587590.1342 664457.542
    ## Kent:Less than HS-Dane:Bachelors        655682.7895 617735.3106 693630.269
    ## Summit:Less than HS-Dane:Bachelors      623843.6480 585666.2897 662021.006
    ## Dane:Less than HS-Dane:Bachelors        755965.1730 713892.9344 798037.412
    ## Allen:Grad-Dupage:Grad                  -22473.1557 -81175.2984  36228.987
    ## Kent:Grad-Dupage:Grad                   -19766.5051 -68124.0236  28591.013
    ## Summit:Grad-Dupage:Grad                 -20031.0714 -65732.5060  25670.363
    ## Dane:Grad-Dupage:Grad                   -10531.9669 -53779.9244  32715.991
    ## Dupage:High School-Dupage:Grad          -59899.2641 -87743.1718 -32055.356
    ## Allen:High School-Dupage:Grad           -62398.2615 -93735.4907 -31061.032
    ## Kent:High School-Dupage:Grad            -63306.7643 -93904.0883 -32709.440
    ## Summit:High School-Dupage:Grad          -62924.3007 -92379.0872 -33469.514
    ## Dane:High School-Dupage:Grad            -62553.6770 -95355.0292 -29752.325
    ## Dupage:Less than HS-Dupage:Grad         633640.5644 605134.8985 662146.230
    ## Allen:Less than HS-Dupage:Grad          589256.3895 555492.6488 623020.130
    ## Kent:Less than HS-Dupage:Grad           618915.3411 585706.1291 652124.553
    ## Summit:Less than HS-Dupage:Grad         587076.1996 553604.5506 620547.849
    ## Dane:Less than HS-Dupage:Grad           719197.7245 681343.8759 757051.573
    ## Kent:Grad-Allen:Grad                      2706.6506 -66464.5488  71877.850
    ## Summit:Grad-Allen:Grad                    2442.0843 -64899.0351  69783.204
    ## Dane:Grad-Allen:Grad                     11941.1888 -53759.5803  77641.958
    ## Dupage:High School-Allen:Grad           -37426.1084 -94184.2658  19332.049
    ## Allen:High School-Allen:Grad            -39925.1059 -98476.1705  18625.959
    ## Kent:High School-Allen:Grad             -40833.6087 -98992.0257  17324.808
    ## Summit:High School-Allen:Grad           -40451.1450 -98016.6702  17114.380
    ## Dane:High School-Allen:Grad             -40080.5213 -99428.0903  19267.048
    ## Dupage:Less than HS-Allen:Grad          656113.7201 599028.0106 713199.429
    ## Allen:Less than HS-Allen:Grad           611729.5451 551844.6938 671614.396
    ## Kent:Less than HS-Allen:Grad            641388.4967 581814.5343 700962.459
    ## Summit:Less than HS-Allen:Grad          609549.3552 549828.7012 669270.009
    ## Dane:Less than HS-Allen:Grad            741670.8802 679388.3056 803953.455
    ## Summit:Grad-Kent:Grad                     -264.5663 -58808.0065  58278.874
    ## Dane:Grad-Kent:Grad                       9234.5382 -47414.3726  65883.449
    ## Dupage:High School-Kent:Grad            -40132.7590 -86110.9763   5845.458
    ## Allen:High School-Kent:Grad             -42631.7565 -90805.7660   5542.253
    ## Kent:High School-Kent:Grad              -43540.2593 -91236.2707   4155.752
    ## Summit:High School-Kent:Grad            -43157.7956 -90129.0391   3813.448
    ## Dane:High School-Kent:Grad              -42787.1719 -91926.1786   6351.835
    ## Dupage:Less than HS-Kent:Grad           653407.0695 607025.1090 699789.030
    ## Allen:Less than HS-Kent:Grad            609022.8946 559236.3155 658809.474
    ## Kent:Less than HS-Kent:Grad             638681.8461 589269.6510 688094.041
    ## Summit:Less than HS-Kent:Grad           606842.7046 557253.7487 656431.661
    ## Dane:Less than HS-Kent:Grad             738964.2296 686317.9845 791610.475
    ## Dane:Grad-Summit:Grad                     9499.1045 -44900.0740  63898.283
    ## Dupage:High School-Summit:Grad          -39868.1927 -83044.2045   3307.819
    ## Allen:High School-Summit:Grad           -42367.1902 -87874.4063   3140.026
    ## Kent:High School-Summit:Grad            -43275.6930 -88276.5933   1725.207
    ## Summit:High School-Summit:Grad          -42893.2293 -87125.2228   1338.764
    ## Dane:High School-Summit:Grad            -42522.6056 -89050.1620   4004.951
    ## Dupage:Less than HS-Summit:Grad         653671.6358 610065.9276 697277.344
    ## Allen:Less than HS-Summit:Grad          609287.4609 562076.4984 656498.423
    ## Kent:Less than HS-Summit:Grad           638946.4124 592130.4263 685762.399
    ## Summit:Less than HS-Summit:Grad         607107.2709 560104.7595 654109.782
    ## Dane:Less than HS-Summit:Grad           739228.7959 689011.2825 789446.309
    ## Dupage:High School-Dane:Grad            -49367.2972 -89937.3933  -8797.201
    ## Allen:High School-Dane:Grad             -51866.2947 -94908.9645  -8823.625
    ## Kent:High School-Dane:Grad              -52774.7975 -95281.8055 -10267.789
    ## Summit:High School-Dane:Grad            -52392.3338 -94084.4667 -10700.201
    ## Dane:High School-Dane:Grad              -52021.7101 -96141.7532  -7901.667
    ## Dupage:Less than HS-Dane:Grad           644172.5313 603145.4367 685199.626
    ## Allen:Less than HS-Dane:Grad            599788.3564 554948.1993 644628.513
    ## Kent:Less than HS-Dane:Grad             629447.3080 585023.2012 673871.415
    ## Summit:Less than HS-Dane:Grad           597608.1664 552987.5346 642228.798
    ## Dane:Less than HS-Dane:Grad             729729.6914 681734.2400 777725.143
    ## Allen:High School-Dupage:High School     -2498.9974 -30022.9651  25024.970
    ## Kent:High School-Dupage:High School      -3407.5002 -30086.0141  23271.014
    ## Summit:High School-Dupage:High School    -3025.0366 -28385.0705  22334.997
    ## Dane:High School-Dupage:High School      -2654.4129 -31834.4673  26525.642
    ## Dupage:Less than HS-Dupage:High School  693539.8285 669288.6459 717791.011
    ## Allen:Less than HS-Dupage:High School   649155.6536 618897.8107 679413.496
    ## Kent:Less than HS-Dupage:High School    678814.6052 649176.8146 708452.396
    ## Summit:Less than HS-Dupage:High School  646975.4637 617043.9059 676907.021
    ## Dane:Less than HS-Dupage:High School    779096.9886 744334.0669 813859.910
    ## Kent:High School-Allen:High School        -908.5028 -31214.9681  29397.962
    ## Summit:High School-Allen:High School      -526.0391 -29678.5699  28626.492
    ## Dane:High School-Allen:High School        -155.4154 -32685.6215  32374.791
    ## Dupage:Less than HS-Allen:High School   696038.8259 667845.5893 724232.063
    ## Allen:Less than HS-Allen:High School    651654.6510 618154.2661 685155.036
    ## Kent:Less than HS-Allen:High School     681313.6026 648372.1794 714255.026
    ## Summit:Less than HS-Allen:High School   649474.4611 616268.4844 682680.438
    ## Dane:Less than HS-Allen:High School     781595.9860 743976.8492 819215.123
    ## Summit:High School-Kent:High School        382.4637 -27973.2123  28738.140
    ## Dane:High School-Kent:High School          753.0873 -31064.9675  32571.142
    ## Dupage:Less than HS-Kent:High School    696947.3287 669578.8634 724315.794
    ## Allen:Less than HS-Kent:High School     652563.1538 619753.8549 685372.453
    ## Kent:Less than HS-Kent:High School      682222.1054 649983.7483 714460.463
    ## Summit:Less than HS-Kent:High School    650382.9639 617874.3313 682891.596
    ## Dane:Less than HS-Kent:High School      782504.4888 745499.4383 819509.539
    ## Dane:High School-Summit:High School        370.6237 -30350.3273  31091.575
    ## Dupage:Less than HS-Summit:High School  696564.8651 670479.9823 722649.748
    ## Allen:Less than HS-Summit:High School   652180.6902 620434.2209 683927.159
    ## Kent:Less than HS-Summit:High School    681839.6417 650683.5848 712995.699
    ## Summit:Less than HS-Summit:High School  650000.5002 618564.8611 681436.139
    ## Dane:Less than HS-Summit:High School    782122.0252 746055.9472 818188.103
    ## Dupage:Less than HS-Dane:High School    696194.2414 666382.0733 726006.409
    ## Allen:Less than HS-Dane:High School     651810.0665 616936.2573 686683.876
    ## Kent:Less than HS-Dane:High School      681469.0181 647131.8059 715806.230
    ## Summit:Less than HS-Dane:High School    649629.8765 615038.7843 684220.969
    ## Dane:Less than HS-Dane:High School      781751.4015 742904.1850 820598.618
    ## Allen:Less than HS-Dupage:Less than HS  -44384.1749 -75252.0683 -13516.281
    ## Kent:Less than HS-Dupage:Less than HS   -14725.2233 -44985.5673  15535.121
    ## Summit:Less than HS-Dupage:Less than HS -46564.3648 -77112.4899 -16016.240
    ## Dane:Less than HS-Dupage:Less than HS    85557.1601  50261.9690 120852.351
    ## Kent:Less than HS-Allen:Less than HS     29658.9516  -5598.7514  64916.655
    ## Summit:Less than HS-Allen:Less than HS   -2180.1899 -37685.1916  33324.812
    ## Dane:Less than HS-Allen:Less than HS    129941.3350  90278.1567 169604.513
    ## Summit:Less than HS-Kent:Less than HS   -31839.1415 -66817.2306   3138.948
    ## Dane:Less than HS-Kent:Less than HS     100282.3834  61090.1739 139474.593
    ## Dane:Less than HS-Summit:Less than HS   132121.5249  92706.6951 171536.355
    ##                                             p adj
    ## Allen:Associates-Dupage:Associates      1.0000000
    ## Kent:Associates-Dupage:Associates       1.0000000
    ## Summit:Associates-Dupage:Associates     1.0000000
    ## Dane:Associates-Dupage:Associates       1.0000000
    ## Dupage:Bachelors-Dupage:Associates      0.9994016
    ## Allen:Bachelors-Dupage:Associates       1.0000000
    ## Kent:Bachelors-Dupage:Associates        1.0000000
    ## Summit:Bachelors-Dupage:Associates      0.9999999
    ## Dane:Bachelors-Dupage:Associates        1.0000000
    ## Dupage:Grad-Dupage:Associates           0.0548422
    ## Allen:Grad-Dupage:Associates            0.9999707
    ## Kent:Grad-Dupage:Associates             0.9979134
    ## Summit:Grad-Dupage:Associates           0.9968613
    ## Dane:Grad-Dupage:Associates             0.8178754
    ## Dupage:High School-Dupage:Associates    0.9999088
    ## Allen:High School-Dupage:Associates     0.9995159
    ## Kent:High School-Dupage:Associates      0.9987148
    ## Summit:High School-Dupage:Associates    0.9987759
    ## Dane:High School-Dupage:Associates      0.9996145
    ## Dupage:Less than HS-Dupage:Associates   0.0000000
    ## Allen:Less than HS-Dupage:Associates    0.0000000
    ## Kent:Less than HS-Dupage:Associates     0.0000000
    ## Summit:Less than HS-Dupage:Associates   0.0000000
    ## Dane:Less than HS-Dupage:Associates     0.0000000
    ## Kent:Associates-Allen:Associates        1.0000000
    ## Summit:Associates-Allen:Associates      1.0000000
    ## Dane:Associates-Allen:Associates        1.0000000
    ## Dupage:Bachelors-Allen:Associates       0.9254693
    ## Allen:Bachelors-Allen:Associates        0.9999987
    ## Kent:Bachelors-Allen:Associates         0.9999416
    ## Summit:Bachelors-Allen:Associates       0.9980241
    ## Dane:Bachelors-Allen:Associates         0.9998889
    ## Dupage:Grad-Allen:Associates            0.0166847
    ## Allen:Grad-Allen:Associates             0.9916302
    ## Kent:Grad-Allen:Associates              0.9195314
    ## Summit:Grad-Allen:Associates            0.8987344
    ## Dane:Grad-Allen:Associates              0.4473711
    ## Dupage:High School-Allen:Associates     1.0000000
    ## Allen:High School-Allen:Associates      1.0000000
    ## Kent:High School-Allen:Associates       1.0000000
    ## Summit:High School-Allen:Associates     1.0000000
    ## Dane:High School-Allen:Associates       1.0000000
    ## Dupage:Less than HS-Allen:Associates    0.0000000
    ## Allen:Less than HS-Allen:Associates     0.0000000
    ## Kent:Less than HS-Allen:Associates      0.0000000
    ## Summit:Less than HS-Allen:Associates    0.0000000
    ## Dane:Less than HS-Allen:Associates      0.0000000
    ## Summit:Associates-Kent:Associates       1.0000000
    ## Dane:Associates-Kent:Associates         1.0000000
    ## Dupage:Bachelors-Kent:Associates        0.9881356
    ## Allen:Bachelors-Kent:Associates         1.0000000
    ## Kent:Bachelors-Kent:Associates          0.9999989
    ## Summit:Bachelors-Kent:Associates        0.9998896
    ## Dane:Bachelors-Kent:Associates          0.9999976
    ## Dupage:Grad-Kent:Associates             0.0660492
    ## Allen:Grad-Kent:Associates              0.9984602
    ## Kent:Grad-Kent:Associates               0.9776598
    ## Summit:Grad-Kent:Associates             0.9711939
    ## Dane:Grad-Kent:Associates               0.6731612
    ## Dupage:High School-Kent:Associates      1.0000000
    ## Allen:High School-Kent:Associates       1.0000000
    ## Kent:High School-Kent:Associates        1.0000000
    ## Summit:High School-Kent:Associates      1.0000000
    ## Dane:High School-Kent:Associates        1.0000000
    ## Dupage:Less than HS-Kent:Associates     0.0000000
    ## Allen:Less than HS-Kent:Associates      0.0000000
    ## Kent:Less than HS-Kent:Associates       0.0000000
    ## Summit:Less than HS-Kent:Associates     0.0000000
    ## Dane:Less than HS-Kent:Associates       0.0000000
    ## Dane:Associates-Summit:Associates       1.0000000
    ## Dupage:Bachelors-Summit:Associates      0.9949199
    ## Allen:Bachelors-Summit:Associates       1.0000000
    ## Kent:Bachelors-Summit:Associates        0.9999999
    ## Summit:Bachelors-Summit:Associates      0.9999747
    ## Dane:Bachelors-Summit:Associates        0.9999997
    ## Dupage:Grad-Summit:Associates           0.0886880
    ## Allen:Grad-Summit:Associates            0.9993455
    ## Kent:Grad-Summit:Associates             0.9878258
    ## Summit:Grad-Summit:Associates           0.9840241
    ## Dane:Grad-Summit:Associates             0.7414385
    ## Dupage:High School-Summit:Associates    1.0000000
    ## Allen:High School-Summit:Associates     1.0000000
    ## Kent:High School-Summit:Associates      1.0000000
    ## Summit:High School-Summit:Associates    1.0000000
    ## Dane:High School-Summit:Associates      1.0000000
    ## Dupage:Less than HS-Summit:Associates   0.0000000
    ## Allen:Less than HS-Summit:Associates    0.0000000
    ## Kent:Less than HS-Summit:Associates     0.0000000
    ## Summit:Less than HS-Summit:Associates   0.0000000
    ## Dane:Less than HS-Summit:Associates     0.0000000
    ## Dupage:Bachelors-Dane:Associates        0.9996838
    ## Allen:Bachelors-Dane:Associates         1.0000000
    ## Kent:Bachelors-Dane:Associates          1.0000000
    ## Summit:Bachelors-Dane:Associates        0.9999994
    ## Dane:Bachelors-Dane:Associates          1.0000000
    ## Dupage:Grad-Dane:Associates             0.2616044
    ## Allen:Grad-Dane:Associates              0.9999202
    ## Kent:Grad-Dane:Associates               0.9979713
    ## Summit:Grad-Dane:Associates             0.9973642
    ## Dane:Grad-Dane:Associates               0.8992175
    ## Dupage:High School-Dane:Associates      1.0000000
    ## Allen:High School-Dane:Associates       1.0000000
    ## Kent:High School-Dane:Associates        1.0000000
    ## Summit:High School-Dane:Associates      1.0000000
    ## Dane:High School-Dane:Associates        1.0000000
    ## Dupage:Less than HS-Dane:Associates     0.0000000
    ## Allen:Less than HS-Dane:Associates      0.0000000
    ## Kent:Less than HS-Dane:Associates       0.0000000
    ## Summit:Less than HS-Dane:Associates     0.0000000
    ## Dane:Less than HS-Dane:Associates       0.0000000
    ## Allen:Bachelors-Dupage:Bachelors        0.9999964
    ## Kent:Bachelors-Dupage:Bachelors         0.9999995
    ## Summit:Bachelors-Dupage:Bachelors       1.0000000
    ## Dane:Bachelors-Dupage:Bachelors         0.9999996
    ## Dupage:Grad-Dupage:Bachelors            0.0799895
    ## Allen:Grad-Dupage:Bachelors             1.0000000
    ## Kent:Grad-Dupage:Bachelors              1.0000000
    ## Summit:Grad-Dupage:Bachelors            1.0000000
    ## Dane:Grad-Dupage:Bachelors              0.9988157
    ## Dupage:High School-Dupage:Bachelors     0.0004306
    ## Allen:High School-Dupage:Bachelors      0.0020765
    ## Kent:High School-Dupage:Bachelors       0.0006477
    ## Summit:High School-Dupage:Bachelors     0.0002772
    ## Dane:High School-Dupage:Bachelors       0.0053702
    ## Dupage:Less than HS-Dupage:Bachelors    0.0000000
    ## Allen:Less than HS-Dupage:Bachelors     0.0000000
    ## Kent:Less than HS-Dupage:Bachelors      0.0000000
    ## Summit:Less than HS-Dupage:Bachelors    0.0000000
    ## Dane:Less than HS-Dupage:Bachelors      0.0000000
    ## Kent:Bachelors-Allen:Bachelors          1.0000000
    ## Summit:Bachelors-Allen:Bachelors        1.0000000
    ## Dane:Bachelors-Allen:Bachelors          1.0000000
    ## Dupage:Grad-Allen:Bachelors             0.1121624
    ## Allen:Grad-Allen:Bachelors              0.9999997
    ## Kent:Grad-Allen:Bachelors               0.9999077
    ## Summit:Grad-Allen:Bachelors             0.9998509
    ## Dane:Grad-Allen:Bachelors               0.9388063
    ## Dupage:High School-Allen:Bachelors      0.9830389
    ## Allen:High School-Allen:Bachelors       0.9669144
    ## Kent:High School-Allen:Bachelors        0.9418993
    ## Summit:High School-Allen:Bachelors      0.9398337
    ## Dane:High School-Allen:Bachelors        0.9732061
    ## Dupage:Less than HS-Allen:Bachelors     0.0000000
    ## Allen:Less than HS-Allen:Bachelors      0.0000000
    ## Kent:Less than HS-Allen:Bachelors       0.0000000
    ## Summit:Less than HS-Allen:Bachelors     0.0000000
    ## Dane:Less than HS-Allen:Bachelors       0.0000000
    ## Summit:Bachelors-Kent:Bachelors         1.0000000
    ## Dane:Bachelors-Kent:Bachelors           1.0000000
    ## Dupage:Grad-Kent:Bachelors              0.0607386
    ## Allen:Grad-Kent:Bachelors               1.0000000
    ## Kent:Grad-Kent:Bachelors                0.9999736
    ## Summit:Grad-Kent:Bachelors              0.9999529
    ## Dane:Grad-Kent:Bachelors                0.9457572
    ## Dupage:High School-Kent:Bachelors       0.7738890
    ## Allen:High School-Kent:Bachelors        0.7309112
    ## Kent:High School-Kent:Bachelors         0.6304702
    ## Summit:High School-Kent:Bachelors       0.6074719
    ## Dane:High School-Kent:Bachelors         0.7751684
    ## Dupage:Less than HS-Kent:Bachelors      0.0000000
    ## Allen:Less than HS-Kent:Bachelors       0.0000000
    ## Kent:Less than HS-Kent:Bachelors        0.0000000
    ## Summit:Less than HS-Kent:Bachelors      0.0000000
    ## Dane:Less than HS-Kent:Bachelors        0.0000000
    ## Dane:Bachelors-Summit:Bachelors         1.0000000
    ## Dupage:Grad-Summit:Bachelors            0.1745004
    ## Allen:Grad-Summit:Bachelors             1.0000000
    ## Kent:Grad-Summit:Bachelors              0.9999999
    ## Summit:Grad-Summit:Bachelors            0.9999998
    ## Dane:Grad-Summit:Bachelors              0.9927814
    ## Dupage:High School-Summit:Bachelors     0.3555415
    ## Allen:High School-Summit:Bachelors      0.3449074
    ## Kent:High School-Summit:Bachelors       0.2520698
    ## Summit:High School-Summit:Bachelors     0.2266043
    ## Dane:High School-Summit:Bachelors       0.4052751
    ## Dupage:Less than HS-Summit:Bachelors    0.0000000
    ## Allen:Less than HS-Summit:Bachelors     0.0000000
    ## Kent:Less than HS-Summit:Bachelors      0.0000000
    ## Summit:Less than HS-Summit:Bachelors    0.0000000
    ## Dane:Less than HS-Summit:Bachelors      0.0000000
    ## Dupage:Grad-Dane:Bachelors              0.0466908
    ## Allen:Grad-Dane:Bachelors               1.0000000
    ## Kent:Grad-Dane:Bachelors                0.9999772
    ## Summit:Grad-Dane:Bachelors              0.9999582
    ## Dane:Grad-Dane:Bachelors                0.9432958
    ## Dupage:High School-Dane:Bachelors       0.6678305
    ## Allen:High School-Dane:Bachelors        0.6327369
    ## Kent:High School-Dane:Bachelors         0.5199283
    ## Summit:High School-Dane:Bachelors       0.4907734
    ## Dane:High School-Dane:Bachelors         0.6892381
    ## Dupage:Less than HS-Dane:Bachelors      0.0000000
    ## Allen:Less than HS-Dane:Bachelors       0.0000000
    ## Kent:Less than HS-Dane:Bachelors        0.0000000
    ## Summit:Less than HS-Dane:Bachelors      0.0000000
    ## Dane:Less than HS-Dane:Bachelors        0.0000000
    ## Allen:Grad-Dupage:Grad                  0.9996723
    ## Kent:Grad-Dupage:Grad                   0.9990584
    ## Summit:Grad-Dupage:Grad                 0.9972904
    ## Dane:Grad-Dupage:Grad                   0.9999999
    ## Dupage:High School-Dupage:Grad          0.0000000
    ## Allen:High School-Dupage:Grad           0.0000000
    ## Kent:High School-Dupage:Grad            0.0000000
    ## Summit:High School-Dupage:Grad          0.0000000
    ## Dane:High School-Dupage:Grad            0.0000000
    ## Dupage:Less than HS-Dupage:Grad         0.0000000
    ## Allen:Less than HS-Dupage:Grad          0.0000000
    ## Kent:Less than HS-Dupage:Grad           0.0000000
    ## Summit:Less than HS-Dupage:Grad         0.0000000
    ## Dane:Less than HS-Dupage:Grad           0.0000000
    ## Kent:Grad-Allen:Grad                    1.0000000
    ## Summit:Grad-Allen:Grad                  1.0000000
    ## Dane:Grad-Allen:Grad                    1.0000000
    ## Dupage:High School-Allen:Grad           0.7603512
    ## Allen:High School-Allen:Grad            0.7005958
    ## Kent:High School-Allen:Grad             0.6431466
    ## Summit:High School-Allen:Grad           0.6414400
    ## Dane:High School-Allen:Grad             0.7184743
    ## Dupage:Less than HS-Allen:Grad          0.0000000
    ## Allen:Less than HS-Allen:Grad           0.0000000
    ## Kent:Less than HS-Allen:Grad            0.0000000
    ## Summit:Less than HS-Allen:Grad          0.0000000
    ## Dane:Less than HS-Allen:Grad            0.0000000
    ## Summit:Grad-Kent:Grad                   1.0000000
    ## Dane:Grad-Kent:Grad                     1.0000000
    ## Dupage:High School-Kent:Grad            0.1969619
    ## Allen:High School-Kent:Grad             0.1757887
    ## Kent:High School-Kent:Grad              0.1333175
    ## Summit:High School-Kent:Grad            0.1253871
    ## Dane:High School-Kent:Grad              0.2008659
    ## Dupage:Less than HS-Kent:Grad           0.0000000
    ## Allen:Less than HS-Kent:Grad            0.0000000
    ## Kent:Less than HS-Kent:Grad             0.0000000
    ## Summit:Less than HS-Kent:Grad           0.0000000
    ## Dane:Less than HS-Kent:Grad             0.0000000
    ## Dane:Grad-Summit:Grad                   1.0000000
    ## Dupage:High School-Summit:Grad          0.1195370
    ## Allen:High School-Summit:Grad           0.1102797
    ## Kent:High School-Summit:Grad            0.0786234
    ## Summit:High School-Summit:Grad          0.0716697
    ## Dane:High School-Summit:Grad            0.1318849
    ## Dupage:Less than HS-Summit:Grad         0.0000000
    ## Allen:Less than HS-Summit:Grad          0.0000000
    ## Kent:Less than HS-Summit:Grad           0.0000000
    ## Summit:Less than HS-Summit:Grad         0.0000000
    ## Dane:Less than HS-Summit:Grad           0.0000000
    ## Dupage:High School-Dane:Grad            0.0022362
    ## Allen:High School-Dane:Grad             0.0027082
    ## Kent:High School-Dane:Grad              0.0014884
    ## Summit:High School-Dane:Grad            0.0011551
    ## Dane:High School-Dane:Grad              0.0040833
    ## Dupage:Less than HS-Dane:Grad           0.0000000
    ## Allen:Less than HS-Dane:Grad            0.0000000
    ## Kent:Less than HS-Dane:Grad             0.0000000
    ## Summit:Less than HS-Dane:Grad           0.0000000
    ## Dane:Less than HS-Dane:Grad             0.0000000
    ## Allen:High School-Dupage:High School    1.0000000
    ## Kent:High School-Dupage:High School     1.0000000
    ## Summit:High School-Dupage:High School   1.0000000
    ## Dane:High School-Dupage:High School     1.0000000
    ## Dupage:Less than HS-Dupage:High School  0.0000000
    ## Allen:Less than HS-Dupage:High School   0.0000000
    ## Kent:Less than HS-Dupage:High School    0.0000000
    ## Summit:Less than HS-Dupage:High School  0.0000000
    ## Dane:Less than HS-Dupage:High School    0.0000000
    ## Kent:High School-Allen:High School      1.0000000
    ## Summit:High School-Allen:High School    1.0000000
    ## Dane:High School-Allen:High School      1.0000000
    ## Dupage:Less than HS-Allen:High School   0.0000000
    ## Allen:Less than HS-Allen:High School    0.0000000
    ## Kent:Less than HS-Allen:High School     0.0000000
    ## Summit:Less than HS-Allen:High School   0.0000000
    ## Dane:Less than HS-Allen:High School     0.0000000
    ## Summit:High School-Kent:High School     1.0000000
    ## Dane:High School-Kent:High School       1.0000000
    ## Dupage:Less than HS-Kent:High School    0.0000000
    ## Allen:Less than HS-Kent:High School     0.0000000
    ## Kent:Less than HS-Kent:High School      0.0000000
    ## Summit:Less than HS-Kent:High School    0.0000000
    ## Dane:Less than HS-Kent:High School      0.0000000
    ## Dane:High School-Summit:High School     1.0000000
    ## Dupage:Less than HS-Summit:High School  0.0000000
    ## Allen:Less than HS-Summit:High School   0.0000000
    ## Kent:Less than HS-Summit:High School    0.0000000
    ## Summit:Less than HS-Summit:High School  0.0000000
    ## Dane:Less than HS-Summit:High School    0.0000000
    ## Dupage:Less than HS-Dane:High School    0.0000000
    ## Allen:Less than HS-Dane:High School     0.0000000
    ## Kent:Less than HS-Dane:High School      0.0000000
    ## Summit:Less than HS-Dane:High School    0.0000000
    ## Dane:Less than HS-Dane:High School      0.0000000
    ## Allen:Less than HS-Dupage:Less than HS  0.0000418
    ## Kent:Less than HS-Dupage:Less than HS   0.9885624
    ## Summit:Less than HS-Dupage:Less than HS 0.0000073
    ## Dane:Less than HS-Dupage:Less than HS   0.0000000
    ## Kent:Less than HS-Allen:Less than HS    0.2606063
    ## Summit:Less than HS-Allen:Less than HS  1.0000000
    ## Dane:Less than HS-Allen:Less than HS    0.0000000
    ## Summit:Less than HS-Kent:Less than HS   0.1369207
    ## Dane:Less than HS-Kent:Less than HS     0.0000000
    ## Dane:Less than HS-Summit:Less than HS   0.0000000

and I really have no idea where to begin with this output -\_-
