Analyses for ‘Seasonal variation of micro and macro soil invertebrate
communities in a temperate forest’
================
2024-March-10

# Across Year Effects

## How does the seasonal variability impact different invertebrate communities?

<div class="figure">

<img src="Figures/SupplementaryFigure1_ClimateVariables.png" alt="Climate Variables" width="3600" />
<p class="caption">

Climate Variables
</p>

</div>

## Nematodes:

### Total Nematodes

Significant drivers were season and the interaction between topography
and tree functional type.

``` r
model.total <-lme(total.nematodes~season+landscape.position+mycorrhizal.fungi.type+season:landscape.position+
                     mycorrhizal.fungi.type:landscape.position+season:mycorrhizal.fungi.type+
                     season:landscape.position:mycorrhizal.fungi.type, 
                     random=list(sample.id=~1, tree.identity=~1), data = nematode)
summary(model.total)$tTable
```

                                                                             Value Std.Error  DF t-value p-value
    (Intercept)                                                             4752.9      3136 100  1.5156  0.1328
    seasonSpring                                                             403.3      3489 100  0.1156  0.9082
    seasonSummer                                                            5744.1      3748 100  1.5327  0.1285
    seasonFall                                                              8627.2      5464  29  1.5790  0.1252
    seasonWinter.early                                                     13463.7      4455  29  3.0224  0.0052
    landscape.positionUphill                                               10449.8      4945  29  2.1134  0.0433
    mycorrhizal.fungi.typeECM                                               5091.7      6213  29  0.8195  0.4192
    seasonSpring:landscape.positionUphill                                  -5268.3      5450 100 -0.9667  0.3360
    seasonSummer:landscape.positionUphill                                  -2879.1      5833 100 -0.4936  0.6227
    seasonFall:landscape.positionUphill                                      683.2      7602  29  0.0899  0.9290
    seasonWinter.early:landscape.positionUphill                              -85.9      6668  29 -0.0129  0.9898
    landscape.positionUphill:mycorrhizal.fungi.typeECM                     -9308.2      8237  29 -1.1301  0.2677
    seasonSpring:mycorrhizal.fungi.typeECM                                 -2131.9      6696 100 -0.3184  0.7508
    seasonSummer:mycorrhizal.fungi.typeECM                                 -3373.5      7089 100 -0.4759  0.6352
    seasonFall:mycorrhizal.fungi.typeECM                                   -4380.6      8102  29 -0.5407  0.5929
    seasonWinter.early:mycorrhizal.fungi.typeECM                          -10537.9      7656  29 -1.3764  0.1792
    seasonSpring:landscape.positionUphill:mycorrhizal.fungi.typeECM         4143.7      8980 100  0.4614  0.6455
    seasonSummer:landscape.positionUphill:mycorrhizal.fungi.typeECM         6900.9      9506 100  0.7260  0.4696
    seasonFall:landscape.positionUphill:mycorrhizal.fungi.typeECM          -2226.4     10911  29 -0.2041  0.8397
    seasonWinter.early:landscape.positionUphill:mycorrhizal.fungi.typeECM   1985.3     10386  29  0.1911  0.8497

``` r
anova(model.total)
```

                                                     numDF denDF F-value p-value
    (Intercept)                                          1    92     371  <.0001
    season                                               4    92       9  <.0001
    landscape.position                                   1    37      15  0.0004
    mycorrhizal.fungi.type                               1    37       3  0.0811
    season:landscape.position                            4    92       1  0.7350
    landscape.position:mycorrhizal.fungi.type            1    37       4  0.0468
    season:mycorrhizal.fungi.type                        4    92       2  0.1226
    season:landscape.position:mycorrhizal.fungi.type     4    92       0  0.8527

``` r
AIC(model.total)
```

    [1] 2856

``` r
rsquared(model.total)
```

             Response   family     link method Marginal Conditional
    1 total.nematodes gaussian identity   none    0.381       0.492

``` r
pairs(emmeans(model.total, specs= ~landscape.position*mycorrhizal.fungi.type))
```

     contrast                   estimate   SE df t.ratio p.value
     Downhill AM - Uphill AM       -8940 2140 29  -4.190  0.0010
     Downhill AM - Downhill ECM    -1007 2280 29  -0.440  0.9710
     Downhill AM - Uphill ECM      -2799 2100 29  -1.340  0.5490
     Uphill AM - Downhill ECM       7933 2310 29   3.430  0.0090
     Uphill AM - Uphill ECM         6141 2120 29   2.890  0.0340
     Downhill ECM - Uphill ECM     -1792 2270 29  -0.790  0.8590

    Results are averaged over the levels of: season 
    Degrees-of-freedom method: containment 
    P value adjustment: tukey method for comparing a family of 4 estimates 

<div class="figure">

<img src="Figures/Figure1_NematodeTotalAbundance.png" alt="Total Nematode Abundance Time Series" width="3600" />
<p class="caption">

Total Nematode Abundance Time Series
</p>

</div>

### Bacterial Feeding Nematodes

Bacterial feeder abundance was best explained by season.

``` r
model.bf <-lme(bacterial.feeders~season+landscape.position+season:landscape.position+
                 season:mycorrhizal.fungi.type+season:landscape.position:mycorrhizal.fungi.type, 
               random=list(sample.id=~1, tree.identity=~1), data = nematode)
summary(model.bf)$tTable
```

                                                                            Value Std.Error DF t-value p-value
    (Intercept)                                                            2128.2      1590 98  1.3383   0.184
    seasonSpring                                                            668.1      1754 98  0.3809   0.704
    seasonSummer                                                           2490.2      1901 98  1.3101   0.193
    seasonFall                                                             2289.9      2788 31  0.8214   0.418
    seasonWinter.early                                                     3546.2      2269 31  1.5626   0.128
    landscape.positionUphill                                               2105.8      2506 31  0.8402   0.407
    seasonSpring:landscape.positionUphill                                 -2114.0      2741 98 -0.7712   0.442
    seasonSummer:landscape.positionUphill                                 -1093.8      2957 98 -0.3700   0.712
    seasonFall:landscape.positionUphill                                     758.8      3871 31  0.1960   0.846
    seasonWinter.early:landscape.positionUphill                           -3053.8      3395 31 -0.8996   0.375
    seasonWinter.late:mycorrhizal.fungi.typeECM                            1979.2      3147 98  0.6289   0.531
    seasonSpring:mycorrhizal.fungi.typeECM                                  748.1      2025 98  0.3694   0.713
    seasonSummer:mycorrhizal.fungi.typeECM                                   63.3      1844 98  0.0343   0.973
    seasonFall:mycorrhizal.fungi.typeECM                                   -404.5      2661 31 -0.1520   0.880
    seasonWinter.early:mycorrhizal.fungi.typeECM                          -2564.0      2290 31 -1.1199   0.271
    seasonWinter.late:landscape.positionUphill:mycorrhizal.fungi.typeECM  -2340.7      4173 98 -0.5609   0.576
    seasonSpring:landscape.positionUphill:mycorrhizal.fungi.typeECM       -1232.8      2802 98 -0.4400   0.661
    seasonSummer:landscape.positionUphill:mycorrhizal.fungi.typeECM        2209.2      2547 98  0.8673   0.388
    seasonFall:landscape.positionUphill:mycorrhizal.fungi.typeECM         -2955.5      3656 31 -0.8084   0.425
    seasonWinter.early:landscape.positionUphill:mycorrhizal.fungi.typeECM  2146.4      3238 31  0.6629   0.512

``` r
anova(model.bf)
```

                                                     numDF denDF F-value p-value
    (Intercept)                                          1    90   147.6  <.0001
    season                                               4    90     3.0  0.0232
    landscape.position                                   1    39     1.7  0.2016
    season:landscape.position                            4    90     0.7  0.5631
    season:mycorrhizal.fungi.type                        5    90     0.6  0.6969
    season:landscape.position:mycorrhizal.fungi.type     5    90     0.5  0.7868

``` r
AIC(model.bf)
```

    [1] 2670

``` r
rsquared(model.bf)
```

               Response   family     link method Marginal Conditional
    1 bacterial.feeders gaussian identity   none    0.148       0.312

``` r
pairs(emmeans(model.bf, specs= ~season))
```

    NOTE: A nesting structure was detected in the fitted model:
        mycorrhizal.fungi.type %in% season

    NOTE: Results may be misleading due to involvement in interactions

     contrast                   estimate   SE df t.ratio p.value
     Winter.late - Spring            727 1130 98   0.640  0.9670
     Winter.late - Summer          -2123 1200 98  -1.760  0.4010
     Winter.late - Fall            -1324 1390 31  -0.950  0.8730
     Winter.late - Winter.early     -869 1320 31  -0.660  0.9640
     Spring - Summer               -2850  866 98  -3.290  0.0120
     Spring - Fall                 -2051 1150 31  -1.780  0.4020
     Spring - Winter.early         -1597 1070 31  -1.490  0.5750
     Summer - Fall                   799 1110 31   0.720  0.9510
     Summer - Winter.early          1253 1030 31   1.220  0.7420
     Fall - Winter.early             454 1220 31   0.370  0.9960

    Results are averaged over the levels of: landscape.position, mycorrhizal.fungi.type 
    Degrees-of-freedom method: containment 
    P value adjustment: tukey method for comparing a family of 5 estimates 

### Fungal Feeding Nematodes

Fungal feeder abundance was best explained by season.

``` r
model.ff <-lme(fungal.feeders~season+landscape.position+ season:landscape.position+mycorrhizal.fungi.type:landscape.position+
                 +season:landscape.position:mycorrhizal.fungi.type, 
               random=list(sample.id=~1, tree.identity=~1), data = nematode)
summary(model.ff)$tTable
```

                                                                               Value Std.Error  DF  t-value p-value
    (Intercept)                                                               74.243       176 100  0.42086   0.675
    seasonSpring                                                               0.156       176 100  0.00089   0.999
    seasonSummer                                                              47.931       209 100  0.22927   0.819
    seasonFall                                                               208.031       332  29  0.62594   0.536
    seasonWinter.early                                                       112.956       266  29  0.42455   0.674
    landscape.positionUphill                                                  79.329       277  29  0.28675   0.776
    seasonSpring:landscape.positionUphill                                   -195.584       276 100 -0.70935   0.480
    seasonSummer:landscape.positionUphill                                    -43.881       323 100 -0.13596   0.892
    seasonFall:landscape.positionUphill                                      570.657       451  29  1.26398   0.216
    seasonWinter.early:landscape.positionUphill                             -199.457       395  29 -0.50520   0.617
    landscape.positionDownhill:mycorrhizal.fungi.typeECM                     340.494       343  29  0.99171   0.330
    landscape.positionUphill:mycorrhizal.fungi.typeECM                      -146.584       301  29 -0.48642   0.630
    seasonSpring:landscape.positionDownhill:mycorrhizal.fungi.typeECM       -224.593       342 100 -0.65584   0.513
    seasonSummer:landscape.positionDownhill:mycorrhizal.fungi.typeECM       -217.399       386 100 -0.56270   0.575
    seasonFall:landscape.positionDownhill:mycorrhizal.fungi.typeECM          -47.806       474  29 -0.10088   0.920
    seasonWinter.early:landscape.positionDownhill:mycorrhizal.fungi.typeECM -245.604       444  29 -0.55304   0.584
    seasonSpring:landscape.positionUphill:mycorrhizal.fungi.typeECM          197.016       306 100  0.64323   0.522
    seasonSummer:landscape.positionUphill:mycorrhizal.fungi.typeECM            6.206       350 100  0.01776   0.986
    seasonFall:landscape.positionUphill:mycorrhizal.fungi.typeECM           -384.889       425  29 -0.90594   0.372
    seasonWinter.early:landscape.positionUphill:mycorrhizal.fungi.typeECM    324.051       412  29  0.78559   0.438

``` r
anova(model.ff)
```

                                                     numDF denDF F-value p-value
    (Intercept)                                          1    92   21.30  <.0001
    season                                               4    92    4.28  0.0032
    landscape.position                                   1    37    0.28  0.6008
    season:landscape.position                            4    92    0.31  0.8731
    landscape.position:mycorrhizal.fungi.type            2    37    1.08  0.3497
    season:landscape.position:mycorrhizal.fungi.type     8    92    0.56  0.8041

``` r
AIC(model.ff) 
```

    [1] 2056

``` r
rsquared(model.ff)
```

            Response   family     link method Marginal Conditional
    1 fungal.feeders gaussian identity   none    0.213       0.479

``` r
pairs(emmeans(model.ff, specs= ~season))
```

    NOTE: A nesting structure was detected in the fitted model:
        mycorrhizal.fungi.type %in% landscape.position

    NOTE: Results may be misleading due to involvement in interactions

     contrast                   estimate    SE  df t.ratio p.value
     Winter.late - Spring            105 115.0 100   0.910  0.8920
     Winter.late - Summer             27 130.0 100   0.210  1.0000
     Winter.late - Fall             -385 159.0  29  -2.420  0.1380
     Winter.late - Winter.early      -33 152.0  29  -0.220  0.9990
     Spring - Summer                 -78  91.5 100  -0.850  0.9140
     Spring - Fall                  -490 136.0  29  -3.610  0.0090
     Spring - Winter.early          -137 127.0  29  -1.080  0.8140
     Summer - Fall                  -412 134.0  29  -3.080  0.0330
     Summer - Winter.early           -60 124.0  29  -0.480  0.9890
     Fall - Winter.early             352 149.0  29   2.370  0.1540

    Results are averaged over the levels of: mycorrhizal.fungi.type, landscape.position 
    Degrees-of-freedom method: containment 
    P value adjustment: tukey method for comparing a family of 5 estimates 

### Plant Parasitic Nematodes

Best explained by season and the interaction between topography and tree
functional type.

``` r
model.pp <-lme(plant.parasites.aph.~season+landscape.position+mycorrhizal.fungi.type + season:landscape.position+mycorrhizal.fungi.type:landscape.position+
                  season:mycorrhizal.fungi.type+season:landscape.position:mycorrhizal.fungi.type, 
                random=list(sample.id=~1, tree.identity=~1), data = nematode)
summary(model.pp)$tTable
```

                                                                          Value Std.Error  DF t-value p-value
    (Intercept)                                                            2143      2322 100  0.9228  0.3583
    seasonSpring                                                           -174      2614 100 -0.0665  0.9471
    seasonSummer                                                           2831      2774 100  1.0207  0.3098
    seasonFall                                                             5535      4010  29  1.3802  0.1781
    seasonWinter.early                                                     8656      3277  29  2.6417  0.0132
    landscape.positionUphill                                               7295      3663  29  1.9918  0.0559
    mycorrhizal.fungi.typeECM                                              2318      4607  29  0.5031  0.6187
    seasonSpring:landscape.positionUphill                                 -2363      4082 100 -0.5790  0.5639
    seasonSummer:landscape.positionUphill                                 -1480      4320 100 -0.3426  0.7326
    seasonFall:landscape.positionUphill                                    -606      5597  29 -0.1084  0.9145
    seasonWinter.early:landscape.positionUphill                            3052      4910  29  0.6215  0.5391
    landscape.positionUphill:mycorrhizal.fungi.typeECM                    -5553      6106  29 -0.9094  0.3706
    seasonSpring:mycorrhizal.fungi.typeECM                                 -358      5008 100 -0.0714  0.9432
    seasonSummer:mycorrhizal.fungi.typeECM                                 -632      5259 100 -0.1203  0.9045
    seasonFall:mycorrhizal.fungi.typeECM                                  -1480      5973  29 -0.2478  0.8060
    seasonWinter.early:mycorrhizal.fungi.typeECM                          -5279      5650  29 -0.9343  0.3578
    seasonSpring:landscape.positionUphill:mycorrhizal.fungi.typeECM        1735      6716 100  0.2584  0.7967
    seasonSummer:landscape.positionUphill:mycorrhizal.fungi.typeECM        1166      7048 100  0.1655  0.8689
    seasonFall:landscape.positionUphill:mycorrhizal.fungi.typeECM         -2129      8047  29 -0.2645  0.7932
    seasonWinter.early:landscape.positionUphill:mycorrhizal.fungi.typeECM -3191      7659  29 -0.4166  0.6800

``` r
anova(model.pp)
```

                                                     numDF denDF F-value p-value
    (Intercept)                                          1    92   231.6  <.0001
    season                                               4    92     7.5  <.0001
    landscape.position                                   1    37    14.8  0.0005
    mycorrhizal.fungi.type                               1    37     4.7  0.0370
    season:landscape.position                            4    92     0.5  0.7535
    landscape.position:mycorrhizal.fungi.type            1    37     7.1  0.0115
    season:mycorrhizal.fungi.type                        4    92     1.8  0.1413
    season:landscape.position:mycorrhizal.fungi.type     4    92     0.2  0.9276

``` r
pairs(emmeans(model.pp, specs= ~landscape.position*mycorrhizal.fungi.type))
```

     contrast                   estimate   SE df t.ratio p.value
     Downhill AM - Uphill AM       -7016 1560 29  -4.490  0.0010
     Downhill AM - Downhill ECM     -768 1670 29  -0.460  0.9670
     Downhill AM - Uphill ECM      -1747 1530 29  -1.140  0.6690
     Uphill AM - Downhill ECM       6247 1690 29   3.690  0.0050
     Uphill AM - Uphill ECM         5268 1560 29   3.390  0.0100
     Downhill ECM - Uphill ECM      -979 1670 29  -0.590  0.9350

    Results are averaged over the levels of: season 
    Degrees-of-freedom method: containment 
    P value adjustment: tukey method for comparing a family of 4 estimates 

``` r
AIC(model.pp)
```

    [1] 2777

``` r
rsquared(model.pp)
```

                  Response   family     link method Marginal Conditional
    1 plant.parasites.aph. gaussian identity   none     0.38       0.481

``` r
pairs(emmeans(model.pp, specs= ~season))
```

     contrast                   estimate   SE  df t.ratio p.value
     Winter.late - Spring           1100 1680 100   0.660  0.9650
     Winter.late - Summer          -2066 1760 100  -1.170  0.7670
     Winter.late - Fall            -3959 2010  29  -1.970  0.3060
     Winter.late - Winter.early    -6745 1920  29  -3.520  0.0120
     Spring - Summer               -3167 1280 100  -2.480  0.1030
     Spring - Fall                 -5060 1660  29  -3.050  0.0370
     Spring - Winter.early         -7845 1540  29  -5.090  <.0001
     Summer - Fall                 -1893 1600  29  -1.180  0.7610
     Summer - Winter.early         -4679 1480  29  -3.170  0.0270
     Fall - Winter.early           -2786 1750  29  -1.590  0.5130

    Results are averaged over the levels of: landscape.position, mycorrhizal.fungi.type 
    Degrees-of-freedom method: containment 
    P value adjustment: tukey method for comparing a family of 5 estimates 

### Omnivorous and Predatory Nematodes

A new group created with the sum of omnivorous and predatory nematodes.
This group was best explained by season and topography.

``` r
model.pred1 <-lme(om.pr~ season+landscape.position+ mycorrhizal.fungi.type + season:landscape.position + mycorrhizal.fungi.type:landscape.position + season:mycorrhizal.fungi.type + season:mycorrhizal.fungi.type:landscape.position, 
                  random=list(sample.id=~1, tree.identity=~1), data = nematode)
summary(model.pred1)$tTable
```

                                                                           Value Std.Error  DF t-value p-value
    (Intercept)                                                            175.8       204 100  0.8633  0.3900
    seasonSpring                                                           -29.7       257 100 -0.1156  0.9082
    seasonSummer                                                           359.4       239 100  1.5037  0.1358
    seasonFall                                                             417.8       323  29  1.2942  0.2058
    seasonWinter.early                                                     623.4       270  29  2.3097  0.0282
    landscape.positionUphill                                               402.2       322  29  1.2492  0.2216
    mycorrhizal.fungi.typeECM                                              186.6       407  29  0.4582  0.6502
    seasonSpring:landscape.positionUphill                                 -155.5       398 100 -0.3903  0.6971
    seasonSummer:landscape.positionUphill                                  260.8       374 100  0.6965  0.4877
    seasonFall:landscape.positionUphill                                    -19.9       465  29 -0.0428  0.9662
    seasonWinter.early:landscape.positionUphill                            386.5       408  29  0.9474  0.3513
    landscape.positionUphill:mycorrhizal.fungi.typeECM                    -450.5       539  29 -0.8362  0.4099
    seasonSpring:mycorrhizal.fungi.typeECM                                 -87.9       481 100 -0.1828  0.8553
    seasonSummer:mycorrhizal.fungi.typeECM                                -277.2       461 100 -0.6010  0.5492
    seasonFall:mycorrhizal.fungi.typeECM                                    81.7       501  29  0.1629  0.8717
    seasonWinter.early:mycorrhizal.fungi.typeECM                          -230.9       478  29 -0.4829  0.6328
    seasonSpring:landscape.positionUphill:mycorrhizal.fungi.typeECM        448.9       645 100  0.6955  0.4884
    seasonSummer:landscape.positionUphill:mycorrhizal.fungi.typeECM        446.0       616 100  0.7246  0.4704
    seasonFall:landscape.positionUphill:mycorrhizal.fungi.typeECM           48.2       679  29  0.0709  0.9440
    seasonWinter.early:landscape.positionUphill:mycorrhizal.fungi.typeECM  -72.0       645  29 -0.1117  0.9118

``` r
anova(model.pred1)
```

                                                     numDF denDF F-value p-value
    (Intercept)                                          1    92     317  <.0001
    season                                               4    92      12  <.0001
    landscape.position                                   1    37      25  <.0001
    mycorrhizal.fungi.type                               1    37       0   0.488
    season:landscape.position                            4    92       2   0.141
    landscape.position:mycorrhizal.fungi.type            1    37       2   0.189
    season:mycorrhizal.fungi.type                        4    92       1   0.538
    season:landscape.position:mycorrhizal.fungi.type     4    92       1   0.736

``` r
AIC(model.pred1) #2145
```

    [1] 2145

``` r
AICc(model.pred1) #2153
```

    [1] 2153

``` r
rsquared(model.pred1) #37
```

      Response   family     link method Marginal Conditional
    1    om.pr gaussian identity   none    0.365       0.368

``` r
pairs(emmeans(model.pred1, specs= ~season))
```

    NOTE: Results may be misleading due to involvement in interactions

     contrast                   estimate  SE  df t.ratio p.value
     Winter.late - Spring             39 161 100   0.240  0.9990
     Winter.late - Summer           -463 154 100  -3.010  0.0270
     Winter.late - Fall             -461 170  29  -2.710  0.0760
     Winter.late - Winter.early     -683 161  29  -4.240  0.0020
     Spring - Summer                -502 116 100  -4.320  <.0001
     Spring - Fall                  -500 137  29  -3.650  0.0080
     Spring - Winter.early          -722 126  29  -5.730  <.0001
     Summer - Fall                     2 127  29   0.010  1.0000
     Summer - Winter.early          -221 116  29  -1.910  0.3370
     Fall - Winter.early            -222 136  29  -1.630  0.4890

    Results are averaged over the levels of: landscape.position, mycorrhizal.fungi.type 
    Degrees-of-freedom method: containment 
    P value adjustment: tukey method for comparing a family of 5 estimates 

<div class="figure">

<img src="Figures/Figure2_NematodeFeedingGuilds.png" alt="Nematode Trophic Groups by Season" width="3600" />
<p class="caption">

Nematode Trophic Groups by Season
</p>

</div>

### Nematode Diversity (Simpson Diversity Index)

Simpson diversity was best explained by tree functional type and season

``` r
worm.simp1 <-lme(simpson~season+landscape.position+mycorrhizal.fungi.type +
                  season:landscape.position+mycorrhizal.fungi.type:landscape.position+
                  season:mycorrhizal.fungi.type+season:landscape.position:mycorrhizal.fungi.type, 
                  random=list(sample.id=~1, tree.identity=~1), data = nematode)
summary(worm.simp1)$tTable
```

    ##                                                                          Value Std.Error  DF t-value  p-value
    ## (Intercept)                                                            0.51920    0.0386 100 13.4546 3.60e-24
    ## seasonSpring                                                          -0.01228    0.0482 100 -0.2547 7.99e-01
    ## seasonSummer                                                           0.00101    0.0454 100  0.0223 9.82e-01
    ## seasonFall                                                             0.00670    0.0616  29  0.1088 9.14e-01
    ## seasonWinter.early                                                    -0.09316    0.0514  29 -1.8119 8.04e-02
    ## landscape.positionUphill                                              -0.04870    0.0610  29 -0.7982 4.31e-01
    ## mycorrhizal.fungi.typeECM                                              0.05998    0.0772  29  0.7772 4.43e-01
    ## seasonSpring:landscape.positionUphill                                 -0.01960    0.0748 100 -0.2622 7.94e-01
    ## seasonSummer:landscape.positionUphill                                  0.04781    0.0711 100  0.6724 5.03e-01
    ## seasonFall:landscape.positionUphill                                    0.01313    0.0885  29  0.1483 8.83e-01
    ## seasonWinter.early:landscape.positionUphill                            0.03137    0.0777  29  0.4039 6.89e-01
    ## landscape.positionUphill:mycorrhizal.fungi.typeECM                    -0.05196    0.1021  29 -0.5089 6.15e-01
    ## seasonSpring:mycorrhizal.fungi.typeECM                                -0.01799    0.0904 100 -0.1989 8.43e-01
    ## seasonSummer:mycorrhizal.fungi.typeECM                                -0.05997    0.0875 100 -0.6852 4.95e-01
    ## seasonFall:mycorrhizal.fungi.typeECM                                  -0.05106    0.0954  29 -0.5353 5.97e-01
    ## seasonWinter.early:mycorrhizal.fungi.typeECM                          -0.01328    0.0907  29 -0.1465 8.85e-01
    ## seasonSpring:landscape.positionUphill:mycorrhizal.fungi.typeECM        0.06299    0.1214 100  0.5190 6.05e-01
    ## seasonSummer:landscape.positionUphill:mycorrhizal.fungi.typeECM        0.07877    0.1168 100  0.6742 5.02e-01
    ## seasonFall:landscape.positionUphill:mycorrhizal.fungi.typeECM          0.08175    0.1292  29  0.6329 5.32e-01
    ## seasonWinter.early:landscape.positionUphill:mycorrhizal.fungi.typeECM  0.09090    0.1225  29  0.7421 4.64e-01

``` r
anova(worm.simp1)
```

    ##                                                  numDF denDF F-value p-value
    ## (Intercept)                                          1    92    4242  <.0001
    ## season                                               4    92       3  0.0106
    ## landscape.position                                   1    37       1  0.2702
    ## mycorrhizal.fungi.type                               1    37       5  0.0341
    ## season:landscape.position                            4    92       1  0.4118
    ## landscape.position:mycorrhizal.fungi.type            1    37       0  0.5689
    ## season:mycorrhizal.fungi.type                        4    92       0  0.8001
    ## season:landscape.position:mycorrhizal.fungi.type     4    92       0  0.9605

``` r
AIC(worm.simp1)
```

    ## [1] -172

``` r
rsquared(worm.simp1)
```

    ##   Response   family     link method Marginal Conditional
    ## 1  simpson gaussian identity   none    0.153       0.167

### Predator-Prey Index

No significant interactions, but best model contains season only.

``` r
model.predator.prey.total <-lme(trophic.index~season, 
                                random=list(sample.id=~1, tree.identity=~1), data = nematode)
summary(model.predator.prey.total)$tTable
```

    ##                      Value Std.Error  DF t-value p-value
    ## (Intercept)        0.04046    0.0145 106   2.783 0.00638
    ## seasonSpring       0.00594    0.0165 106   0.361 0.71885
    ## seasonSummer       0.02650    0.0170 106   1.559 0.12209
    ## seasonFall         0.04128    0.0190  38   2.170 0.03634
    ## seasonWinter.early 0.02611    0.0186  38   1.406 0.16785

``` r
anova(model.predator.prey.total)
```

    ##             numDF denDF F-value p-value
    ## (Intercept)     1   104   143.8  <.0001
    ## season          4   104     1.8   0.138

``` r
rsquared(model.predator.prey.total) #18
```

    ##        Response   family     link method Marginal Conditional
    ## 1 trophic.index gaussian identity   none   0.0549       0.178

``` r
AIC(model.predator.prey.total) #-413
```

    ## [1] -413

``` r
AICc(model.predator.prey.total) #-412
```

    ## [1] -412

``` r
pairs(emmeans(model.predator.prey.total, "season"))
```

    ##  contrast                   estimate     SE  df t.ratio p.value
    ##  Winter.late - Spring        -0.0059 0.0165 106  -0.361  0.9960
    ##  Winter.late - Summer        -0.0265 0.0170 106  -1.559  0.5270
    ##  Winter.late - Fall          -0.0413 0.0190  38  -2.170  0.2130
    ##  Winter.late - Winter.early  -0.0261 0.0186  38  -1.406  0.6280
    ##  Spring - Summer             -0.0206 0.0130 106  -1.583  0.5120
    ##  Spring - Fall               -0.0353 0.0160  38  -2.206  0.1990
    ##  Spring - Winter.early       -0.0202 0.0155  38  -1.303  0.6910
    ##  Summer - Fall               -0.0148 0.0152  38  -0.970  0.8670
    ##  Summer - Winter.early        0.0004 0.0147  38   0.026  1.0000
    ##  Fall - Winter.early          0.0152 0.0169  38   0.900  0.8950
    ## 
    ## Degrees-of-freedom method: containment 
    ## P value adjustment: tukey method for comparing a family of 5 estimates

### Total Plant Parasite to Free Living Nematodes

Best explained by season and the interaction between topography and tree
functional type.

``` r
model.lifestyle.total <-lme(lifestyle.index~season+landscape.position+ mycorrhizal.fungi.type +
                              season:landscape.position+mycorrhizal.fungi.type:landscape.position,
                            random=list(sample.id=~1, tree.identity=~1), na.action = na.omit, data = nematode)
summary(model.lifestyle.total)$tTable
```

    ##                                                      Value Std.Error  DF t-value p-value
    ## (Intercept)                                         1.0399     0.543 104   1.916  0.0582
    ## seasonSpring                                       -0.2316     0.657 104  -0.352  0.7252
    ## seasonSummer                                        0.1827     0.619 104   0.295  0.7684
    ## seasonFall                                          0.6564     0.686  33   0.957  0.3453
    ## seasonWinter.early                                  1.4364     0.662  33   2.170  0.0373
    ## landscape.positionUphill                            1.4238     0.782  33   1.821  0.0777
    ## mycorrhizal.fungi.typeECM                           0.0982     0.362  33   0.271  0.7878
    ## seasonSpring:landscape.positionUphill               0.5308     0.928 104   0.572  0.5687
    ## seasonSummer:landscape.positionUphill              -0.8423     0.875 104  -0.963  0.3378
    ## seasonFall:landscape.positionUphill                -0.6586     0.974  33  -0.676  0.5036
    ## seasonWinter.early:landscape.positionUphill        -0.4268     0.932  33  -0.458  0.6499
    ## landscape.positionUphill:mycorrhizal.fungi.typeECM -1.0502     0.502  33  -2.092  0.0442

``` r
anova(model.lifestyle.total)
```

    ##                                           numDF denDF F-value p-value
    ## (Intercept)                                   1   100   211.7  <.0001
    ## season                                        4   100     4.9  0.0012
    ## landscape.position                            1    37     4.9  0.0335
    ## mycorrhizal.fungi.type                        1    37     2.8  0.1004
    ## season:landscape.position                     4   100     1.3  0.2887
    ## landscape.position:mycorrhizal.fungi.type     1    37     4.4  0.0434

``` r
rsquared(model.lifestyle.total) #19
```

    ##          Response   family     link method Marginal Conditional
    ## 1 lifestyle.index gaussian identity   none    0.192       0.192

``` r
AIC(model.lifestyle.total) #587
```

    ## [1] 587

``` r
AICc(model.lifestyle.total) #591
```

    ## [1] 591

### Total Channel Ratio (Fungal Feeding : Bacterial Feeding Nematodes)

Best explained by season.

``` r
model.channel.total <-lme(channel.index~season,
                          random=list(sample.id=~1, tree.identity=~1), na.action = na.omit, data = nematode)
summary(model.channel.total)$tTable
```

    ##                      Value Std.Error  DF t-value p-value
    ## (Intercept)         0.0397    0.0331 106   1.200 0.23273
    ## seasonSpring       -0.0211    0.0375 106  -0.562 0.57503
    ## seasonSummer       -0.0118    0.0387 106  -0.305 0.76112
    ## seasonFall          0.1508    0.0432  38   3.487 0.00125
    ## seasonWinter.early  0.0320    0.0422  38   0.759 0.45246

``` r
anova(model.channel.total)
```

    ##             numDF denDF F-value p-value
    ## (Intercept)     1   104   30.61  <.0001
    ## season          4   104    6.78   1e-04

``` r
rsquared(model.channel.total) #29
```

    ##        Response   family     link method Marginal Conditional
    ## 1 channel.index gaussian identity   none    0.189       0.292

``` r
AIC(model.channel.total) #-166
```

    ## [1] -166

``` r
AICc(model.channel.total) #-165
```

    ## [1] -165

<div class="figure">

<img src="Figures/Figure3_NematodeIndices.png" alt="Nematode Indices by Season" width="3600" />
<p class="caption">

Nematode Indices by Season
</p>

</div>

### Supplemental Figure With other interactions

These are other significant interactions in the models that are
highlighted in the results section.

<div class="figure">

<img src="Figures/SupplementaryFigure2_NematodeTrends.png" alt="Other Signficant Interactions in Nematode Models" width="3600" />
<p class="caption">

Other Signficant Interactions in Nematode Models
</p>

</div>

### Nematode Community Composition

The matrix used for this was using nematode relative abundance
(calculated from Total Nematodes, so accounting for unknown nematodes in
the overall community)

``` r
#Running ANOVA of NMDS
adonis2(nematode_matrix.rel ~ season * landscape.position * mycorrhizal.fungi.type, data=nematode,
        permutations=999, by = "terms")
```

    ## Permutation test for adonis under reduced model
    ## Terms added sequentially (first to last)
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## adonis2(formula = nematode_matrix.rel ~ season * landscape.position * mycorrhizal.fungi.type, data = nematode, permutations = 999, by = "terms")
    ##                                                   Df SumOfSqs    R2     F Pr(>F)    
    ## season                                             4     0.62 0.145  7.08  0.001 ***
    ## landscape.position                                 1     0.22 0.052 10.23  0.001 ***
    ## mycorrhizal.fungi.type                             1     0.02 0.005  0.88  0.356    
    ## season:landscape.position                          4     0.27 0.062  3.03  0.018 *  
    ## season:mycorrhizal.fungi.type                      4     0.03 0.008  0.39  0.902    
    ## landscape.position:mycorrhizal.fungi.type          1     0.15 0.035  6.87  0.006 ** 
    ## season:landscape.position:mycorrhizal.fungi.type   4     0.02 0.004  0.22  0.988    
    ## Residual                                         135     2.96 0.689                 
    ## Total                                            154     4.29 1.000                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Season and landscape position signficant
# landscape position and mycorrhizal fungi
# season and landscape position alone

adonis2(nematode_matrix.rel ~ PRECIP + AIRTEMP + soil.moisture, data=nematode,
        permutations=999, by = "terms")
```

    ## Permutation test for adonis under reduced model
    ## Terms added sequentially (first to last)
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## adonis2(formula = nematode_matrix.rel ~ PRECIP + AIRTEMP + soil.moisture, data = nematode, permutations = 999, by = "terms")
    ##                Df SumOfSqs    R2    F Pr(>F)   
    ## PRECIP          1     0.13 0.030 5.01  0.013 * 
    ## AIRTEMP         1     0.18 0.042 6.85  0.004 **
    ## soil.moisture   1     0.05 0.011 1.75  0.180   
    ## Residual      151     3.94 0.917               
    ## Total         154     4.29 1.000               
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# precipitation and air temp also drive community composition!
```

<div class="figure">

<img src="Figures/Figure4_NematodeCommunity.png" alt="NMDS of Nematode Community Composition" width="4500" />
<p class="caption">

NMDS of Nematode Community Composition
</p>

</div>

<div class="figure">

<img src="Figures/SupplementalFigure3_NematodeCommunitybyClimate.png" alt="NMDS of Nematode Community Composition by Climate" width="4500" />
<p class="caption">

NMDS of Nematode Community Composition by Climate
</p>

</div>

## Macrofauna

Initial models included season, sample type (ecological group),
landscape position (topography), and mycorhizal type (tree functional
type) + all interactions

### Macrofauna Total Abundance

Abundance driven by season, ecological group, and interaction between
topography and tree functional type

``` r
# model with sample type
bug.total <-lme(sqrt(totalAbundance)~ season + sample.type + season:landscape.position +
                  mycorrhizal.fungi.type:landscape.position, 
                random=list(tree.identity=~1, sample.id=~1), data = macrofauna)
summary(bug.total) 
```

    ## Linear mixed-effects model fit by REML
    ##   Data: macrofauna 
    ##   AIC BIC logLik
    ##   257 278   -113
    ## 
    ## Random effects:
    ##  Formula: ~1 | tree.identity
    ##         (Intercept)
    ## StdDev:    0.000192
    ## 
    ##  Formula: ~1 | sample.id %in% tree.identity
    ##         (Intercept) Residual
    ## StdDev:     0.00227       11
    ## 
    ## Fixed effects:  sqrt(totalAbundance) ~ season + sample.type + season:landscape.position +      mycorrhizal.fungi.type:landscape.position 
    ##                                                       Value Std.Error DF t-value p-value
    ## (Intercept)                                           12.85      6.27 16    2.05  0.0572
    ## seasonSpring                                          11.15      7.78  2    1.43  0.2882
    ## seasonSummer                                          16.48      7.78  2    2.12  0.1682
    ## seasonFall                                             5.97      7.78 16    0.77  0.4536
    ## seasonWinter.early                                     4.14      7.78 16    0.53  0.6017
    ## sample.typeSoil                                       23.47      3.48 16    6.75  0.0000
    ## seasonWinter.late:landscape.positionUphill            -4.98      8.61  2   -0.58  0.6211
    ## seasonSpring:landscape.positionUphill                -16.07      8.61  2   -1.87  0.2030
    ## seasonSummer:landscape.positionUphill                 -6.61      8.16  9   -0.81  0.4384
    ## seasonFall:landscape.positionUphill                   11.73      8.61 16    1.36  0.1917
    ## seasonWinter.early:landscape.positionUphill          -11.55      8.61 16   -1.34  0.1982
    ## landscape.positionDownhill:mycorrhizal.fungi.typeECM  -7.84      4.92  2   -1.59  0.2520
    ## landscape.positionUphill:mycorrhizal.fungi.typeECM    13.31      5.50  2    2.42  0.1366
    ##  Correlation: 
    ##                                                      (Intr) ssnSpr ssnSmm ssnFll ssnWn. smpl.S ssnWntr.l:.U ssnSp:.U ssnSm:.U ssF:.U ssnWntr.r:.U l.D:..
    ## seasonSpring                                         -0.620                                                                                             
    ## seasonSummer                                         -0.620  0.500                                                                                      
    ## seasonFall                                           -0.620  0.500  0.500                                                                               
    ## seasonWinter.early                                   -0.620  0.500  0.500  0.500                                                                        
    ## sample.typeSoil                                      -0.277  0.000  0.000  0.000  0.000                                                                 
    ## seasonWinter.late:landscape.positionUphill           -0.672  0.452  0.452  0.452  0.452  0.000                                                          
    ## seasonSpring:landscape.positionUphill                -0.112 -0.452  0.000  0.000  0.000  0.000  0.184                                                   
    ## seasonSummer:landscape.positionUphill                -0.118  0.000 -0.477  0.000  0.000  0.000  0.086        0.086                                      
    ## seasonFall:landscape.positionUphill                  -0.112  0.000  0.000 -0.452  0.000  0.000  0.184        0.184    0.086                             
    ## seasonWinter.early:landscape.positionUphill          -0.112  0.000  0.000  0.000 -0.452  0.000  0.184        0.184    0.086    0.184                    
    ## landscape.positionDownhill:mycorrhizal.fungi.typeECM -0.392  0.000  0.000  0.000  0.000  0.000  0.286        0.286    0.302    0.286  0.286             
    ## landscape.positionUphill:mycorrhizal.fungi.typeECM    0.000  0.000  0.000  0.000  0.000  0.000 -0.319       -0.319    0.000   -0.319 -0.319        0.000
    ## 
    ## Standardized Within-Group Residuals:
    ##     Min      Q1     Med      Q3     Max 
    ## -2.0483 -0.6084  0.0194  0.3546  2.1949 
    ## 
    ## Number of Observations: 40
    ## Number of Groups: 
    ##                tree.identity sample.id %in% tree.identity 
    ##                           11                           19

``` r
anova(bug.total)
```

    ##                                           numDF denDF F-value p-value
    ## (Intercept)                                   1    11   299.3  <.0001
    ## season                                        4    11     2.8  0.0784
    ## sample.type                                   1    11    45.5  <.0001
    ## season:landscape.position                     5    11     1.9  0.1786
    ## landscape.position:mycorrhizal.fungi.type     2     6     4.2  0.0724

``` r
rsquared(bug.total)
```

    ##         Response   family     link method Marginal Conditional
    ## 1 totalAbundance gaussian identity   none    0.657       0.657

``` r
AIC(bug.total) #257
```

    ## [1] 257

``` r
AICc(bug.total) #281
```

    ## [1] 281

``` r
pairs(emmeans(bug.total, specs= ~season))
```

    ## NOTE: A nesting structure was detected in the fitted model:
    ##     landscape.position %in% season, mycorrhizal.fungi.type %in% (season*landscape.position)

    ## NOTE: Results may be misleading due to involvement in interactions

    ##  contrast                   estimate  SE df t.ratio p.value
    ##  Winter.late - Spring          -5.60 5.5  2  -1.019  0.8340
    ##  Winter.late - Summer         -12.34 5.5  2  -2.244  0.4110
    ##  Winter.late - Fall           -14.33 5.5  2  -2.606  0.3340
    ##  Winter.late - Winter.early    -0.86 5.5  2  -0.156  1.0000
    ##  Spring - Summer               -6.73 5.5  2  -1.224  0.7520
    ##  Spring - Fall                 -8.73 5.5  2  -1.587  0.6110
    ##  Spring - Winter.early          4.75 5.5  2   0.863  0.8900
    ##  Summer - Fall                 -1.99 5.5  2  -0.362  0.9930
    ##  Summer - Winter.early         11.48 5.5  2   2.088  0.4510
    ##  Fall - Winter.early           13.48 5.5 16   2.450  0.1520
    ## 
    ## Results are averaged over the levels of: sample.type, mycorrhizal.fungi.type, landscape.position 
    ## Note: contrasts are still on the sqrt scale. Consider using
    ##       regrid() if you want contrasts of back-transformed estimates. 
    ## Degrees-of-freedom method: containment 
    ## P value adjustment: tukey method for comparing a family of 5 estimates

<div class="figure">

<img src="Figures/Figure5_MacrofaunaTotalAbundance.png" alt="Total Macrofauna Abundance By Season" width="3600" />
<p class="caption">

Total Macrofauna Abundance By Season
</p>

</div>

### Macrofauna Shannon Diversity

Driven entirely by ecological group

``` r
bug.shan <-lme(shannon~ sample.type, 
               random=list(tree.identity=~1, sample.id=~1), data = macrofauna)
summary(bug.shan)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: macrofauna 
    ##    AIC  BIC logLik
    ##   41.9 50.1    -16
    ## 
    ## Random effects:
    ##  Formula: ~1 | tree.identity
    ##         (Intercept)
    ## StdDev:       0.189
    ## 
    ##  Formula: ~1 | sample.id %in% tree.identity
    ##         (Intercept) Residual
    ## StdDev:      0.0948    0.294
    ## 
    ## Fixed effects:  shannon ~ sample.type 
    ##                 Value Std.Error DF t-value p-value
    ## (Intercept)     1.311    0.0916 20   14.32  0.0000
    ## sample.typeSoil 0.325    0.0930 20    3.49  0.0023
    ##  Correlation: 
    ##                 (Intr)
    ## sample.typeSoil -0.508
    ## 
    ## Standardized Within-Group Residuals:
    ##     Min      Q1     Med      Q3     Max 
    ## -2.9796 -0.4680  0.0439  0.5497  2.2599 
    ## 
    ## Number of Observations: 40
    ## Number of Groups: 
    ##                tree.identity sample.id %in% tree.identity 
    ##                           11                           19

``` r
anova(bug.shan)
```

    ##             numDF denDF F-value p-value
    ## (Intercept)     1    20     349  <.0001
    ## sample.type     1    20      12  0.0023

``` r
rsquared(bug.shan)
```

    ##   Response   family     link method Marginal Conditional
    ## 1  shannon gaussian identity   none    0.171       0.453

``` r
AIC(bug.shan) #42 
```

    ## [1] 41.9

``` r
AICc(bug.shan) #44
```

    ## [1] 43.7

<div class="figure">

<img src="Figures/Figure6_MacrofaunaSampleType.png" alt="Composition by Sample Type" width="3600" />
<p class="caption">

Composition by Sample Type
</p>

</div>

## Macrofauna By Sample Type

Models included season, topography, and tree functional type + all
interactions

### Epigeic Abundance

No significant interactions in any model - this is the best using AIC

``` r
litter.total <-lme(sqrt(totalAbundance)~ landscape.position + mycorrhizal.fungi.type +
                     mycorrhizal.fungi.type:landscape.position, 
                   random=list(tree.identity=~1, sample.id=~1), data = macrofauna[macrofauna$sample.type == "Litter",])
summary(litter.total)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: macrofauna[macrofauna$sample.type == "Litter", ] 
    ##   AIC BIC logLik
    ##   144 149  -64.9
    ## 
    ## Random effects:
    ##  Formula: ~1 | tree.identity
    ##         (Intercept)
    ## StdDev:        8.59
    ## 
    ##  Formula: ~1 | sample.id %in% tree.identity
    ##         (Intercept) Residual
    ## StdDev:     0.00108     9.34
    ## 
    ## Fixed effects:  sqrt(totalAbundance) ~ landscape.position + mycorrhizal.fungi.type +      mycorrhizal.fungi.type:landscape.position 
    ##                                                    Value Std.Error DF t-value p-value
    ## (Intercept)                                        17.44      6.54  9   2.666  0.0258
    ## landscape.positionUphill                            5.18      8.10  6   0.639  0.5462
    ## mycorrhizal.fungi.typeECM                          -3.28      8.86  9  -0.370  0.7200
    ## landscape.positionUphill:mycorrhizal.fungi.typeECM -0.44     11.42  6  -0.038  0.9707
    ##  Correlation: 
    ##                                                    (Intr) lnds.U m..ECM
    ## landscape.positionUphill                           -0.714              
    ## mycorrhizal.fungi.typeECM                          -0.739  0.527       
    ## landscape.positionUphill:mycorrhizal.fungi.typeECM  0.507 -0.709 -0.614
    ## 
    ## Standardized Within-Group Residuals:
    ##    Min     Q1    Med     Q3    Max 
    ## -1.219 -0.427 -0.146  0.187  2.081 
    ## 
    ## Number of Observations: 20
    ## Number of Groups: 
    ##                tree.identity sample.id %in% tree.identity 
    ##                           11                           19

``` r
anova(litter.total)
```

    ##                                           numDF denDF F-value p-value
    ## (Intercept)                                   1     9   29.02  0.0004
    ## landscape.position                            1     6    0.93  0.3721
    ## mycorrhizal.fungi.type                        1     9    0.25  0.6303
    ## landscape.position:mycorrhizal.fungi.type     1     6    0.00  0.9707

``` r
rsquared(litter.total)
```

    ##         Response   family     link method Marginal Conditional
    ## 1 totalAbundance gaussian identity   none   0.0619       0.492

``` r
AIC(litter.total) #144
```

    ## [1] 144

``` r
AICc(litter.total) #153
```

    ## [1] 153

### Epigeic Diversity (Shannon)

Topography weakly trending towards significant (p=0.08)

``` r
litter.shan <-lme(shannon~ landscape.position, 
                  random=list(tree.identity=~1, sample.id=~1), data = macrofauna[macrofauna$sample.type == "Litter",])
summary(litter.shan)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: macrofauna[macrofauna$sample.type == "Litter", ] 
    ##   AIC  BIC logLik
    ##    29 33.4  -9.48
    ## 
    ## Random effects:
    ##  Formula: ~1 | tree.identity
    ##         (Intercept)
    ## StdDev:       0.464
    ## 
    ##  Formula: ~1 | sample.id %in% tree.identity
    ##         (Intercept) Residual
    ## StdDev:       0.203    0.075
    ## 
    ## Fixed effects:  shannon ~ landscape.position 
    ##                           Value Std.Error DF t-value p-value
    ## (Intercept)               1.464     0.170 10    8.61  0.0000
    ## landscape.positionUphill -0.343     0.171  7   -2.01  0.0849
    ##  Correlation: 
    ##                          (Intr)
    ## landscape.positionUphill -0.475
    ## 
    ## Standardized Within-Group Residuals:
    ##     Min      Q1     Med      Q3     Max 
    ## -0.6168 -0.1412 -0.0257  0.2080  0.7797 
    ## 
    ## Number of Observations: 20
    ## Number of Groups: 
    ##                tree.identity sample.id %in% tree.identity 
    ##                           11                           19

``` r
anova(litter.shan)
```

    ##                    numDF denDF F-value p-value
    ## (Intercept)            1    10    75.6  <.0001
    ## landscape.position     1     7     4.0  0.0849

``` r
rsquared(litter.shan)
```

    ##   Response   family     link method Marginal Conditional
    ## 1  shannon gaussian identity   none    0.106       0.981

``` r
AIC(litter.shan) #29
```

    ## [1] 29

``` r
AICc(litter.shan) #33
```

    ## [1] 33.2

### Endogeic Abundance

Interaction (topography x tree functional type) is trending towards
significant (p=0.08)

``` r
soil.total <-lme(sqrt(totalAbundance)~ mycorrhizal.fungi.type + mycorrhizal.fungi.type:landscape.position, 
                 random=list(tree.identity=~1, sample.id=~1), data = macrofauna[macrofauna$sample.type == "Soil",])
summary(soil.total)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: macrofauna[macrofauna$sample.type == "Soil", ] 
    ##   AIC BIC logLik
    ##   149 154  -67.5
    ## 
    ## Random effects:
    ##  Formula: ~1 | tree.identity
    ##         (Intercept)
    ## StdDev:    4.88e-05
    ## 
    ##  Formula: ~1 | sample.id %in% tree.identity
    ##         (Intercept) Residual
    ## StdDev:     0.00321     13.4
    ## 
    ## Fixed effects:  sqrt(totalAbundance) ~ mycorrhizal.fungi.type + mycorrhizal.fungi.type:landscape.position 
    ##                                                    Value Std.Error DF t-value p-value
    ## (Intercept)                                         47.5      6.01  9    7.90  0.0000
    ## mycorrhizal.fungi.typeECM                          -13.3      8.50  9   -1.57  0.1518
    ## mycorrhizal.fungi.typeAM:landscape.positionUphill  -12.9      8.14  6   -1.58  0.1651
    ## mycorrhizal.fungi.typeECM:landscape.positionUphill  20.9      9.02  6    2.32  0.0594
    ##  Correlation: 
    ##                                                    (Intr) my..ECM m..AM:
    ## mycorrhizal.fungi.typeECM                          -0.707               
    ## mycorrhizal.fungi.typeAM:landscape.positionUphill  -0.739  0.522        
    ## mycorrhizal.fungi.typeECM:landscape.positionUphill  0.000 -0.471   0.000
    ## 
    ## Standardized Within-Group Residuals:
    ##    Min     Q1    Med     Q3    Max 
    ## -1.407 -0.667 -0.255  0.527  2.190 
    ## 
    ## Number of Observations: 20
    ## Number of Groups: 
    ##                tree.identity sample.id %in% tree.identity 
    ##                           11                           19

``` r
anova(soil.total)
```

    ##                                           numDF denDF F-value p-value
    ## (Intercept)                                   1     9   193.5  <.0001
    ## mycorrhizal.fungi.type                        1     9     0.2  0.6309
    ## mycorrhizal.fungi.type:landscape.position     2     6     3.9  0.0807

``` r
rsquared(soil.total)
```

    ##         Response   family     link method Marginal Conditional
    ## 1 totalAbundance gaussian identity   none      0.3         0.3

``` r
AIC(soil.total) #149
```

    ## [1] 149

``` r
AICc(soil.total) #158
```

    ## [1] 158

``` r
pairs(emmeans(soil.total, specs= ~landscape.position*mycorrhizal.fungi.type))
```

    ## NOTE: A nesting structure was detected in the fitted model:
    ##     landscape.position %in% mycorrhizal.fungi.type

    ##  contrast                   estimate   SE df t.ratio p.value
    ##  Downhill AM - Uphill AM       12.87 8.14  6   1.580  0.4530
    ##  Downhill AM - Downhill ECM    13.32 8.50  9   1.566  0.4420
    ##  Downhill AM - Uphill ECM      -7.62 9.02  6  -0.844  0.8320
    ##  Uphill AM - Downhill ECM       0.45 8.14  6   0.055  1.0000
    ##  Uphill AM - Uphill ECM       -20.49 8.68  6  -2.360  0.1860
    ##  Downhill ECM - Uphill ECM    -20.93 9.02  6  -2.321  0.1950
    ## 
    ## Note: contrasts are still on the sqrt scale. Consider using
    ##       regrid() if you want contrasts of back-transformed estimates. 
    ## Degrees-of-freedom method: containment 
    ## P value adjustment: tukey method for comparing a family of 4 estimates

### Endogeic Shannon Diversity

Not signficant, best model is null model

``` r
soil.shan <-lme(shannon~ landscape.position, 
                random=list(tree.identity=~1, sample.id=~1), data = macrofauna[macrofauna$sample.type == "Soil",])
summary(soil.shan)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: macrofauna[macrofauna$sample.type == "Soil", ] 
    ##    AIC  BIC logLik
    ##   7.65 12.1   1.18
    ## 
    ## Random effects:
    ##  Formula: ~1 | tree.identity
    ##         (Intercept)
    ## StdDev:    7.97e-07
    ## 
    ##  Formula: ~1 | sample.id %in% tree.identity
    ##         (Intercept) Residual
    ## StdDev:       0.241    0.006
    ## 
    ## Fixed effects:  shannon ~ landscape.position 
    ##                          Value Std.Error DF t-value p-value
    ## (Intercept)              1.571    0.0762 10   20.63   0.000
    ## landscape.positionUphill 0.156    0.1107  7    1.41   0.201
    ##  Correlation: 
    ##                          (Intr)
    ## landscape.positionUphill -0.688
    ## 
    ## Standardized Within-Group Residuals:
    ##      Min       Q1      Med       Q3      Max 
    ## -0.70454 -0.01408  0.00384  0.01607  0.70949 
    ## 
    ## Number of Observations: 20
    ## Number of Groups: 
    ##                tree.identity sample.id %in% tree.identity 
    ##                           11                           19

``` r
anova(soil.shan)
```

    ##                    numDF denDF F-value p-value
    ## (Intercept)            1    10     886  <.0001
    ## landscape.position     1     7       2   0.201

``` r
rsquared(soil.shan)
```

    ##   Response   family     link method Marginal Conditional
    ## 1  shannon gaussian identity   none   0.0997       0.999

``` r
AIC(soil.shan) #11
```

    ## [1] 7.65

``` r
AICc(soil.shan) #18
```

    ## [1] 11.9

### Macrofauna Community Composition

``` r
# Running ANOVA of NMDS
adonis2(macrofauna_matrix.rel ~ season * landscape.position * mycorrhizal.fungi.type * sample.type, 
        data=macrofauna, permutations=999, by = "terms")
```

    ## Permutation test for adonis under reduced model
    ## Terms added sequentially (first to last)
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## adonis2(formula = macrofauna_matrix.rel ~ season * landscape.position * mycorrhizal.fungi.type * sample.type, data = macrofauna, permutations = 999, by = "terms")
    ##                                                              Df SumOfSqs    R2     F Pr(>F)    
    ## season                                                        4     1.73 0.189  2.84  0.033 *  
    ## landscape.position                                            1     0.36 0.040  2.38  0.102    
    ## mycorrhizal.fungi.type                                        1     0.12 0.013  0.80  0.605    
    ## sample.type                                                   1     2.04 0.223 13.41  0.001 ***
    ## season:landscape.position                                     4     0.49 0.053  0.80  0.687    
    ## season:mycorrhizal.fungi.type                                 4     0.86 0.094  1.41  0.250    
    ## landscape.position:mycorrhizal.fungi.type                     1     0.21 0.023  1.41  0.274    
    ## season:sample.type                                            4     0.99 0.108  1.63  0.194    
    ## landscape.position:sample.type                                1     0.24 0.027  1.60  0.229    
    ## mycorrhizal.fungi.type:sample.type                            1     0.10 0.011  0.64  0.689    
    ## season:landscape.position:mycorrhizal.fungi.type              3     0.48 0.052  1.05  0.475    
    ## season:landscape.position:sample.type                         4     0.52 0.057  0.85  0.632    
    ## season:mycorrhizal.fungi.type:sample.type                     4     0.32 0.035  0.52  0.928    
    ## landscape.position:mycorrhizal.fungi.type:sample.type         1     0.13 0.014  0.86  0.538    
    ## season:landscape.position:mycorrhizal.fungi.type:sample.type  3     0.24 0.026  0.53  0.898    
    ## Residual                                                      2     0.30 0.033                 
    ## Total                                                        39     9.14 1.000                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# only season and sample type are significant :) (no interaction)

# what about with season combined?
adonis2(macrofauna_matrix.rel ~ season.1 * landscape.position * mycorrhizal.fungi.type * sample.type, 
        data=macrofauna, permutations=999, by = "terms")
```

    ## Permutation test for adonis under reduced model
    ## Terms added sequentially (first to last)
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## adonis2(formula = macrofauna_matrix.rel ~ season.1 * landscape.position * mycorrhizal.fungi.type * sample.type, data = macrofauna, permutations = 999, by = "terms")
    ##                                                                Df SumOfSqs    R2     F Pr(>F)    
    ## season.1                                                        3     1.36 0.149  2.56  0.008 ** 
    ## landscape.position                                              1     0.36 0.040  2.05  0.065 .  
    ## mycorrhizal.fungi.type                                          1     0.12 0.013  0.69  0.674    
    ## sample.type                                                     1     2.04 0.223 11.50  0.001 ***
    ## season.1:landscape.position                                     3     0.40 0.044  0.75  0.741    
    ## season.1:mycorrhizal.fungi.type                                 3     0.65 0.071  1.21  0.269    
    ## landscape.position:mycorrhizal.fungi.type                       1     0.21 0.023  1.21  0.290    
    ## season.1:sample.type                                            3     0.86 0.094  1.61  0.081 .  
    ## landscape.position:sample.type                                  1     0.24 0.027  1.37  0.225    
    ## mycorrhizal.fungi.type:sample.type                              1     0.10 0.011  0.55  0.763    
    ## season.1:landscape.position:mycorrhizal.fungi.type              2     0.25 0.027  0.70  0.736    
    ## season.1:landscape.position:sample.type                         3     0.38 0.042  0.72  0.762    
    ## season.1:mycorrhizal.fungi.type:sample.type                     3     0.11 0.012  0.21  0.999    
    ## landscape.position:mycorrhizal.fungi.type:sample.type           1     0.13 0.014  0.74  0.626    
    ## season.1:landscape.position:mycorrhizal.fungi.type:sample.type  2     0.15 0.016  0.41  0.947    
    ## Residual                                                       10     1.77 0.194                 
    ## Total                                                          39     9.14 1.000                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# pretty much the same - weak relationship w fungi (p=0.1) and interaction between season and sample type (p=0.09)

adonis2(macrofauna_matrix.rel ~ PRECIP + AIRTEMP + soil.moisture, 
        data=macrofauna, permutations=999, by = "terms", na.action = na.omit)
```

    ## Permutation test for adonis under reduced model
    ## Terms added sequentially (first to last)
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## adonis2(formula = macrofauna_matrix.rel ~ PRECIP + AIRTEMP + soil.moisture, data = macrofauna, permutations = 999, by = "terms", na.action = na.omit)
    ##               Df SumOfSqs    R2    F Pr(>F)    
    ## PRECIP         1     0.80 0.106 4.08  0.001 ***
    ## AIRTEMP        1     0.28 0.037 1.43  0.217    
    ## soil.moisture  1     0.60 0.079 3.03  0.014 *  
    ## Residual      30     5.89 0.778                
    ## Total         33     7.57 1.000                
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Precip and soil moisture significant
```

<div class="figure">

<img src="Figures/Figure7_MacrofaunaCommunity.png" alt="NMDS of Macrofauna Community Composition" width="4500" />
<p class="caption">

NMDS of Macrofauna Community Composition
</p>

</div>

<div class="figure">

<img src="Figures/SupplementalFigure4_MacrofaunaCommunitybyClimate.png" alt="NMDS of Macrofauna with Climate Variables" width="4500" />
<p class="caption">

NMDS of Macrofauna with Climate Variables
</p>

</div>

## Within Season Nematode Analyses

### What are the variables driving changes in nematode community composition within each season?

Used as explanatory, but not highlighted in the results section

## Predator-Prey Index

### Winter Predator-Prey Index

``` r
model.predator.prey.winter <-lme(trophic.index~ PRECIP, 
                                 random=list(sample.id=~1, tree.identity=~1), na.action = na.omit, data = nematode.winter) 
summary(model.predator.prey.winter)$tTable
```

    ##                 Value Std.Error DF t-value  p-value
    ## (Intercept)  0.075142  0.007123 29   10.55 1.94e-11
    ## PRECIP      -0.000528  0.000143 29   -3.69 9.15e-04

``` r
anova(model.predator.prey.winter)
```

    ##             numDF denDF F-value p-value
    ## (Intercept)     1    29   126.5  <.0001
    ## PRECIP          1    29    13.6   9e-04

``` r
rsquared(model.predator.prey.winter) #32
```

    ##        Response   family     link method Marginal Conditional
    ## 1 trophic.index gaussian identity   none    0.245       0.323

``` r
AIC(model.predator.prey.winter) #-167
```

    ## [1] -163

``` r
AICc(model.predator.prey.winter) #-166
```

    ## [1] -161

### Spring Predator-Prey Index

``` r
model.predator.prey.spring <-lme(trophic.index~ PRECIP, 
                                 random=list(sample.id=~1, tree.identity=~1), na.action = na.omit, data = nematode.spring) 
summary(model.predator.prey.spring)$tTable
```

    ##                 Value Std.Error DF t-value  p-value
    ## (Intercept)  0.056309  0.007280 16    7.74 8.56e-07
    ## PRECIP      -0.000421  0.000195 10   -2.15 5.67e-02

``` r
anova(model.predator.prey.spring)
```

    ##             numDF denDF F-value p-value
    ## (Intercept)     1    16    69.0  <.0001
    ## PRECIP          1    10     4.6  0.0567

``` r
rsquared(model.predator.prey.spring) #92
```

    ##        Response   family     link method Marginal Conditional
    ## 1 trophic.index gaussian identity   none   0.0651       0.682

``` r
AIC(model.predator.prey.spring) #-121
```

    ## [1] -115

``` r
AICc(model.predator.prey.spring) #-118
```

    ## [1] -113

### Summer Predator-Prey Index

``` r
model.predator.prey.summer <-lme(trophic.index~ AIRTEMP + landscape.position,
                                 random=list(sample.id=~1, tree.identity=~1), na.action = na.omit, data = nematode.summer) 
summary(model.predator.prey.summer)$tTable
```

    ##                             Value Std.Error DF t-value p-value
    ## (Intercept)               0.08409   0.06799 31   1.237   0.225
    ## AIRTEMP                  -0.00176   0.00303 31  -0.581   0.566
    ## landscape.positionUphill  0.04292   0.01488 14   2.884   0.012

``` r
anova(model.predator.prey.summer)
```

    ##                    numDF denDF F-value p-value
    ## (Intercept)            1    31    80.0  <.0001
    ## AIRTEMP                1    31     0.3   0.566
    ## landscape.position     1    14     8.3   0.012

``` r
rsquared(model.predator.prey.summer) #36
```

    ##        Response   family     link method Marginal Conditional
    ## 1 trophic.index gaussian identity   none    0.209       0.374

``` r
AIC(model.predator.prey.summer) #-140
```

    ## [1] -135

``` r
AICc(model.predator.prey.summer) #-137
```

    ## [1] -133

### Fall Predator-Prey Index

Best model was null model, but this is the next best - precip is not
significant

``` r
model.predator.prey.fall <-lme(trophic.index~ PRECIP,
                               random=list(sample.id=~1, tree.identity=~1), na.action = na.omit, data = nematode.fall) 
summary(model.predator.prey.fall)$tTable
```

    ##               Value Std.Error DF t-value p-value
    ## (Intercept) 0.07324   0.02672 18   2.741  0.0134
    ## PRECIP      0.00145   0.00241 18   0.603  0.5538

``` r
anova(model.predator.prey.fall)
```

    ##             numDF denDF F-value p-value
    ## (Intercept)     1    18   14.51  0.0013
    ## PRECIP          1    18    0.36  0.5538

``` r
rsquared(model.predator.prey.fall) #11
```

    ##        Response   family     link method Marginal Conditional
    ## 1 trophic.index gaussian identity   none   0.0131      0.0685

``` r
AIC(model.predator.prey.fall) #-22
```

    ## [1] -20.8

``` r
AICc(model.predator.prey.fall) #-19
```

    ## [1] -18

## Nematode Lifestyle Index

This index is a ratio of plant parasite vs free living nematodes

### Winter Lifestyle Index

No significant variables

``` r
model.lifestyle.winter <-lme(lifestyle.index~ soil.moisture + mycorrhizal.fungi.type + landscape.position:mycorrhizal.fungi.type,
                             random=list(sample.id=~1, tree.identity=~1), na.action = na.omit, data = nematode.winter)
summary(model.lifestyle.winter)$tTable
```

    ##                                                     Value Std.Error DF t-value p-value
    ## (Intercept)                                         6.488     3.065 29   2.117   0.043
    ## soil.moisture                                      -0.152     0.101 29  -1.504   0.143
    ## mycorrhizal.fungi.typeECM                          -0.176     0.950 12  -0.185   0.856
    ## mycorrhizal.fungi.typeAM:landscape.positionUphill   1.211     0.890 12   1.361   0.199
    ## mycorrhizal.fungi.typeECM:landscape.positionUphill -0.345     0.972 12  -0.355   0.729

``` r
anova(model.lifestyle.winter)
```

    ##                                           numDF denDF F-value p-value
    ## (Intercept)                                   1    29    51.7  <.0001
    ## soil.moisture                                 1    29     1.7   0.199
    ## mycorrhizal.fungi.type                        1    12     1.9   0.189
    ## mycorrhizal.fungi.type:landscape.position     2    12     1.0   0.397

``` r
rsquared(model.lifestyle.winter) #20
```

    ##          Response   family     link method Marginal Conditional
    ## 1 lifestyle.index gaussian identity   none    0.113       0.148

``` r
AIC(model.lifestyle.winter) #219
```

    ## [1] 219

``` r
AICc(model.lifestyle.winter) #223
```

    ## [1] 223

### Spring Lifestyle Index

``` r
model.lifestyle.spring <-lme(lifestyle.index~ landscape.position,
                             random=list(sample.id=~1, tree.identity=~1), na.action = na.omit, data = nematode.spring)
summary(model.lifestyle.spring)$tTable
```

    ##                          Value Std.Error DF t-value p-value
    ## (Intercept)              0.845     0.308 15    2.75 0.01493
    ## landscape.positionUphill 1.442     0.435 15    3.32 0.00471

``` r
anova(model.lifestyle.spring)
```

    ##                    numDF denDF F-value p-value
    ## (Intercept)            1    15    51.9  <.0001
    ## landscape.position     1    15    11.0  0.0047

``` r
rsquared(model.lifestyle.spring) #26
```

    ##          Response   family     link method Marginal Conditional
    ## 1 lifestyle.index gaussian identity   none    0.262       0.262

``` r
AIC(model.lifestyle.spring) #113
```

    ## [1] 113

``` r
AICc(model.lifestyle.spring) #115
```

    ## [1] 115

### Summer Lifestyle Index

``` r
model.lifestyle.summer <-lme(lifestyle.index~ PRECIP,
                             random=list(sample.id=~1, tree.identity=~1), na.action = na.omit, data = nematode.summer)
summary(model.lifestyle.summer)$tTable
```

    ##                Value Std.Error DF t-value  p-value
    ## (Intercept)  1.57822   0.18939 31    8.33 2.06e-09
    ## PRECIP      -0.00582   0.00303 31   -1.92 6.35e-02

``` r
anova(model.lifestyle.summer)
```

    ##             numDF denDF F-value p-value
    ## (Intercept)     1    31    83.2  <.0001
    ## PRECIP          1    31     3.7  0.0635

``` r
rsquared(model.lifestyle.summer) #36
```

    ##          Response   family     link method Marginal Conditional
    ## 1 lifestyle.index gaussian identity   none   0.0496       0.371

``` r
AIC(model.lifestyle.summer) #127
```

    ## [1] 127

``` r
AICc(model.lifestyle.summer) #128
```

    ## [1] 128

### Fall Lifestyle Index

Best model is null - soil moisture not signficant and rsquared is
hideous (best r-squared value from stepwise is from the full model)

``` r
model.lifestyle.fall <-lme(lifestyle.index~ soil.moisture,
                           random=list(sample.id=~1, tree.identity=~1), na.action = na.omit, data = nematode.fall)
summary(model.lifestyle.fall)$tTable
```

    ##                  Value Std.Error DF t-value p-value
    ## (Intercept)    1.95820    0.8032 18   2.438  0.0254
    ## soil.moisture -0.00537    0.0308 18  -0.174  0.8635

``` r
anova(model.lifestyle.fall)
```

    ##               numDF denDF F-value p-value
    ## (Intercept)       1    18    33.3  <.0001
    ## soil.moisture     1    18     0.0   0.864

``` r
rsquared(model.lifestyle.fall) #.5
```

    ##          Response   family     link method Marginal Conditional
    ## 1 lifestyle.index gaussian identity   none  0.00117     0.00117

``` r
AIC(model.lifestyle.fall) #117
```

    ## [1] 117

``` r
AICc(model.lifestyle.fall) #120
```

    ## [1] 120

## Nematode Channel Index

Ratio of fungal feeding and bacterial feeding nematodes \### Winter
Channel Index

``` r
model.channel.winter <-lme(channel.index~ soil.moisture, 
                           random=list(sample.id=~1, tree.identity=~1), na.action = na.omit, data = nematode.winter)
summary(model.channel.winter)$tTable
```

    ##                  Value Std.Error DF t-value p-value
    ## (Intercept)    0.27779   0.09491 29    2.93  0.0066
    ## soil.moisture -0.00768   0.00334 29   -2.30  0.0289

``` r
anova(model.channel.winter)
```

    ##               numDF denDF F-value p-value
    ## (Intercept)       1    29   30.07  <.0001
    ## soil.moisture     1    29    5.29  0.0289

``` r
rsquared(model.channel.winter) #10
```

    ##        Response   family     link method Marginal Conditional
    ## 1 channel.index gaussian identity   none    0.101       0.101

``` r
AIC(model.channel.winter) #-85
```

    ## [1] -85

``` r
AICc(model.channel.winter) #-84
```

    ## [1] -83.6

### Spring Channel Index

``` r
model.channel.spring <-lme(channel.index~ landscape.position, 
                           random=list(sample.id=~1, tree.identity=~1), na.action = na.omit, data = nematode.spring)
summary(model.channel.spring)$tTable
```

    ##                            Value Std.Error DF t-value p-value
    ## (Intercept)               0.0391    0.0127 15    3.08 0.00765
    ## landscape.positionUphill -0.0356    0.0176 15   -2.02 0.06127

``` r
anova(model.channel.spring)
```

    ##                    numDF denDF F-value p-value
    ## (Intercept)            1    15    5.46  0.0337
    ## landscape.position     1    15    4.09  0.0613

``` r
rsquared(model.channel.spring) #70
```

    ##        Response   family     link method Marginal Conditional
    ## 1 channel.index gaussian identity   none    0.173         0.7

``` r
AIC(model.channel.spring) #-102
```

    ## [1] -102

``` r
AICc(model.channel.spring) #-100
```

    ## [1] -99.4

### Summer Channel Index

Best model is null model - no significant predictor variables

``` r
model.channel.summer <-lme(channel.index~ AIRTEMP, 
                           random=list(sample.id=~1, tree.identity=~1), na.action = na.omit, data = nematode.summer)
summary(model.channel.summer)$tTable
```

    ##                Value Std.Error DF t-value p-value
    ## (Intercept)  0.05858   0.07745 31   0.756   0.455
    ## AIRTEMP     -0.00124   0.00344 31  -0.361   0.720

``` r
anova(model.channel.summer)
```

    ##             numDF denDF F-value p-value
    ## (Intercept)     1    31    4.51  0.0418
    ## AIRTEMP         1    31    0.13  0.7204

``` r
rsquared(model.channel.summer) #61
```

    ##        Response   family     link method Marginal Conditional
    ## 1 channel.index gaussian identity   none  0.00113       0.601

``` r
AIC(model.channel.summer) #-116
```

    ## [1] -115

``` r
AICc(model.channel.summer) #-115
```

    ## [1] -114

### Fall Channel Index

Best model is null, no signficant predictor variables

``` r
model.channel.fall <-lme(channel.index~AIRTEMP, 
                         random=list(sample.id=~1, tree.identity=~1), na.action = na.omit, data = nematode.fall)
summary(model.channel.fall)$tTable
```

    ##               Value Std.Error DF t-value p-value
    ## (Intercept) -0.2159    0.3182 18  -0.679   0.506
    ## AIRTEMP      0.0244    0.0188 18   1.299   0.210

``` r
anova(model.channel.fall)
```

    ##             numDF denDF F-value p-value
    ## (Intercept)     1    18   11.19  0.0036
    ## AIRTEMP         1    18    1.69  0.2105

``` r
rsquared(model.channel.fall) #17
```

    ##        Response   family     link method Marginal Conditional
    ## 1 channel.index gaussian identity   none   0.0558       0.154

``` r
AIC(model.channel.fall) #22
```

    ## [1] 22.4

``` r
AICc(model.channel.fall) #25
```

    ## [1] 25.2

## Nematode Diversity (Simpson Diversity Index)

### Winter Nematode Diversity

``` r
model.diversity.winter <-lme(simpson~ soil.moisture, 
                             random=list(sample.id=~1, tree.identity=~1), na.action = na.omit, data = nematode.winter)
summary(model.diversity.winter)$tTable
```

    ##                 Value Std.Error DF t-value p-value
    ## (Intercept)   0.24122   0.13369 29    1.80  0.0816
    ## soil.moisture 0.00811   0.00469 29    1.73  0.0946

``` r
anova(model.diversity.winter) 
```

    ##               numDF denDF F-value p-value
    ## (Intercept)       1    29     791  <.0001
    ## soil.moisture     1    29       3  0.0946

``` r
rsquared(model.diversity.winter) #25
```

    ##   Response   family     link method Marginal Conditional
    ## 1  simpson gaussian identity   none   0.0646       0.166

``` r
AIC(model.diversity.winter) #-60
```

    ## [1] -58.4

``` r
AICc(model.diversity.winter) #-59
```

    ## [1] -57

### Spring Nematode Diversity

``` r
model.diversity.spring <-lme(simpson~ soil.moisture + landscape.position, 
                             random=list(sample.id=~1, tree.identity=~1), na.action = na.omit, data = nematode.spring)
summary(model.diversity.spring)$tTable
```

    ##                             Value Std.Error DF t-value  p-value
    ## (Intercept)               0.72258    0.1079 15    6.70 7.11e-06
    ## soil.moisture            -0.00735    0.0039 10   -1.89 8.86e-02
    ## landscape.positionUphill -0.06591    0.0268 15   -2.46 2.66e-02

``` r
anova(model.diversity.spring)
```

    ##                    numDF denDF F-value p-value
    ## (Intercept)            1    15    1389  <.0001
    ## soil.moisture          1    10       2  0.1603
    ## landscape.position     1    15       6  0.0266

``` r
rsquared(model.diversity.spring) #28
```

    ##   Response   family     link method Marginal Conditional
    ## 1  simpson gaussian identity   none    0.212       0.212

``` r
AIC(model.diversity.spring) #-46
```

    ## [1] -44.6

``` r
AICc(model.diversity.spring) #-43
```

    ## [1] -41.2

### Summer Nematode Diversity

Best model is null, no significant variables - but in worse AIC model,
AIRTEMP trends towards signficant (May rerun this or ask input!)

``` r
model.diversity.summer <-lme(simpson~ mycorrhizal.fungi.type, 
                             random=list(sample.id=~1, tree.identity=~1), na.action = na.omit, data = nematode.summer)
summary(model.diversity.summer)$tTable
```

    ##                            Value Std.Error DF t-value  p-value
    ## (Intercept)               0.5146    0.0177 32  29.083 1.39e-24
    ## mycorrhizal.fungi.typeECM 0.0166    0.0271 14   0.614 5.49e-01

``` r
anova(model.diversity.summer)
```

    ##                        numDF denDF F-value p-value
    ## (Intercept)                1    32    1518  <.0001
    ## mycorrhizal.fungi.type     1    14       0   0.549

``` r
rsquared(model.diversity.summer) #30
```

    ##   Response   family     link method Marginal Conditional
    ## 1  simpson gaussian identity   none   0.0127       0.295

``` r
AIC(model.diversity.summer) #-99
```

    ## [1] -98.7

``` r
AICc(model.diversity.summer) #-97
```

    ## [1] -97.3

### Fall Nematode Diversity

Best model is null, no signficant predictor variables

``` r
model.diversity.fall <-lme(simpson~ soil.moisture, 
                           random=list(sample.id=~1, tree.identity=~1), na.action = na.omit, data = nematode.fall)
summary(model.diversity.fall)$tTable
```

    ##                  Value Std.Error DF t-value  p-value
    ## (Intercept)   0.511375   0.05826 18   8.778 6.38e-08
    ## soil.moisture 0.000523   0.00224 18   0.234 8.18e-01

``` r
anova(model.diversity.fall)
```

    ##               numDF denDF F-value p-value
    ## (Intercept)       1    18     518  <.0001
    ## soil.moisture     1    18       0   0.818

``` r
rsquared(model.diversity.fall) #1
```

    ##   Response   family     link method Marginal Conditional
    ## 1  simpson gaussian identity   none   0.0021      0.0021

``` r
AIC(model.diversity.fall) #-14
```

    ## [1] -14

``` r
AICc(model.diversity.fall) #-11
```

    ## [1] -11.1
