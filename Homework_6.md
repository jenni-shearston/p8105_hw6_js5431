Homework 6
================
J Shearston
November 26, 2018

-   [Problem 1: Washington Post Homicide Data](#problem-1-washington-post-homicide-data)
    -   [Loading and Tidying the Data](#loading-and-tidying-the-data)
    -   [Baltimore, MD](#baltimore-md)
    -   [GLM for All Locations](#glm-for-all-locations)
    -   [Plot of OR and CIs for Each Location](#plot-of-or-and-cis-for-each-location)
-   [Problem 2](#problem-2)
    -   [Loading and Tidying the Data](#loading-and-tidying-the-data-1)
    -   [JAS Model](#jas-model)
    -   [Model Comparison](#model-comparison)
    -   [Conclusion](#conclusion)

Problem 1: Washington Post Homicide Data
----------------------------------------

This problem involves determining the odds of homicide resolution by race (not white vs white), for each location in the dataset.

#### Loading and Tidying the Data

First, homicide data from the Washington Post was loaded into R and cleaned.

``` r
hom_data = read_csv("homicide-data.csv", na = c("", "NA", "Unknown")) %>%
  mutate(city_state = str_c(city, ", ", state),
         solved = case_when(
           disposition == "Closed without arrest" ~ 0, 
           disposition == "Open/No arrest" ~ 0, 
           disposition == "Closed by arrest" ~ 1),
         notwhite_victim = case_when(
           victim_race == "Hispanic" ~ 1,
           victim_race == "White" ~ 0,
           victim_race == "Other" ~ 1,
           victim_race == "Black" ~ 1,
           victim_race == "Asian" ~ 1,
           victim_race == "NA" ~ 1)) %>% 
  filter(!city_state %in% c("Dallas, TX", "Phoenix, AZ", "Kansas City, MO", "Tulsa, AL"))
```

Variables for homicide resolution and victim race were created, such that a homicide was considered solved if it had been "closed by arrest," but not solved if it had been "closed without arrest" or was still "open/no arrest". Non-white race was defined as Hispanic, Other, Black, Asian, or NA (missing). Four locations were removed from analysis because they did not report victim race (Dallas, Tx; Phoenix, AZ; Kansas City, MO) or were the result of data entry error (Tulsa, AL). The resulting dataset had 48507 rows and 15 columns.

#### Baltimore, MD

As a test case, a Generalized Linear Model (glm) was run for the city of Baltimore, MD, to determine the odds of a homicide case being solved for non-white victims, compared to white victims. Victim age and sex were included in the model as covariates.

``` r
baltimore_glm = 
  hom_data %>% 
  filter(city_state == "Baltimore, MD") %>% 
  glm(solved ~ notwhite_victim + victim_age + victim_sex, data = ., family = binomial()) %>%
  broom::tidy(conf.int = TRUE) %>% 
  mutate(OR = exp(estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high)) %>%
  filter(term == "notwhite_victim") %>% 
  select(term, OR, conf.low, conf.high) 

baltimore_glm %>% knitr::kable()
```

| term             |        OR|   conf.low|  conf.high|
|:-----------------|---------:|----------:|----------:|
| notwhite\_victim |  0.440608|  0.3121625|  0.6196693|

The table above shows an odds of 0.44 for non-white victims of having their homicide resolved, suggesting that non-white victims are significantly less likely to have their homicides resolved, compared to white victims.

#### GLM for All Locations

Next, GLM models were run for every location, to determine the odds of homicide resolution for non-white victims (compared to white victims) including victim age and sex as covariates. Results are shown in the table below.

``` r
hom_models = hom_data %>%
  group_by(city_state) %>% 
  nest() %>% 
  mutate(glm_results = map(data, ~broom::tidy(
    glm(solved ~ notwhite_victim + victim_age + victim_sex, data = ., family = binomial()),
    conf.int = TRUE))) %>%
  select(-data) %>% 
  unnest() %>% 
  mutate(OR = exp(estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high)) %>% 
  filter(term == "notwhite_victim") %>% 
  select(city_state, OR, conf.low, conf.high)
  
hom_models %>% knitr::kable()
```

| city\_state        |         OR|   conf.low|  conf.high|
|:-------------------|----------:|----------:|----------:|
| Albuquerque, NM    |  0.7392456|  0.4449066|  1.2201862|
| Atlanta, GA        |  0.7528020|  0.4244281|  1.2990553|
| Baltimore, MD      |  0.4406080|  0.3121625|  0.6196693|
| Baton Rouge, LA    |  0.6676289|  0.3043703|  1.4053484|
| Birmingham, AL     |  1.0392783|  0.6116821|  1.7537497|
| Boston, MA         |  0.1266673|  0.0471849|  0.2853727|
| Buffalo, NY        |  0.3922970|  0.2112860|  0.7144477|
| Charlotte, NC      |  0.5575017|  0.3131012|  0.9506573|
| Chicago, IL        |  0.5620844|  0.4317559|  0.7342755|
| Cincinnati, OH     |  0.3183561|  0.1799173|  0.5413056|
| Columbus, OH       |  0.8606506|  0.6381595|  1.1611588|
| Denver, CO         |  0.6018870|  0.3576083|  1.0076531|
| Detroit, MI        |  0.6515978|  0.4876683|  0.8699183|
| Durham, NC         |  1.0028175|  0.3902597|  2.4518708|
| Fort Worth, TX     |  0.8378356|  0.5527861|  1.2636208|
| Fresno, CA         |  0.4448991|  0.2208349|  0.8411275|
| Houston, TX        |  0.8726047|  0.6984318|  1.0897262|
| Indianapolis, IN   |  0.5045560|  0.3807759|  0.6652965|
| Jacksonville, FL   |  0.6581751|  0.5017927|  0.8617718|
| Las Vegas, NV      |  0.7627184|  0.5913554|  0.9813478|
| Long Beach, CA     |  0.7939031|  0.3790487|  1.6055437|
| Los Angeles, CA    |  0.6658424|  0.4812283|  0.9159982|
| Louisville, KY     |  0.3919136|  0.2572275|  0.5899006|
| Memphis, TN        |  0.7781637|  0.5163124|  1.1541560|
| Miami, FL          |  0.5767198|  0.3758124|  0.8854351|
| Milwaukee, wI      |  0.6323892|  0.3982950|  0.9815833|
| Minneapolis, MN    |  0.6457029|  0.3409396|  1.2039974|
| Nashville, TN      |  0.9022539|  0.6545711|  1.2391419|
| New Orleans, LA    |  0.4668113|  0.2947090|  0.7385293|
| New York, NY       |  0.5317831|  0.2707387|  0.9890362|
| Oakland, CA        |  0.2129779|  0.0989173|  0.4181874|
| Oklahoma City, OK  |  0.6812533|  0.4770878|  0.9696968|
| Omaha, NE          |  0.1700939|  0.0913756|  0.2999534|
| Philadelphia, PA   |  0.6438263|  0.4845237|  0.8500346|
| Pittsburgh, PA     |  0.2815606|  0.1572505|  0.4852413|
| Richmond, VA       |  0.4474146|  0.1442284|  1.1501092|
| San Antonio, TX    |  0.6893496|  0.4585818|  1.0256000|
| Sacramento, CA     |  0.7807364|  0.4431665|  1.3476439|
| Savannah, GA       |  0.6049920|  0.2788886|  1.2766732|
| San Bernardino, CA |  0.8801457|  0.3936535|  1.9991648|
| San Diego, CA      |  0.4833560|  0.2943959|  0.7783316|
| San Francisco, CA  |  0.4582812|  0.2880269|  0.7188686|
| St. Louis, MO      |  0.5770478|  0.4045959|  0.8186394|
| Stockton, CA       |  0.3757201|  0.1933475|  0.7127053|
| Tampa, FL          |  1.1588262|  0.5848662|  2.2928961|
| Tulsa, OK          |  0.5955021|  0.4062619|  0.8659588|
| Washington, DC     |  0.5138958|  0.2515724|  0.9967816|

#### Plot of OR and CIs for Each Location

Finally, a plot of AORs and CIs was created to visualize how the odds of homicide resolution for non-white victims changes by city (Figure 1).

``` r
hom_models %>% 
  mutate(city_state = fct_reorder(city_state, OR),
         Significant = case_when(
           conf.high < 1 ~ 1,
           conf.high > 1 ~ 0),
         Significant = as.logical(Significant)) %>% 
  ggplot(aes(x = city_state, y = OR, color = Significant)) +
  geom_col() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 1) +
  labs(
    title = "Figure 1. Odds of Homicide Resolution, Non-white vs White Race",
    x = "Location",
    y = "Adjusted OR") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

<img src="Homework_6_files/figure-markdown_github/plot of ORs-1.png" width="90%" />

As can be seen from Figure 1, for most cities, odds of homicide resolution for non-white victims was significantly less than for white victims. No cities had significantly higher AORs of homicide resolution for non-white victims, although several cities did show no significant difference between non-white and white victims.

Problem 2
---------

This problem involves selecting an appropriate model to evaluate the effects of select factors on child birthweight.

#### Loading and Tidying the Data

First, the birthweight data was loaded into R and cleaned.

``` r
birth_data = read_csv("birthweight.csv") %>% 
  mutate(babysex = factor(babysex),
         frace = factor(frace),
         malform = factor(malform),
         mrace = factor(mrace))
```

The dataset contains 4342 rows and 20 columns. In the code chunk below missing data was evaluated, and the dataset was found to be case-complete with no missing values.

``` r
skimr::skim(birth_data) %>% skimr::kable()
```

    ## Skim summary statistics  
    ##  n obs: 4342    
    ##  n variables: 20    
    ## 
    ## Variable type: factor
    ## 
    ##  variable    missing    complete     n      n_unique              top_counts               ordered 
    ## ----------  ---------  ----------  ------  ----------  ---------------------------------  ---------
    ##  babysex        0         4342      4342       2            1: 2230, 2: 2112, NA: 0         FALSE  
    ##   frace         0         4342      4342       5        1: 2123, 2: 1911, 4: 248, 3: 46     FALSE  
    ##  malform        0         4342      4342       2             0: 4327, 1: 15, NA: 0          FALSE  
    ##   mrace         0         4342      4342       4        1: 2147, 2: 1909, 4: 243, 3: 43     FALSE  
    ## 
    ## Variable type: integer
    ## 
    ##  variable    missing    complete     n       mean       sd      p0     p25      p50      p75     p100      hist   
    ## ----------  ---------  ----------  ------  --------  --------  -----  ------  --------  ------  ------  ----------
    ##   bhead         0         4342      4342    33.65      1.62     21      33       34       35      41     <U+2581><U+2581><U+2581><U+2581><U+2585><U+2587><U+2581><U+2581> 
    ##  blength        0         4342      4342    49.75      2.72     20      48       50       51      63     <U+2581><U+2581><U+2581><U+2581><U+2581><U+2587><U+2581><U+2581> 
    ##    bwt          0         4342      4342    3114.4    512.15    595    2807    3132.5    3459    4791    <U+2581><U+2581><U+2581><U+2583><U+2587><U+2587><U+2582><U+2581> 
    ##   delwt         0         4342      4342    145.57    22.21     86     131      143      157     334     <U+2581><U+2587><U+2585><U+2581><U+2581><U+2581><U+2581><U+2581> 
    ##  fincome        0         4342      4342    44.11     25.98      0      25       35       65      96     <U+2581><U+2582><U+2587><U+2582><U+2582><U+2582><U+2581><U+2583> 
    ##  menarche       0         4342      4342    12.51      1.48      0      12       12       13      19     <U+2581><U+2581><U+2581><U+2581><U+2582><U+2587><U+2581><U+2581> 
    ##  mheight        0         4342      4342    63.49      2.66     48      62       63       65      77     <U+2581><U+2581><U+2581><U+2585><U+2587><U+2582><U+2581><U+2581> 
    ##   momage        0         4342      4342     20.3      3.88     12      18       20       22      44     <U+2582><U+2587><U+2585><U+2582><U+2581><U+2581><U+2581><U+2581> 
    ##   parity        0         4342      4342    0.0023     0.1       0      0        0        0       6      <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581> 
    ##  pnumlbw        0         4342      4342      0         0        0      0        0        0       0      <U+2581><U+2581><U+2581><U+2587><U+2581><U+2581><U+2581><U+2581> 
    ##  pnumsga        0         4342      4342      0         0        0      0        0        0       0      <U+2581><U+2581><U+2581><U+2587><U+2581><U+2581><U+2581><U+2581> 
    ##    ppwt         0         4342      4342    123.49    20.16     70     110      120      134     287     <U+2581><U+2587><U+2586><U+2581><U+2581><U+2581><U+2581><U+2581> 
    ##   wtgain        0         4342      4342    22.08     10.94     -46     15       22       28      89     <U+2581><U+2581><U+2581><U+2587><U+2587><U+2581><U+2581><U+2581> 
    ## 
    ## Variable type: numeric
    ## 
    ##  variable    missing    complete     n      mean      sd      p0       p25      p50      p75     p100      hist   
    ## ----------  ---------  ----------  ------  -------  ------  -------  -------  -------  -------  ------  ----------
    ##  gaweeks        0         4342      4342    39.43    3.15    17.7     38.3     39.9     41.1     51.3    <U+2581><U+2581><U+2581><U+2581><U+2583><U+2587><U+2581><U+2581> 
    ##   ppbmi         0         4342      4342    21.57    3.18    13.07    19.53    21.03    22.91    46.1    <U+2581><U+2587><U+2585><U+2581><U+2581><U+2581><U+2581><U+2581> 
    ##   smoken        0         4342      4342    4.15     7.41      0        0        0        5       60     <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>

#### JAS Model

Next, a model was developed to evaluate the effects of select characteristics and birthweight. Variables that have previously been identified as assoicated with birthweight were included in the model: avg. number of cigarettes smoked per day during pregnancy, mother's pre-pregnancy BMI, gestational age, family income, and mother's race. Some other variables that may be associated with birthweight, but which may be colinear with selected variables or caused by the outcome, were not included, such as: mother's height, pre-pregnancy weight, delivery weight, weight gain, baby's head circumference and baby's length.

``` r
# Model
jas_m = 
  lm(bwt ~ smoken + ppbmi + gaweeks + fincome + mrace, data = birth_data) 

# Plot of model residuals against fitted values
birth_data %>%  
  modelr::add_residuals(jas_m) %>% 
  modelr::add_predictions(jas_m) %>% 
  ggplot(aes(x = pred, y = resid)) +
  geom_point() +
  labs(
    title = "Figure 2. Residual vs Predicted Plot for JAS Model",
    x = "Predicted Values",
    y = "Residuals")
```

<img src="Homework_6_files/figure-markdown_github/JAS model-1.png" width="90%" />

The residual vs predictor plot looks fairly acceptable, as points cluster around 0, are fairly symmetrically distributed, and don't form an obvious pattern.

#### Model Comparison

Finally, the JAS model was compared to two others: (1) birthweight predicted by length at birth and gestational age, and (2) birthweight predicted by head circumference, length at birth, baby sex, and interaction between all these terms. Cross validation was completed by splitting the birthweight dataset into 100 training and test dataframes, and the three models were compared using the root mean squared errors (RMSEs) of these runs.

``` r
# Length model
length_m = lm(bwt ~ blength + gaweeks, data = birth_data)

# Head circumference model
head_m = lm(bwt ~ bhead*blength + bhead*babysex + blength*babysex + babysex*bhead*blength, data = birth_data)

# Cross validation

library(modelr)
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-24. For overview type 'help("mgcv-package")'.

``` r
cv_df = 
  crossv_mc(birth_data, 100) %>% 
  mutate(jas_m    = map(train, ~lm(bwt ~ smoken + ppbmi + gaweeks + fincome + mrace, data = .x)),
         length_m = map(train, ~lm(bwt ~ blength + gaweeks, data = .x)),
         head_m   = map(train, ~lm(bwt ~ bhead*blength + bhead*babysex + blength*babysex + 
                                     babysex*bhead*blength, data = .x))) %>% 
  mutate(rmse_jas    = map2_dbl(jas_m, test, ~rmse(model = .x, data = .y)),
         rmse_length = map2_dbl(length_m, test, ~rmse(model = .x, data = .y)),
         rmse_head   = map2_dbl(head_m, test, ~rmse(model = .x, data = .y)))

# Plot prediction error distribution
cv_df %>% 
  select(starts_with("rmse")) %>% 
  gather(key = model, value = rmse) %>% 
  mutate(model = str_replace(model, "rmse_", ""),
         model = fct_inorder(model)) %>% 
  ggplot(aes(x = model, y = rmse)) + 
  geom_violin() +
  labs(
    title = "Figure 3. RMSEs for Three Models",
    x = "Model",
    y = "RMSE")
```

<img src="Homework_6_files/figure-markdown_github/model comparison-1.png" width="90%" />

#### Conclusion

In Figure 3 above, the linear model that uses head circumference, length at birth, baby sex, and these variables' interactions to predict birthweight clearly has the lowest root mean squared error and is the "best" fit. However, this model is very difficult to interpret, and since its RMSE is not *that* much better than the model with baby length and gestational age, it is advisable to use the length model as it can be interpreted with much greater ease.
