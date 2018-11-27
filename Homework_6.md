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

Problem 1: Washington Post Homicide Data
----------------------------------------

#### Loading and Tidying the Data

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

#### Baltimore, MD

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

#### GLM for All Locations

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

Problem 2
---------
