Homework 6
================
J Shearston
November 26, 2018

-   [Problem 1: Washington Post Homicide Data](#problem-1-washington-post-homicide-data)
    -   [Loading and Tidying the Data](#loading-and-tidying-the-data)
    -   [Baltimore, MD](#baltimore-md)
    -   [GLM for All Locations](#glm-for-all-locations)
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

    ## Parsed with column specification:
    ## cols(
    ##   uid = col_character(),
    ##   reported_date = col_integer(),
    ##   victim_last = col_character(),
    ##   victim_first = col_character(),
    ##   victim_race = col_character(),
    ##   victim_age = col_integer(),
    ##   victim_sex = col_character(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   lat = col_double(),
    ##   lon = col_double(),
    ##   disposition = col_character()
    ## )

#### Baltimore, MD

#### GLM for All Locations

Problem 2
---------
