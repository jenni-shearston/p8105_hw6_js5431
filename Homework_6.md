Homework 6
================
J Shearston
November 26, 2018

-   [Problem 1: Washington Post Homicide Data](#problem-1-washington-post-homicide-data)
    -   [Loading and Tidying the Data](#loading-and-tidying-the-data)
-   [Problem 2](#problem-2)

Problem 1: Washington Post Homicide Data
----------------------------------------

#### Loading and Tidying the Data

``` r
hom_data = read_csv("homicide-data.csv") %>%
  mutate(city_state = str_c(city, ", ", state),
         city_state = as_factor(city_state),
         unsolved = str_detect(disposition, "Open/No arrest"),
         unsolved = ifelse(unsolved == TRUE, "1", "0"))
```

    ## Parsed with column specification:
    ## cols(
    ##   uid = col_character(),
    ##   reported_date = col_integer(),
    ##   victim_last = col_character(),
    ##   victim_first = col_character(),
    ##   victim_race = col_character(),
    ##   victim_age = col_character(),
    ##   victim_sex = col_character(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   lat = col_double(),
    ##   lon = col_double(),
    ##   disposition = col_character()
    ## )

``` r
#filter(city_state != "Dallas, TX" | city_state != "Phoenix, AZ" | city_state != "Kansas City, MO" | city_state != "Tulsa, AL")
```

Problem 2
---------
