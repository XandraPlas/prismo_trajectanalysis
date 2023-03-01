PRISMO Post-deployment depression
================
17 December, 2022

``` r
library("worcs")
library(tidySEM)
library(ggplot2)
library(MASS)
run_everything = FALSE
options(digits = 2)
# We recommend that you prepare your raw data for analysis in 'prepare_data.R',
# and end that file with either open_data(yourdata), or closed_data(yourdata).
# Then, uncomment the line below to load the original or synthetic data
# (whichever is available), to allow anyone to reproduce your code:
df <- read.csv("../df.csv")
if(!file.exists("out.yml")){
  yaml::write_yaml(list(), "out.yml")
} 
out <- yaml::read_yaml("out.yml")
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
```

This manuscript uses the Workflow for Open Reproducible Code in Science
(Van Lissa et al. 2020) to ensure reproducibility and transparency.
**NOTE: The code is not yet in a repository, but I do recommend it:**

All code <!--and data--> are available at \<git@\[…\].git\>.

<!--The function below inserts a notification if the manuscript is knit using synthetic data. Make sure to insert it after load_data().-->

# Data cleaning

We first examined item descriptives. All items were extremely
right-skewed due to censoring at the lower end of the scale.

``` r
# Get descriptives
desc <- descriptives(df)
desc <- desc[, !colSums(is.na(desc)) == nrow(desc)]
knitr::kable(desc, caption = "Item descriptives")
```

| name      | type    |   n | missing | unique | mean | median | mode |   sd | min | max | range | skew | skew_2se |  kurt | kurt_2se |
|:----------|:--------|----:|--------:|-------:|-----:|-------:|-----:|-----:|----:|----:|------:|-----:|---------:|------:|---------:|
| SCL_A\_3  | integer | 840 |    0.14 |      5 |  1.2 |      1 |    1 | 0.48 |   1 |   4 |     3 |  2.5 |     14.7 |   6.2 |     18.4 |
| SCL_B\_3  | integer | 820 |    0.16 |      6 |  1.2 |      1 |    1 | 0.56 |   1 |   5 |     4 |  2.8 |     16.5 |   9.0 |     26.4 |
| SCL_C\_3  | integer | 744 |    0.24 |      6 |  1.2 |      1 |    1 | 0.55 |   1 |   5 |     4 |  2.9 |     16.2 |   9.3 |     26.0 |
| SCL_D\_3  | integer | 566 |    0.42 |      6 |  1.2 |      1 |    1 | 0.60 |   1 |   5 |     4 |  3.3 |     16.3 |  13.0 |     31.7 |
| SCL_E\_3  | integer | 558 |    0.43 |      6 |  1.2 |      1 |    1 | 0.55 |   1 |   5 |     4 |  2.9 |     14.2 |  10.3 |     25.0 |
| SCL_G\_3  | integer | 603 |    0.38 |      6 |  1.2 |      1 |    1 | 0.56 |   1 |   5 |     4 |  3.2 |     16.0 |  11.5 |     29.0 |
| SCL_A\_5  | integer | 839 |    0.14 |      6 |  1.1 |      1 |    1 | 0.33 |   1 |   5 |     4 |  6.0 |     35.6 |  47.3 |    140.4 |
| SCL_B\_5  | integer | 819 |    0.16 |      5 |  1.1 |      1 |    1 | 0.41 |   1 |   4 |     3 |  3.8 |     22.2 |  15.6 |     45.8 |
| SCL_C\_5  | integer | 744 |    0.24 |      5 |  1.1 |      1 |    1 | 0.43 |   1 |   4 |     3 |  3.8 |     21.3 |  17.4 |     48.7 |
| SCL_D\_5  | integer | 568 |    0.42 |      6 |  1.2 |      1 |    1 | 0.56 |   1 |   5 |     4 |  3.0 |     14.7 |  10.0 |     24.5 |
| SCL_E\_5  | integer | 558 |    0.43 |      6 |  1.3 |      1 |    1 | 0.69 |   1 |   5 |     4 |  3.0 |     14.2 |   9.0 |     21.8 |
| SCL_G\_5  | integer | 604 |    0.38 |      6 |  1.3 |      1 |    1 | 0.61 |   1 |   5 |     4 |  2.9 |     14.4 |   9.4 |     23.7 |
| SCL_A\_14 | integer | 840 |    0.14 |      5 |  1.3 |      1 |    1 | 0.58 |   1 |   4 |     3 |  1.9 |     11.6 |   4.2 |     12.3 |
| SCL_B\_14 | integer | 820 |    0.16 |      6 |  1.5 |      1 |    1 | 0.76 |   1 |   5 |     4 |  1.8 |     10.7 |   3.2 |      9.5 |
| SCL_C\_14 | integer | 744 |    0.24 |      6 |  1.4 |      1 |    1 | 0.75 |   1 |   5 |     4 |  2.1 |     11.9 |   5.2 |     14.6 |
| SCL_D\_14 | integer | 567 |    0.42 |      6 |  1.5 |      1 |    1 | 0.79 |   1 |   5 |     4 |  2.0 |      9.8 |   4.3 |     10.4 |
| SCL_E\_14 | integer | 558 |    0.43 |      6 |  1.5 |      1 |    1 | 0.84 |   1 |   5 |     4 |  2.0 |      9.5 |   3.9 |      9.5 |
| SCL_G\_14 | integer | 604 |    0.38 |      6 |  1.5 |      1 |    1 | 0.78 |   1 |   5 |     4 |  1.8 |      9.1 |   3.3 |      8.2 |
| SCL_A\_15 | integer | 840 |    0.14 |      3 |  1.0 |      1 |    1 | 0.08 |   1 |   2 |     1 | 11.7 |     69.2 | 134.7 |    399.6 |
| SCL_B\_15 | integer | 820 |    0.16 |      5 |  1.0 |      1 |    1 | 0.17 |   1 |   4 |     3 | 11.5 |     67.6 | 161.3 |    472.9 |
| SCL_C\_15 | integer | 743 |    0.24 |      4 |  1.0 |      1 |    1 | 0.16 |   1 |   4 |     3 | 11.4 |     63.8 | 166.7 |    465.2 |
| SCL_D\_15 | integer | 567 |    0.42 |      5 |  1.0 |      1 |    1 | 0.22 |   1 |   4 |     3 |  8.8 |     43.0 |  91.0 |    222.2 |
| SCL_E\_15 | integer | 557 |    0.43 |      5 |  1.0 |      1 |    1 | 0.24 |   1 |   4 |     3 |  7.6 |     36.9 |  67.5 |    163.3 |
| SCL_G\_15 | integer | 604 |    0.38 |      5 |  1.0 |      1 |    1 | 0.32 |   1 |   4 |     3 |  7.0 |     35.0 |  52.6 |    132.4 |
| SCL_A\_19 | integer | 840 |    0.14 |      5 |  1.1 |      1 |    1 | 0.34 |   1 |   4 |     3 |  4.2 |     24.6 |  20.4 |     60.5 |
| SCL_B\_19 | integer | 820 |    0.16 |      5 |  1.1 |      1 |    1 | 0.44 |   1 |   4 |     3 |  3.7 |     21.6 |  14.6 |     42.8 |
| SCL_C\_19 | integer | 744 |    0.24 |      6 |  1.1 |      1 |    1 | 0.39 |   1 |   5 |     4 |  5.1 |     28.5 |  32.8 |     91.6 |
| SCL_D\_19 | integer | 568 |    0.42 |      6 |  1.1 |      1 |    1 | 0.41 |   1 |   5 |     4 |  4.8 |     23.5 |  28.4 |     69.3 |
| SCL_E\_19 | integer | 558 |    0.43 |      5 |  1.1 |      1 |    1 | 0.38 |   1 |   4 |     3 |  4.0 |     19.6 |  19.0 |     45.9 |
| SCL_G\_19 | integer | 604 |    0.38 |      6 |  1.1 |      1 |    1 | 0.40 |   1 |   5 |     4 |  5.1 |     25.5 |  31.0 |     78.1 |
| SCL_A\_20 | integer | 840 |    0.14 |      5 |  1.0 |      1 |    1 | 0.27 |   1 |   4 |     3 |  6.4 |     38.2 |  49.7 |    147.4 |
| SCL_B\_20 | integer | 820 |    0.16 |      5 |  1.1 |      1 |    1 | 0.35 |   1 |   5 |     4 |  4.9 |     28.8 |  30.6 |     89.8 |
| SCL_C\_20 | integer | 744 |    0.24 |      5 |  1.1 |      1 |    1 | 0.36 |   1 |   4 |     3 |  4.2 |     23.6 |  20.6 |     57.5 |
| SCL_D\_20 | integer | 567 |    0.42 |      6 |  1.2 |      1 |    1 | 0.55 |   1 |   5 |     4 |  3.9 |     18.9 |  17.0 |     41.5 |
| SCL_E\_20 | integer | 559 |    0.43 |      6 |  1.2 |      1 |    1 | 0.51 |   1 |   5 |     4 |  4.1 |     19.7 |  19.8 |     48.0 |
| SCL_G\_20 | integer | 603 |    0.38 |      5 |  1.1 |      1 |    1 | 0.43 |   1 |   4 |     3 |  3.3 |     16.5 |  11.6 |     29.3 |
| SCL_A\_22 | integer | 843 |    0.14 |      4 |  1.0 |      1 |    1 | 0.21 |   1 |   3 |     2 |  5.7 |     33.9 |  35.3 |    105.0 |
| SCL_B\_22 | integer | 822 |    0.16 |      5 |  1.0 |      1 |    1 | 0.26 |   1 |   4 |     3 |  5.8 |     34.0 |  39.5 |    115.8 |
| SCL_C\_22 | integer | 743 |    0.24 |      5 |  1.1 |      1 |    1 | 0.30 |   1 |   4 |     3 |  5.4 |     30.2 |  32.4 |     90.4 |
| SCL_D\_22 | integer | 563 |    0.42 |      6 |  1.1 |      1 |    1 | 0.41 |   1 |   5 |     4 |  4.7 |     23.0 |  26.5 |     64.5 |
| SCL_E\_22 | integer | 556 |    0.43 |      4 |  1.1 |      1 |    1 | 0.33 |   1 |   3 |     2 |  4.0 |     19.1 |  16.0 |     38.7 |
| SCL_G\_22 | integer | 604 |    0.38 |      5 |  1.1 |      1 |    1 | 0.37 |   1 |   4 |     3 |  5.3 |     26.9 |  32.0 |     80.5 |
| SCL_A\_26 | integer | 843 |    0.14 |      5 |  1.1 |      1 |    1 | 0.32 |   1 |   4 |     3 |  4.1 |     24.1 |  19.2 |     57.1 |
| SCL_B\_26 | integer | 820 |    0.16 |      5 |  1.1 |      1 |    1 | 0.36 |   1 |   4 |     3 |  4.4 |     25.8 |  22.6 |     66.2 |
| SCL_C\_26 | integer | 742 |    0.24 |      5 |  1.1 |      1 |    1 | 0.34 |   1 |   4 |     3 |  4.4 |     24.4 |  22.8 |     63.7 |
| SCL_D\_26 | integer | 563 |    0.42 |      5 |  1.1 |      1 |    1 | 0.43 |   1 |   5 |     4 |  4.4 |     21.3 |  26.2 |     63.6 |
| SCL_E\_26 | integer | 557 |    0.43 |      5 |  1.1 |      1 |    1 | 0.43 |   1 |   4 |     3 |  3.2 |     15.4 |  10.5 |     25.4 |
| SCL_G\_26 | integer | 602 |    0.38 |      6 |  1.2 |      1 |    1 | 0.53 |   1 |   5 |     4 |  3.0 |     15.2 |  10.6 |     26.8 |
| SCL_A\_29 | integer | 843 |    0.14 |      6 |  1.1 |      1 |    1 | 0.40 |   1 |   5 |     4 |  4.9 |     29.3 |  31.8 |     94.6 |
| SCL_B\_29 | integer | 820 |    0.16 |      5 |  1.1 |      1 |    1 | 0.43 |   1 |   4 |     3 |  3.5 |     20.8 |  14.0 |     41.0 |
| SCL_C\_29 | integer | 743 |    0.24 |      5 |  1.1 |      1 |    1 | 0.46 |   1 |   4 |     3 |  3.6 |     20.3 |  14.9 |     41.5 |
| SCL_D\_29 | integer | 563 |    0.42 |      6 |  1.2 |      1 |    1 | 0.55 |   1 |   5 |     4 |  3.8 |     18.2 |  17.0 |     41.2 |
| SCL_E\_29 | integer | 555 |    0.43 |      6 |  1.2 |      1 |    1 | 0.57 |   1 |   5 |     4 |  3.1 |     14.8 |  11.4 |     27.5 |
| SCL_G\_29 | integer | 602 |    0.38 |      6 |  1.2 |      1 |    1 | 0.59 |   1 |   5 |     4 |  3.1 |     15.6 |  11.0 |     27.7 |
| SCL_A\_30 | integer | 842 |    0.14 |      5 |  1.1 |      1 |    1 | 0.28 |   1 |   4 |     3 |  6.3 |     37.5 |  49.2 |    146.2 |
| SCL_B\_30 | integer | 822 |    0.16 |      5 |  1.1 |      1 |    1 | 0.36 |   1 |   4 |     3 |  4.5 |     26.2 |  22.2 |     65.1 |
| SCL_C\_30 | integer | 743 |    0.24 |      5 |  1.1 |      1 |    1 | 0.44 |   1 |   4 |     3 |  3.5 |     19.8 |  13.9 |     38.9 |
| SCL_D\_30 | integer | 563 |    0.42 |      6 |  1.2 |      1 |    1 | 0.53 |   1 |   5 |     4 |  3.8 |     18.6 |  17.1 |     41.6 |
| SCL_E\_30 | integer | 555 |    0.43 |      6 |  1.2 |      1 |    1 | 0.53 |   1 |   5 |     4 |  3.8 |     18.1 |  16.6 |     40.2 |
| SCL_G\_30 | integer | 601 |    0.39 |      6 |  1.2 |      1 |    1 | 0.56 |   1 |   5 |     4 |  3.7 |     18.6 |  15.2 |     38.1 |
| SCL_A\_31 | integer | 843 |    0.14 |      6 |  1.4 |      1 |    1 | 0.65 |   1 |   5 |     4 |  1.8 |     10.5 |   3.2 |      9.6 |
| SCL_B\_31 | integer | 822 |    0.16 |      6 |  1.3 |      1 |    1 | 0.66 |   1 |   5 |     4 |  2.4 |     14.4 |   6.8 |     20.1 |
| SCL_C\_31 | integer | 743 |    0.24 |      5 |  1.3 |      1 |    1 | 0.65 |   1 |   4 |     3 |  2.1 |     11.9 |   4.6 |     12.8 |
| SCL_D\_31 | integer | 563 |    0.42 |      6 |  1.4 |      1 |    1 | 0.78 |   1 |   5 |     4 |  2.1 |     10.3 |   4.6 |     11.1 |
| SCL_E\_31 | integer | 555 |    0.43 |      6 |  1.5 |      1 |    1 | 0.80 |   1 |   5 |     4 |  1.9 |      9.3 |   3.5 |      8.4 |
| SCL_G\_31 | integer | 601 |    0.39 |      6 |  1.5 |      1 |    1 | 0.76 |   1 |   5 |     4 |  1.9 |      9.3 |   3.8 |      9.4 |
| SCL_A\_32 | integer | 842 |    0.14 |      5 |  1.1 |      1 |    1 | 0.33 |   1 |   4 |     3 |  4.4 |     25.9 |  22.9 |     68.0 |
| SCL_B\_32 | integer | 822 |    0.16 |      5 |  1.1 |      1 |    1 | 0.41 |   1 |   4 |     3 |  3.2 |     19.0 |  10.9 |     31.9 |
| SCL_C\_32 | integer | 743 |    0.24 |      5 |  1.1 |      1 |    1 | 0.42 |   1 |   4 |     3 |  3.5 |     19.4 |  13.0 |     36.3 |
| SCL_D\_32 | integer | 563 |    0.42 |      6 |  1.2 |      1 |    1 | 0.53 |   1 |   5 |     4 |  3.6 |     17.5 |  15.6 |     37.8 |
| SCL_E\_32 | integer | 555 |    0.43 |      6 |  1.2 |      1 |    1 | 0.57 |   1 |   5 |     4 |  4.0 |     19.4 |  18.8 |     45.4 |
| SCL_G\_32 | integer | 602 |    0.38 |      6 |  1.2 |      1 |    1 | 0.49 |   1 |   5 |     4 |  3.7 |     18.6 |  16.1 |     40.4 |
| SCL_A\_51 | integer | 843 |    0.14 |      5 |  1.1 |      1 |    1 | 0.30 |   1 |   4 |     3 |  5.3 |     31.6 |  34.0 |    101.1 |
| SCL_B\_51 | integer | 820 |    0.16 |      5 |  1.1 |      1 |    1 | 0.38 |   1 |   4 |     3 |  3.6 |     21.0 |  13.7 |     40.3 |
| SCL_C\_51 | integer | 746 |    0.24 |      4 |  1.1 |      1 |    1 | 0.36 |   1 |   3 |     2 |  3.3 |     18.2 |  10.6 |     29.8 |
| SCL_D\_51 | integer | 564 |    0.42 |      6 |  1.2 |      1 |    1 | 0.53 |   1 |   5 |     4 |  4.1 |     19.9 |  20.0 |     48.7 |
| SCL_E\_51 | integer | 555 |    0.43 |      5 |  1.2 |      1 |    1 | 0.50 |   1 |   4 |     3 |  3.2 |     15.3 |  11.1 |     26.7 |
| SCL_G\_51 | integer | 603 |    0.38 |      6 |  1.2 |      1 |    1 | 0.50 |   1 |   5 |     4 |  3.5 |     17.8 |  14.7 |     37.1 |
| SCL_A\_54 | integer | 842 |    0.14 |      5 |  1.1 |      1 |    1 | 0.40 |   1 |   5 |     4 |  3.6 |     21.6 |  17.1 |     50.8 |
| SCL_B\_54 | integer | 821 |    0.16 |      5 |  1.1 |      1 |    1 | 0.42 |   1 |   4 |     3 |  3.3 |     19.4 |  12.3 |     36.2 |
| SCL_C\_54 | integer | 747 |    0.24 |      5 |  1.2 |      1 |    1 | 0.49 |   1 |   4 |     3 |  3.0 |     16.5 |   8.8 |     24.8 |
| SCL_D\_54 | integer | 564 |    0.42 |      6 |  1.2 |      1 |    1 | 0.59 |   1 |   5 |     4 |  3.6 |     17.6 |  16.0 |     39.0 |
| SCL_E\_54 | integer | 555 |    0.43 |      6 |  1.2 |      1 |    1 | 0.58 |   1 |   5 |     4 |  3.5 |     17.1 |  14.8 |     35.7 |
| SCL_G\_54 | integer | 604 |    0.38 |      6 |  1.2 |      1 |    1 | 0.62 |   1 |   5 |     4 |  3.2 |     16.4 |  12.0 |     30.2 |
| SCL_A\_59 | integer | 842 |    0.14 |      6 |  1.2 |      1 |    1 | 0.52 |   1 |   5 |     4 |  2.7 |     15.8 |   9.8 |     29.2 |
| SCL_B\_59 | integer | 820 |    0.16 |      4 |  1.1 |      1 |    1 | 0.31 |   1 |   3 |     2 |  4.2 |     24.7 |  18.5 |     54.3 |
| SCL_C\_59 | integer | 747 |    0.24 |      4 |  1.1 |      1 |    1 | 0.31 |   1 |   3 |     2 |  4.5 |     24.9 |  20.7 |     57.8 |
| SCL_D\_59 | integer | 563 |    0.42 |      6 |  1.1 |      1 |    1 | 0.47 |   1 |   5 |     4 |  3.9 |     19.0 |  18.4 |     44.7 |
| SCL_E\_59 | integer | 555 |    0.43 |      6 |  1.1 |      1 |    1 | 0.44 |   1 |   5 |     4 |  4.9 |     23.8 |  30.2 |     72.9 |
| SCL_G\_59 | integer | 604 |    0.38 |      5 |  1.1 |      1 |    1 | 0.48 |   1 |   4 |     3 |  3.9 |     19.6 |  16.2 |     40.9 |
| SCL_A\_79 | integer | 841 |    0.14 |      4 |  1.0 |      1 |    1 | 0.21 |   1 |   3 |     2 |  5.5 |     32.8 |  32.7 |     97.1 |
| SCL_B\_79 | integer | 818 |    0.16 |      4 |  1.0 |      1 |    1 | 0.23 |   1 |   3 |     2 |  5.0 |     29.4 |  27.0 |     79.0 |
| SCL_C\_79 | integer | 746 |    0.24 |      5 |  1.1 |      1 |    1 | 0.28 |   1 |   4 |     3 |  5.9 |     33.0 |  42.8 |    119.8 |
| SCL_D\_79 | integer | 567 |    0.42 |      5 |  1.1 |      1 |    1 | 0.39 |   1 |   5 |     4 |  5.5 |     26.9 |  39.5 |     96.4 |
| SCL_E\_79 | integer | 557 |    0.43 |      5 |  1.1 |      1 |    1 | 0.41 |   1 |   4 |     3 |  4.2 |     20.5 |  19.9 |     48.0 |
| SCL_G\_79 | integer | 602 |    0.38 |      6 |  1.1 |      1 |    1 | 0.49 |   1 |   5 |     4 |  4.3 |     21.4 |  20.3 |     51.1 |

Item descriptives

Item-level missingness ranged from $[0.14, 0.43]$ We used the missForest
algoritm for single imputation; this approach interpolates missing
values based on a random forest model (a flexible machine learning
model) of all other variables. Simulation studies have shown that its
performance is comparable to multiple imputation \[REF Stekhoven\].

``` r
# Use single imputation
df <- df[, grepl("^SCL", names(df))]
names(df) <- gsub("^SCL_(.)_(\\d+)$", "SCL\\2_\\1", names(df))
set.seed(73274)
df_imp <- missRanger::missRanger(df)
saveRDS(df_imp, "df_imp.RData")
```

## Scale descriptives

Next, we computed sum scores for each wave and examined scale
descriptives.

``` r
# Compute scale scores
df_imp <- readRDS("df_imp.RData")
df_scl <- df_imp
names(df_scl) <- gsub("^SCL(\\d+)_(.)$", "SCL_\\2_\\1", names(df_scl))
#df_scl[] <- lapply(df_scl, function(i){as.integer(as.character(i))})
if(anyNA(df_scl)) stop("Requires complete data")
scl <- tidy_sem(as.data.frame(df_scl))
scl_scales <- create_scales(scl, totals = TRUE)
desc <- scl_scales$descriptives
write.csv(desc, "scale_desc.csv", row.names = FALSE)
```

``` r
desc <- read.csv("scale_desc.csv", stringsAsFactors = FALSE)
knitr::kable(desc, getOption("digits"))
```

| Subscale | Items |   n | mean |  sd | min | max | skew | skew_2se | kurt | kurt_2se | Reliability | Interpret | min_load | max_load |
|:---------|------:|----:|-----:|----:|----:|----:|-----:|---------:|-----:|---------:|------------:|:----------|---------:|---------:|
| SCL_A    |    16 | 978 |   18 | 2.9 |  16 |  41 |  3.0 |       19 |   13 |       42 |        0.80 | Good      |     0.20 |     0.73 |
| SCL_B    |    16 | 978 |   18 | 3.6 |  16 |  50 |  3.2 |       20 |   14 |       44 |        0.85 | Good      |     0.29 |     0.77 |
| SCL_C    |    16 | 978 |   18 | 3.8 |  16 |  46 |  3.2 |       20 |   13 |       42 |        0.88 | Good      |     0.37 |     0.82 |
| SCL_D    |    16 | 978 |   19 | 4.8 |  16 |  64 |  3.9 |       25 |   23 |       74 |        0.92 | Excellent |     0.41 |     0.86 |
| SCL_E    |    16 | 978 |   19 | 4.8 |  16 |  63 |  3.5 |       22 |   18 |       57 |        0.92 | Excellent |     0.38 |     0.83 |
| SCL_G    |    16 | 978 |   19 | 5.3 |  16 |  60 |  3.6 |       23 |   17 |       55 |        0.94 | Excellent |     0.56 |     0.87 |

Reliability ranged from good to excellent in all waves. Like the
individual items, however, these scores were extremely skewed and peaked
(at zero). As preliminary analyses indicated that this skew resulted in
model convergence problems in LCGA, we compared several transformations
to reduce skew: The square and cube root, log, inverse, and Box-Cox
transformations.

``` r
df_scores <- scl_scales$scores
df_scores <- reshape(df_scores, direction= "long", varying = names(df_scores), sep = "_")
out[["rng_scl"]] <- range(df_scores$SCL)

df_scores$log <- scales::rescale(log(df_scores$SCL), to = c(0, 1))
df_scores$sqrt <- scales::rescale(sqrt(df_scores$SCL), to = c(0, 1))
df_scores$qrt <- scales::rescale(df_scores$SCL^.33, to = c(0, 1))
df_scores$reciprocal <- scales::rescale(1/df_scores$SCL, to = c(0, 1))
bc <- function(x, lambda){
  (((x ^ lambda) - 1) / lambda)
}
invbc <- function(x, lambda){
  ((x*lambda)+1)^(1/lambda)
}
b <- MASS::boxcox(lm(df_scores$SCL ~ 1))
lambda <- b$x[which.max(b$y)]

df_scores$boxcox <- bc(df_scores$SCL, lambda)
out[["rng_bc"]] <- range(df_scores$boxcox)
df_scores$boxcox <- scales::rescale(df_scores$boxcox, to = c(0, 1))
df_scores$SCL <- scales::rescale(df_scores$SCL, to = c(0, 1))
df_plot <- do.call(rbind, lapply(c("SCL", "log", "sqrt", "qrt", "boxcox"), function(n){
  data.frame(df_scores[c("time", "id")],
             Value = df_scores[[n]],
             Transformation = n)
}))
p_trans <- ggplot(df_plot, aes(x = Value, colour = Transformation)) + geom_density() + facet_wrap(~time) + scale_y_sqrt() + xlab("SCL (rescaled to 0-1)")
ggsave("transformations.svg", p_trans, device = "svg", width = 210, height = 120, units = "mm")
```

``` r
knitr::include_graphics("transformations.svg")
```

![Different transformations to reduce skew](transformations.svg) The
Box-Cox transformation clearly reduced skew the most. Consequently, we
proceeded with the Box-Cox transformed scores for analysis. Via this
transformation, patterns of heterogeneity between participants can be
examined in more detail.

``` r
dat <- df_scores[, c("id", "time", "boxcox")]
dat <- reshape(dat, direction = "wide", v.names = "boxcox", timevar = "time", idvar = "id")
names(dat) <- gsub("boxcox.", "scl_", names(dat))
write.csv(dat, "dat.csv", row.names = FALSE)
```

# Latent Class Growth Analysis

Next, we estimated a latent class growth analysis for SCL. The model
included an overall intercept, centered at T1. To model the potential
effect of deployment on depresion, we also included a dummy variable
that was zero before deployment, and 1 after deployment. Finally, to
model potential change (or recovery) in depression post-deployment, we
included a linear slope from T2-T6. All variances of growth parameters
were fixed to zero due to the sparse nature of the data.

**NOTE: You still have to change the time scales in this model; it
currently assumes that all measurements are equidistant, but that’s not
true.**

``` r
library(worcs)
library(tidySEM)

# Set this condition to TRUE if you really want to re-run the LCGA. 
# It will take a LONG time!
if(!file.exists("res_step.RData")){
  set.seed(699648)
dat <- read.csv("dat.csv", stringsAsFactors = FALSE)
dat[["id"]] <- NULL
names(dat) <- paste0("scl", 1:6)

# Standard LCGA
# This is not run, because you 
# have to account for the fact that
# there may be an effect of deployment
# (the "step function", basically a dummy
# variable for changes due to deployment).
# I only report this here so you see
# how it's done.
# res <- mx_growth_mixture(
#   model =
# "i =~ 1*scl1 + 1*scl2 + 1*scl3 +1*scl4 +1*scl5 +1*scl6
# s =~ 0*scl1 + 1*scl2 + 2*scl3 +3*scl4 +4*scl5 +5*scl6
# scl1 ~~ vscl1*scl1
# scl2 ~~ vscl2*scl2
# scl3 ~~ vscl3*scl3
# scl4 ~~ vscl4*scl4
# scl5 ~~ vscl5*scl5
# scl6 ~~ vscl6*scl6
# i ~~ 0*i
# s ~~ 0*s
# i ~~ 0*s", classes = 1:3,
#   data = dat)
# # In case of convergence problems in the first model:
# res[[1]] <- mxTryHardWideSearch(res[[1]])
# saveRDS(res, "lcga.RData")
# table_fit(res)


# LCGA with step function for effect of deployment
set.seed(67426)
res_step <- mx_growth_mixture(
  model =
"i =~ 1*scl1 + 1*scl2 + 1*scl3 +1*scl4 +1*scl5 +1*scl6
step =~ 0*scl1 + 1*scl2 + 1*scl3 +1*scl4 +1*scl5 +1*scl6
s =~ 0*scl1 + 0*scl2 + 1*scl3 +2*scl4 +3*scl5 +4*scl6
scl1 ~~ vscl1*scl1
scl2 ~~ vscl2*scl2
scl3 ~~ vscl3*scl3
scl4 ~~ vscl4*scl4
scl5 ~~ vscl5*scl5
scl6 ~~ vscl6*scl6
i ~~ 0*i
step ~~ 0*step
s ~~ 0*s
i ~~ 0*s
i ~~ 0*step
s ~~ 0*step", classes = 1:7,
  data = dat)
# In case of convergence problems in the first model:
# res[[1]] <- mxTryHardWideSearch(res[[1]])
saveRDS(res_step, "res_step.RData")
} else {
  res_step <- readRDS("res_step.RData")
}
tab_fit <- table_fit(res_step)
tab_res <- table_results(res_step[[3]])
wald_tests <- tidySEM::wald_test(res_step[[3]], 
                   "class1.M[1,7] = class2.M[1,7]&class1.M[1,7] = class3.M[1,7];class1.M[1,8] = class2.M[1,8]&class1.M[1,8] = class3.M[1,8];class1.M[1,9] = class2.M[1,9]&class1.M[1,9] = class3.M[1,9]")
wald_tests$Hypothesis <- c("Mean i", "Mean step", "Mean slope")
write.csv(tab_fit, "tab_fit_res_step.csv", row.names = FALSE)
write.csv(tab_res, "tab_res_res_step_3.csv", row.names = FALSE)
write.csv(wald_tests, "tab_wald_res_step_3.csv", row.names = FALSE)
p <- tidySEM:::plot.tidy_fit(tab_fit, statistics = c("AIC", "BIC", "saBIC"))
ggsave("plot_scree.svg", p, device = "svg", width = 210, height = 120, units = "mm")
```

## Class enumeration

To determine the correct number of classes, we considered the following
criteria:

1.  We do not consider classes with, on average, fewer than 5
    participants per parameter in a class due to potential local
    underidentification
2.  Lower values for information criteria (AIC, BIC, saBIC) indicate
    better fit
3.  Significant Lo-Mendell-Rubin LRT test indicates better fit for $k$
    vs $k-1$ classes
4.  We do not consider solutions with entropy \< .90 because poor class
    separability compromises interpretability of the results
5.  We do not consider solutions with minimum posterior classification
    probability \< .90 because poor class separability compromises
    interpretability of the results

``` r
tab_fit <- read.csv("tab_fit_res_step.csv", stringsAsFactors = FALSE)
tab_fit <- tab_fit[,c("Name", "Minus2LogLikelihood", "Parameters", "df", "AIC", "BIC", "saBIC", "Entropy", "prob_min", 
"prob_max", "n_min", "n_max", "lmr_lr", "lmr_p")]
knitr::kable(tab_fit, digits = 2, caption = "Fit of LCGA models")
```

| Name | Minus2LogLikelihood | Parameters |    df |   AIC |   BIC | saBIC | Entropy | prob_min | prob_max | n_min | n_max | lmr_lr | lmr_p |
|-----:|--------------------:|-----------:|------:|------:|------:|------:|--------:|---------:|---------:|------:|------:|-------:|------:|
|    1 |               -1047 |          9 |  5859 | -1029 |  -985 | -1043 |    1.00 |     1.00 |     1.00 |  1.00 |  1.00 |     NA |    NA |
|    2 |               -3813 |         13 | 11723 | -3787 | -3723 | -3809 |    0.94 |     0.96 |     0.99 |  0.24 |  0.76 |   2638 |     0 |
|    3 |               -4527 |         17 | 17587 | -4493 | -4410 | -4524 |    0.92 |     0.93 |     0.98 |  0.10 |  0.65 |    682 |     0 |
|    4 |               -4704 |         21 | 23451 | -4662 | -4560 | -4701 |    0.88 |     0.85 |     0.97 |  0.07 |  0.60 |    169 |     0 |
|    5 |               -4798 |         25 | 29315 | -4748 | -4626 | -4794 |    0.87 |     0.76 |     0.97 |  0.05 |  0.60 |     89 |     0 |
|    6 |               -4838 |         29 | 35179 | -4780 | -4639 | -4835 |    0.89 |     0.73 |     0.98 |  0.02 |  0.61 |     39 |     0 |
|    7 |               -4914 |         33 | 41043 | -4848 | -4687 | -4910 |    0.87 |     0.71 |     0.97 |  0.02 |  0.58 |     72 |     0 |

Fit of LCGA models

According to the Table, increasing the number of classes keeps
increasing model fit according to all ICs. All LMR tests are
significant. However, solutions with \>3 classes had entropy and minimum
posterior classification probability below the pre-specified thresholds.
Models with \>3 solutions also had fewer than five observations per
parameter. This suggests that the preferred model should be selected
from 1-3 classes.

### Scree plot

A scree plot indicates that the largest decrease in ICs occurs from 1-2
classes, and the inflection point for all ICs is at 3 classes. A
three-class solution thus appears to be the most parsimonious solution
with good fit.

``` r
knitr::include_graphics("plot_scree.svg")
```

![](plot_scree.svg)<!-- -->

## Selected model

Based on the aforementioned criteria, we selected a 3-class model for
further analyses. The estimated parameters are reported below.

``` r
tab_res <- read.csv("tab_res_res_step_3.csv", stringsAsFactors = FALSE)
tab_res <- tab_res[!grepl("^mix", tab_res$label), ]
knitr::kable(tab_res, digits = 2, caption = "Results from 3-class LCGA model")
```

|     | label          | est_sig     |   se | pval | confint          | class  |
|:----|:---------------|:------------|-----:|-----:|:-----------------|:-------|
| 3   | Variances.scl1 | 0.02\*\*\*  | 0.00 |  0.0 | \[0.02, 0.02\]   | class1 |
| 4   | Variances.scl2 | 0.02\*\*\*  | 0.00 |  0.0 | \[0.02, 0.02\]   | class1 |
| 5   | Variances.scl3 | 0.01\*\*\*  | 0.00 |  0.0 | \[0.01, 0.02\]   | class1 |
| 6   | Variances.scl4 | 0.02\*\*\*  | 0.00 |  0.0 | \[0.02, 0.02\]   | class1 |
| 7   | Variances.scl5 | 0.02\*\*\*  | 0.00 |  0.0 | \[0.02, 0.03\]   | class1 |
| 8   | Variances.scl6 | 0.03\*\*\*  | 0.00 |  0.0 | \[0.03, 0.03\]   | class1 |
| 9   | Means.i        | 0.26\*\*\*  | 0.01 |  0.0 | \[0.24, 0.28\]   | class1 |
| 10  | Means.step     | 0.04\*\*    | 0.01 |  0.0 | \[0.02, 0.06\]   | class1 |
| 11  | Means.s        | 0.02\*\*\*  | 0.00 |  0.0 | \[0.01, 0.03\]   | class1 |
| 12  | Means.i        | 0.10\*\*\*  | 0.01 |  0.0 | \[0.09, 0.11\]   | class2 |
| 13  | Means.step     | -0.03\*\*\* | 0.01 |  0.0 | \[-0.05, -0.02\] | class2 |
| 14  | Means.s        | 0.01\*\*\*  | 0.00 |  0.0 | \[0.00, 0.01\]   | class2 |
| 15  | Means.i        | 0.49\*\*\*  | 0.02 |  0.0 | \[0.46, 0.53\]   | class3 |
| 16  | Means.step     | 0.12\*\*\*  | 0.02 |  0.0 | \[0.08, 0.16\]   | class3 |
| 17  | Means.s        | 0.01        | 0.01 |  0.1 | \[-0.00, 0.02\]  | class3 |

Results from 3-class LCGA model

As evident from these results, Class 1 started at a moderate level of
depressive symptoms, experienced an increase after deployment, followed
by significant increase over time from T2-T6. Class 2 started at a
relatively lower level of depressive symptoms, experienced a decrease
after deployment, followed by increase over time. Class 3 started at a
relatively higher level, experienced an increase after deployment,
followed by stability. As evident from the table below, the differences
between classes in intercept and step are significant.

``` r
wald_tests <- read.csv("tab_wald_res_step_3.csv", stringsAsFactors = FALSE)
knitr::kable(wald_tests, digits = 2, caption = "Wald tests")
```

| Hypothesis |  df | chisq |    p |
|:-----------|----:|------:|-----:|
| Mean i     |   2 |   614 | 0.00 |
| Mean step  |   2 |    66 | 0.00 |
| Mean slope |   2 |    10 | 0.01 |

Wald tests

## Trajectory plot

``` r
p <- tidySEM::plot_growth(res_step[[3]], rawdata = TRUE, alpha_range = c(0, .05))
brks <- seq(0, 1, length.out = 5)
labs <- round(invbc(scales::rescale(brks, from = c(0, 1), to = out$rng_bc), lambda))
p <- p + scale_y_continuous(breaks = seq(0, 1, length.out = 5), labels = labs) + ylab("SCL (rescaled from Box-Cox)")
ggsave("lcga_trajectories.svg", p, device = "svg", width = 210, height = 120, units = "mm")
```

``` r
knitr::include_graphics("lcga_trajectories.svg")
```

![](lcga_trajectories.svg)<!-- -->

Note that the observed individual trajectories show very high
variability within classes.

``` r
yaml::write_yaml(out, "out.yml")
```

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-vanlissaWORCSWorkflowOpen2020" class="csl-entry">

Van Lissa, Caspar J., Andreas M. Brandmaier, Loek Brinkman, Anna-Lena
Lamprecht, Aaron Peikert, Marijn E. Struiksma, and Barbara Vreede. 2020.
“WORCS: A Workflow for Open Reproducible Code in Science,” May.
<https://doi.org/10.17605/OSF.IO/ZCVBS>.

</div>

</div>
