Codebook created on 2023-02-22 at 2023-02-22 09:17:21
================

A codebook contains documentation and metadata describing the contents,
structure, and layout of a data file.

## Dataset description

The data contains 978 cases and 7 variables.

## Codebook

| name  | type    |   n | missing | unique |   mean | median |   mode |     sd | min |    max |  range | skew | skew_2se |  kurt | kurt_2se |
|:------|:--------|----:|--------:|-------:|-------:|-------:|-------:|-------:|----:|-------:|-------:|-----:|---------:|------:|---------:|
| id    | integer | 978 |       0 |    978 | 489.50 | 489.50 | 489.50 | 282.47 |   1 | 978.00 | 977.00 |  0.0 |      0.0 | -1.20 |    -3.85 |
| scl_A | numeric | 978 |       0 |    152 |   0.18 |   0.12 |   0.12 |   0.19 |   0 |   0.90 |   0.90 |  1.2 |      7.6 |  0.98 |     3.15 |
| scl_B | numeric | 978 |       0 |    168 |   0.19 |   0.12 |   0.12 |   0.21 |   0 |   0.96 |   0.96 |  1.2 |      7.9 |  0.92 |     2.95 |
| scl_C | numeric | 978 |       0 |    226 |   0.17 |   0.10 |   0.10 |   0.22 |   0 |   0.94 |   0.94 |  1.4 |      8.7 |  1.03 |     3.29 |
| scl_D | numeric | 978 |       0 |    376 |   0.21 |   0.12 |   0.12 |   0.23 |   0 |   1.00 |   1.00 |  1.2 |      7.8 |  0.68 |     2.17 |
| scl_E | numeric | 978 |       0 |    384 |   0.23 |   0.14 |   0.14 |   0.24 |   0 |   1.00 |   1.00 |  1.0 |      6.5 |  0.23 |     0.74 |
| scl_G | numeric | 978 |       0 |    339 |   0.22 |   0.12 |   0.12 |   0.24 |   0 |   0.99 |   0.99 |  1.3 |      8.1 |  0.90 |     2.88 |

### Legend

- **Name**: Variable name
- **type**: Data type of the variable
- **missing**: Proportion of missing values for this variable
- **unique**: Number of unique values
- **mean**: Mean value
- **median**: Median value
- **mode**: Most common value (for categorical variables, this shows the
  frequency of the most common category)
- **mode_value**: For categorical variables, the value of the most
  common category
- **sd**: Standard deviation (measure of dispersion for numerical
  variables
- **v**: Agresti’s V (measure of dispersion for categorical variables)
- **min**: Minimum value
- **max**: Maximum value
- **range**: Range between minimum and maximum value
- **skew**: Skewness of the variable
- **skew_2se**: Skewness of the variable divided by 2\*SE of the
  skewness. If this is greater than abs(1), skewness is significant
- **kurt**: Kurtosis (peakedness) of the variable
- **kurt_2se**: Kurtosis of the variable divided by 2\*SE of the
  kurtosis. If this is greater than abs(1), kurtosis is significant.

This codebook was generated using the [Workflow for Open Reproducible
Code in Science (WORCS)](https://osf.io/zcvbs/)
