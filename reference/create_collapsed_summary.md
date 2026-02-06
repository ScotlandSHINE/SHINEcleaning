# Collapsed summary (percentages of successes) for proportion graphs

Collapsed summary (percentages of successes) for proportion graphs

## Usage

``` r
create_collapsed_summary(
  data,
  var,
  success,
  genders,
  classes,
  .gender_split = FALSE
)
```

## Arguments

- data:

  Valid input data

- var:

  Variable to calculate by

- success:

  Character vector of categories as 'successes'

- genders:

  List of genders to split by

- classes:

  List of classes to split by

- .gender_split:

  Gender split - passed from params

## Value

A dataframe of proportions/counts of successes
