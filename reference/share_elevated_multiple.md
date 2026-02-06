# Proportions in each subgroup with elevated results across multiple vars

Proportions in each subgroup with elevated results across multiple vars

## Usage

``` r
share_elevated_multiple(
  data,
  varlist,
  levels = c("As expected", "Elevated"),
  .split = TRUE,
  classes = "All",
  genders = c("Boys", "Girls")
)
```

## Arguments

- data:

  Prepared input data

- varlist:

  List of variable labels, with names corresponding to columns

- levels:

  Levels to sum over

- .split:

  Split by gender/class

- classes:

  Vector/list of classes, nested by clusters

- genders:

  Vector of genders

## Value

A table to plot
