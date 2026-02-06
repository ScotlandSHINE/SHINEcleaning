# Table of % categories for multiple variables

Table of % categories for multiple variables

## Usage

``` r
summary_proportions_multiple(
  data,
  varslist,
  success = ~.x %in% c("More than once a week", "About every day"),
  genders = c("Boys", "Girls"),
  classes = "All",
  .gender_split = TRUE
)
```

## Arguments

- data:

  The dataframe of valid responses

- varslist:

  (named) list of variables to use. Names to match vars

- success:

  A purrr-like function denoting numerator categories

- genders:

  List of genders to split by

- classes:

  List of classes to split by

- .gender_split:

  `TRUE`/`FALSE` - split by gender when sufficient numbers of responses

## Value

A summary table of % of 'success' in each group
