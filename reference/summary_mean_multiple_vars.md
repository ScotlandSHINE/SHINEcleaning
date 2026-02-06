# Table of means for multiple variables

Table of means for multiple variables

## Usage

``` r
summary_mean_multiple_vars(
  data,
  varslist,
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

- genders:

  List of genders to split by

- classes:

  List of classes to split by

- .gender_split:

  `TRUE`/`FALSE` - split by gender when sufficient numbers of responses

## Value

A summary table of means of multiple variables
