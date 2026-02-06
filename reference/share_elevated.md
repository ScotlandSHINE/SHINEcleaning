# Produce table of % of sts with elevatved or expected mm scores

Produce table of % of sts with elevatved or expected mm scores

## Usage

``` r
share_elevated(
  data,
  outcome,
  levels = c("As expected", "Elevated"),
  .split = TRUE,
  classes = "All",
  genders = c("Boy", "Girl")
)
```

## Arguments

- data:

  The dataframe of valid responses

- outcome:

  The variable to graph

- levels:

  Levels of the variable (in ascending order)

- .split:

  Split columns by gender x class

- classes:

  Vector names of classes

- genders:

  Vector names of genders

## Value

dataframe
