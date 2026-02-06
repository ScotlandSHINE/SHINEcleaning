# Bar graph of mean of a single var

Bar graph of mean of a single var

## Usage

``` r
bar_mean_single(summary_data, hbsc_data = NULL, ymax, ylab = "Mean")
```

## Arguments

- summary_data:

  A dataframe produced by `summary_mean_single`

- hbsc_data:

  A dataframe with national data to add as points

- ymax:

  Upper limit of graph (deaults to `limits[2]`)

- ylab:

  Label for X axis (summary statistic, i.e. "mean")

## Value

A ggplot2 graph
