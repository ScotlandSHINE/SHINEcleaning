# Bar graph of means of multiple variables

`bar_mean_multiple_vars` returns a horizontal bar graph.
`bar_mean_multiple_vertical` returns a vertical graph.

## Usage

``` r
bar_mean_multiple_vars(summary_data, xmax, xlab = "Mean")

bar_mean_multiple_vertical(summary_data, ymax, ylab = "Mean")
```

## Arguments

- summary_data:

  Data produced by `summary_mean_multiple_vars`

- xmax, ymax:

  Upper limit of graph

- xlab, ylab:

  Label for X axis (summary statistic, i.e. "Mean")
