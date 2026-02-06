# Render a single report for school or local authority

This function renders a report from a given data file.

## Usage

``` r
render_report(
  survey_data,
  survey_type,
  school_name = NA,
  local_authority_name = NA,
  cluster_label = NULL,
  term = NULL,
  number_invited = NULL,
  output_location = NULL,
  gender_split = TRUE,
  classes = NULL,
  filename = "primary_report.docx"
)
```

## Arguments

- survey_data:

  A clean dataframe of survey results

- survey_type:

  The report type - 'primary'/'secondary'

- school_name:

  If a school report, the school's name

- local_authority_name:

  If a local authority report, the LA's name (note not compatible with
  school report/name argument)

- cluster_label:

  Type of school grouping, e.g. local authority, primary cluster. Only
  used for 'local authority' type reports

- term:

  Name of term to print on report

- number_invited:

  Number invited to complete the survey

- output_location:

  Location of file output (defaults to working directory)

- gender_split:

  To split report outputs by gender

- classes:

  Vector or nested list of classes (clustered by combinations)

- filename:

  Name of file to output
