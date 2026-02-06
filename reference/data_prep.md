# Data preparation phase

These functions prepare the data by reshaping it and generating
necessary variables. Helper functions for variables are included.

## Usage

``` r
data_prep(survey_data, report_type = "primary")

who_score(survey_data)

mm_score(survey_data)

sehs_primary(survey_data)

sehs_secondary(survey_data)

asw_score(survey_data)

sdq_score(survey_data)

fas_score(survey_data)
```

## Arguments

- survey_data:

  The data to process

- report_type:

  The type of survey data ("primary" or "secondary")

## Value

`data_prep`: A dataframe with the required variables for rendering a
report

The score-calculating functions return the dataset with relevant columns
appended:

`who_score`: WHO 5-item wellbeing score (`who_score` variable) and
categorical breakdown (`who_cat`: low/good)

`mm_score`: 'Me and My feelings' score for primary schools

`sehs_primary`: SEHS score for primary schools

`sehs_secondary`: SEHS score for secondary schools

`asw_score`: Adolescent sleep-wake score for secondary schools

`sdq_score`: SDQ score for secondary schools

`fas_score`: family affluence score (0-13)
