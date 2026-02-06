# Validators

Validators

## Usage

``` r
duration_too_short(data)

no_test_responses(data)

partial_cases(data)

duplicate_cases(data)

suggest_missing_class(data)

age_year_mismatch(data)

straightlining(data)

recurring_postcodes(data)

no_consent(data)

has_school_id(data)

valid_dob(data)

completed_outside_school_hours(data)

completed_at_weekend(data)
```

## Arguments

- data:

  Raw input dataset

## Value

A tibble with the same length as the input data, showing the validation
results for the corresponding rows in the input data. Columns:

- `message`: error message or an empty string
