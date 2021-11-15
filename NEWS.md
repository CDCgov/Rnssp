# Rnssp 0.0.3 beta

## New Features
* New Regression/EWMA Switch algorithm `alert_switch()` added for anomaly detection.

## Updates
* `add_rmd_template()` no longer has a `template_name` argument. The argument `template_name` has been replaced by `template` which is the first argument in the function call.
* New test `test-add_rmd_template.R` added for `add_rmd_template()`
* New test `test-remove_rmd_template.R` added for `remove_rmd_template()`
* `alert_mar()` has been updated. Its implementation now uses data.table instead of the slider package which significantly reduces computation time.
* `alert_mar()` now returns the original data frame with algorithm variables bound to the input data
* Regression models in `alert_mar()` are now fit using `lm.fit` instead of `lm` to reduce computation time associated with extraneous computations made by `lm`.

## Other News
* `slider` package dependency no longer needed.

# Rnssp 0.0.2

## New Features
* All vignettes can be browsed with the `Rnssp_vignettes()` function
* `list_templates()` lists available templates from the [Rnssp-rmd-templates](https://github.com/CDCgov/Rnssp-rmd-templates) Github repository
* `add_rmd_template()` can now add RMarkdown templates from [Rnssp-rmd-templates](https://github.com/CDCgov/Rnssp-rmd-templates) Github repository
* `add_rmd_template()` can now run in non-interactive mode
* `remove_rmd_template()` removes an existing RMarkdown template from an existing `Rnssp` package template list.
* `change_dates()` changes the start and/or end dates in an ESSENCE API URL

## Bug Fixes
* Abstracted critical user credentials from response object.
* The `$get_api_data()` method has been extended to accept further arguments to parse CSV files.

## Other News
* Added CI/CD pipeline
* Updated all `Rnssp` vignettes.
* [Rnssp Online documentation](https://cdcgov.github.io/Rnssp/) website is now available.
* Added new hex sticker for Rnssp package branding.
* Added a `THANKS.md` to acknowledge various supports and other contributions.

# Rnssp 0.0.1

## New Features

* `alert_ewma()` implements EWMA time series anomaly detection algorithm
* `alert_mar()` implements MAR time series anomaly detection algorithm
* `classify_trend()` implements a time series trend classification algorithm
* Added a vignette for time series anomaly detection and trend classification
* Added a `NEWS.md` file to track changes to the package.

# Rnssp 0.0.0.9000
