# Rnssp 0.2.1
## Updates
* The deprecated `$get_api_tsgraph()` method has been removed.
* The `get_api_tsgraph` function has been removed as a consequence of the removal of the deprecated `$get_api_tsgraph` method.

## Bug Fixes
* The `Credentials` class has been updated. It now interfaces with API services no authorization requirement.
* The `get_essence_data()` function has been upgraded to accommodate the table builder ESSENCE API.
* The `change_dates()` function has been upgraded to fix flip-flopping start and end date values ([#13](https://github.com/CDCgov/Rnssp/issues/13))

# Rnssp 0.2.0

## New Features
* New function `create_profile()` added as a wrapper to the `$new()` method defined in the `Credentials` class.
* New function `get_essence_data()` added to specifically pull data using the NSSP-ESSENCE API.
* Negative binomial detection algorithm `alert_nbinom()` added for weekly time series of counts spanning multiple years.
* `run_app()` has been added as a utility to run embedded shiny apps.
* New `Token` class added for API services token use. The `Token` class has the same methods as the `Credentials` class.
* New function `create_token_profile()` added as a wrapper to the `$new()` method defined in the `Token` class.
* `Rnssp` detects new version and suggests package upgrade.

## Updates
* New test `test-create_profile.R` added for `create_profile()`.
* New test `test-create_token_profile.R` added for `create_token_profile()`.
* New test `test-alert_nbinom.R` added for `alert_nbinom()`.
* New test `test-alert_serfling.R` added for `alert_serfling()`.
* New test `test-get_essence_data.R` added for `get_essence_data()`.
* New test `test-get_api_graph.R` added for `get_api_graph()`.
* New test `test-get_api_tsgraph.R` added for `get_api_tsgraph()`.
* `slider` package added as a dependency.
* `lubridate` package added as a dependency.
* Deprecated `alert_mar()` function has been removed.
* `add_rmd_template()` has been improved with additional arguments and better error handling.
* `remove_rmd_template()` has been improved with additional arguments and better error handling.
* `Rnssp:::add_rmd_template_gui()` has been improved with additional arguments and better error handling.
* `Rnssp:::remove_rmd_template_gui()` has been improved with additional arguments and better error handling.
* `$get_api_response()` has been improved and now prints HTTP status code.
* `$get_api_tsgraph()` has been improved with additional arguments and now prints HTTP status code.
* The `$get_api_tsgraph()` method has been deprecated and will be removed in the next release of `Rnssp`.
* New method `$get_api_graph()` has been added as a replacement to the `$get_api_tsgraph()`.

## Bug Fixes
* Abstracted critical user credentials from the `$get_api_ts_graph()` method.
* Fix the `get_api_tsgraph()` function which was previously returning an API response object.

# Rnssp 0.1.0

## New Features
* New function `get_api_response()` added as a wrapper to the `$get_api_response()` method defined in the `Credentials` class.
* New function `get_api_data()` added as a wrapper to the `$get_api_data()` method defined in the `Credentials` class.
* New function `get_api_tsgraph()` added as a wrapper to the `$get_api_tsgraph()` method defined in the `Credentials` class.
* Regression/EWMA Switch algorithm `alert_switch()` added for anomaly detection.
* Adaptive Multiple Regression algorithm `alert_regression()` added for anomaly detection.
* Farrington Temporal Detector algorithm `alert_farrington()` added for weekly time series of counts spanning multiple years.
* ICD Code Web Scraper function `webscrape_icd()` added to web scrape ICD discharge diagnosis code sets from the CDC FTP server (for ICD-10) or CMS website (for ICD-9).
* The following Addins have been added for RStudio users:
  - Create User Profile via a Graphical User Interface (`Rnssp:::create_user_profile_gui()`)
  - Create a User Profile with a script skeleton (`Rnssp:::create_user_profile()`)
  - List available Rnssp templates (`list_templates()`)
  - Add or Update Rnssp templates (`Rnssp:::add_rmd_template_gui()`)
  - Remove Rnssp templates (`Rnssp:::remove_rmd_template_gui()`)
  - Open Rnssp templates online documentation (`Rnssp:::rnssp_templates_manual()`)
  - Open Rnssp package online documentation (`Rnssp:::rnssp_manual()`)

## Updates
* `add_rmd_template()` no longer has a `template_name` argument. The argument `template_name` has been replaced by `template` which is the first argument in the function call.
* New test `test-alert_farrington.R` added for `alert_farrington()`.
* New test `test-alert_regression.R` added for `alert_regression()`.
* New test `test-alert_switch.R` added for `alert_switch()`.
* New test `test-add_rmd_template.R` added for `add_rmd_template()`.
* New test `test-remove_rmd_template.R` added for `remove_rmd_template()`.
* New test `test-get_api_data.R` added for the `get_api_data()` function.
* New test `test-get_api_response.R` added for the `get_api_response()` function.
* New test `test-get_api_tsgraph.R` added for the `get_api_tsgraph()` function.
* New test `test-get_api_data_method.R` added for the `$get_api_data()` method.
* New test `test-get_api_response_method.R` added for the `$get_api_response()` method.
* New test `test-get_api_tsgraph_method.R` added for the `$get_api_tsgraph()` method.
* New test `test-webscrape_icd.R` added for `webscrape_icd()`
* Added shapefile for US Hospital Service Area (HSA) Shapefile (`hsa_sf`)
* Added Rnssp stopwords (`rnssp_stopwords`)
* `alert_mar()` is depecrated. Use `alert_regression` instead.
* Test `test_alert_mar.R` has been removed for `alert_mar()`.
* Regression models in `alert_regression()` are fit using `lm.fit()` instead of `lm()` to reduce computation time associated with extraneous computations made by `lm()`.
* New vignette added to demonstrate the use of Rnssp addins.

## Other News
* `slider` package dependency no longer needed.
* `cli` package is now a dependency for attractive Command Line Interfaces (CLIs)

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
