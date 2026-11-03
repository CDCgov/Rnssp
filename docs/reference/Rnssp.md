# Tools, Functions, Shapefiles data, and Rmarkdown templates to work with Syndromic Surveillance data

The National Syndromic Surveillance Program (NSSP) at the Centers for
Disease and Control has a large Community of Practice (CoP) including
states and local Departments of Health. The NSSP has developed multiple
resources including analysis reports, analytic tools, and assisted with
scripts to interface with ESSENCE. The `Rnssp` R package is built in an
attempt to improve reusability, reproducibility, and distribution of the
analytical resources developed at the CDC NSSP.

## Rnssp functions

The Rnssp functions include:

- API functions to pull data from the ESSENCE system or any REST API
  service

- Various utility functions to securely set user credentials, add or
  remove R Markdown templates, or set new dates in API urls.

- Anomaly detection and Time Series Trend classification algorithms

- A feature to extend the package installation with NSSP custom-built
  parameterized R Markdown templates

- Utilities to run custom-built shiny apps provided by NSSP

## Rnssp vignettes

The vignettes serve as instructive tutorials demonstrating practical
uses of the Rnssp package. Please run the
[`Rnssp_vignettes()`](https://cdcgov.github.io/Rnssp/reference/Rnssp_vignettes.md)
function to browse all available vignettes.

## Rnssp Rmarkdown templates

By default, the Rnssp package comes with one R Markdown template. We
leave it up to users to decide which template to extend the package with
post-installation. To list all available template, run the
[`list_templates()`](https://cdcgov.github.io/Rnssp/reference/list_templates.md)
function. Please, check the Rnssp R markdown template Github repository
at <https://github.com/cdcgov/Rnssp-rmd-templates>.

## Rnssp Shiny Applications

The Rnssp package comes with one custom-built Rnssp Shiny application.
Users may run Rnssp custom-built shiny applications via Rnssp Rstudio
addins or via the
[`run_app()`](https://cdcgov.github.io/Rnssp/reference/run_app.md)
utility function. To list all available Rnssp shiny apps, run the
[`list_apps()`](https://cdcgov.github.io/Rnssp/reference/list_apps.md)
function. Please, check the Rnssp-shiny-appse Github repository at
<https://github.com/cdcgov/Rnssp-shiny-apps>.

## See also

Useful links:

- <https://cdcgov.github.io/Rnssp>

- <https://github.com/CDCgov/Rnssp>

## Author

Gbedegnon Roseric Azondekon (<gazondekon@cdc.gov>)
