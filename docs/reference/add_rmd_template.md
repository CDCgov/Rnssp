# Utility to Add a New RMarkdown Template

Add an NSSP RMarkdown template report to extend an existing installation
of the Rnssp package.

## Usage

``` r
add_rmd_template(
  template = NULL,
  pkg = "Rnssp",
  restart = TRUE,
  verbose = TRUE
)
```

## Arguments

- template:

  a character string with the name of a single template name. The
  template name must be one of the elements of the vector returned by
  [`list_templates`](https://cdcgov.github.io/Rnssp/reference/list_templates.md).

- pkg:

  a character string with the name of a single package. An error occurs
  if more than one package name is given.

- restart:

  logical. Should R session be restarted after template installation?
  (default is TRUE)

- verbose:

  logical. Should alert session restart message be printed?

## Value

a character string

## Details

In interactive mode, this utility function prompts the user to select an
Rmarkdown template .zip file when the `template` argument is not
specified or set to `NULL`.

In non-interactive mode, this utility function prompts the user to
specify a path to an Rmarkdown template .zip file when the `template`
argument is not specified or set to `NULL`.

When the `template` is specified, regardless of the `pkg` argument, this
utility function download the specified template from the
Rnssp-rmd-templates Github repository. When the specified template name
is not available, it throws an error.

## See also

<https://github.com/CDCgov/Rnssp-rmd-templates>

## Examples

``` r
if (FALSE) { # \dontrun{
add_rmd_template() # Add a new Rmd template to the 'Rnssp' package
add_rmd_template(pkg = "rmarkdown") # Add a new Rmd template to the 'rmarkdown' package
add_rmd_template("text_mining") # Add the 'text_mining' template report to the Rnssp package
} # }
```
