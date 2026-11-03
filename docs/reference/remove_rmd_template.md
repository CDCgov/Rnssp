# Utility to Remove an Existing RMarkdown Template

Remove an Existing Rnssp Rmarkdown template directory.

## Usage

``` r
remove_rmd_template(
  template,
  pkg = "Rnssp",
  recursive = TRUE,
  force = TRUE,
  restart = TRUE,
  verbose = TRUE
)
```

## Arguments

- template:

  a character string with the name of the template to delete

- pkg:

  a character string with the name of a single package. An error occurs
  if more than one package name is given.

- recursive:

  logical. Should directories be deleted recursively? (Default is TRUE)

- force:

  logical. Should permissions be changed (if possible) to allow the file
  or directory to be removed? (Default is TRUE)

- restart:

  logical. Should R session be restarted after template removal?
  (default is TRUE)

- verbose:

  logical. Should alert session restart message be printed?

## Value

a character string

## Examples

``` r
if (FALSE) { # \dontrun{
remove_rmd_template("text_mining") # Remove the Existing Rnssp 'text_mining' template
} # }
```
