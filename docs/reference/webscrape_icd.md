# ICD Code Web Scraper

Function to web scrape ICD discharge diagnosis code sets from the CMS
website. If pulling ICD-10 codes, by default the function will attempt
to pull the diagnosis code update file for the current year. Users can
specify earlier publication years from the last 6 years if needed. The
ICD-9 option will only pull the most recent, final ICD-9 code set
publication from 2014. This function will return an error message if the
requested diagnosis code update file is not yet available or if the CMS
website is unresponsive. The result is a tibble with 5 fields: version,
code, description, published_year, and fy_update_year. Codes are
standardized to upper case with punctuation and extra leading/tailing
white space removed to enable successful joining.

## Usage

``` r
webscrape_icd(icd_version = "ICD10", year = NULL, quiet = FALSE)
```

## Arguments

- icd_version:

  A character value of either "ICD10" or "ICD9" to specify ICD version

- year:

  A numeric integer indicating the publication year of desired ICD-10
  code set. ICD-10 diagnosis code update files are published on October
  1 of each year. Defaults to `NULL` to pull the most recent year's
  publication. An error will occur if this argument is used in
  combination with setting\`icd_version\` to "ICD9".

- quiet:

  logical. If `TRUE`, suppress status messages (if any), and the
  progress bar.

## Value

A tibble

## References

- [CMS website for published ICD-10 diagnosis code update
  files](https://www.cms.gov/medicare/coding-billing/icd-10-codes)

- [CMS website for published ICD-9 diagnosis code
  files](https://www.cms.gov/medicare/coding-billing/icd-10-codes/icd-9-cm-diagnosis-procedure-codes-abbreviated-and-full-code-titles)

## Examples

``` r
# Example 1
icd9_2014 <- webscrape_icd(icd_version = "ICD9")
head(icd9_2014)

# Example 2
icd10_2025 <- webscrape_icd(icd_version = "ICD10", year = 2025)
head(icd10_2025)

# Example 3
icd10_2026 <- webscrape_icd(icd_version = "ICD10", year = 2026)
head(icd10_2026)
```
