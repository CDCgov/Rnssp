# Rnssp Stopwords

A dataset containing:

- Medical Process Terms: Procedural terms used in medical setting
  (examples: administered, tested, treatment)

- Stop Words: Standard English stopwords (examples: some, then, would)

- Ignored Terms: Custom list of uninformative terms that frequently
  occur in syndromic surveillance data sources (examples: unspecified,
  encounter, midnight)

## Usage

``` r
rnssp_stopwords
```

## Format

A data frame with 835 rows and 2 variables:

    Rows: 835
    Columns: 2
    $ word <chr> "ABNORMAL", "ACCESS", "ACHE", "ACHES", "ACTIVE", "ACUTE", "ADMINISTERED", "ADVICE", "AFFECTED", "AFFORD", "AFTERWARDS", "AGO", "ALMOST", …
    $ type <chr> "Medical Process Terms", "Medical Process Terms", "Medical Process Terms", "Medical Process Terms", "Medical Process Terms", "Medical Pro…

## Examples

``` r
library(Rnssp)
data("rnssp_stopwords")

glimpse(rnssp_stopwords)
```
