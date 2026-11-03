# Parse and Explain URL Query Parameters

Extracts and parses the query parameters from a given URL. It splits the
query string into key-value pairs and returns a structured dataframe.

## Usage

``` r
explain_url(url)
```

## Arguments

- url:

  A character string representing the URL to be parsed.

## Value

A dataframe with two columns: `Key` and `Value`, containing the parsed
query parameters.

## Examples

``` r
explain_url("https://example.com/page?name=John&age=30&city=New%20York")
```
