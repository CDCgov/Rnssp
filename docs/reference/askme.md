# Password Prompt Utility

Prompt the user for a password. This function is a wrapper for the
[`getPass`](https://rdrr.io/pkg/getPass/man/getPass.html) function.

## Usage

``` r
askme(prompt = "Please enter your password: ")
```

## Value

a character string

## See also

[`getPass`](https://rdrr.io/pkg/getPass/man/getPass.html)

## Examples

``` r
password <- askme()
```
