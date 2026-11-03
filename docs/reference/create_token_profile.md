# Create a profile with an API token

A wrapper to the `new` method defined in the
[`Token`](https://cdcgov.github.io/Rnssp/reference/Token.md) class.

## Usage

``` r
create_token_profile(
  token = getPass::getPass("Enter/Paste a token: "),
  auth_type = "Bearer"
)
```

## Arguments

- token:

  a string for token

- auth_type:

  type of HTTP authentication. Should be `Bearer` or `Basic`. Default is
  `Bearer`

## Value

An object of class `Token`

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a profile with "Bearer" authentication type
myProfile <- create_token_profile()
myProfile

# Create a profile with a "Basic" authentication type
myProfile2 <- create_token_profile(auth_type = "Basic")
myProfile2
} # }
```
