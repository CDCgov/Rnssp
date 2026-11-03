# Create a profile with a username and password

A wrapper to the `new` method defined in the
[`Credentials`](https://cdcgov.github.io/Rnssp/reference/Credentials.md)
class.

## Usage

``` r
create_profile(
  username = askme("Please enter your username: "),
  password = askme()
)
```

## Arguments

- username:

  a string for username

- password:

  a string for password

## Value

an object of class NSSPCredentials and R6

## Examples

``` r
if (FALSE) { # \dontrun{
myProfile <- create_profile()
myProfile
} # }
```
