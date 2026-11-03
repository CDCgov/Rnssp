# An `Auth` Class is an abstract authentication class

An `Auth` has a private key attribute.

## Methods

### Public methods

- [`NSSPAuth$get_api_data()`](#method-NSSPAuth-get_api_data)

- [`NSSPAuth$clone()`](#method-NSSPAuth-clone)

------------------------------------------------------------------------

### `NSSPAuth$get_api_data()`

Get API data

#### Usage

    NSSPAuth$get_api_data(url, fromCSV = FALSE, ...)

#### Arguments

- `url`:

  a character of API URL

- `fromCSV`:

  a logical, defines whether data are returned in .csv format or .json
  format

- `...`:

  further arguments and CSV parsing parameters to be passed to
  [`read_csv`](https://readr.tidyverse.org/reference/read_delim.html)
  when `fromCSV = TRUE`.

#### Returns

a dataframe (`fromCSV = TRUE`) or a list containing a dataframe and its
metadata (`fromCSV = TRUE`)

------------------------------------------------------------------------

### `NSSPAuth$clone()`

The objects of this class are cloneable with this method.

#### Usage

    NSSPAuth$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
