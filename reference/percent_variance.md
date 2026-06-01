# Calculate the Percent of Variance Explained by Components in SVD

Calculate the percent of variance in a dataset that can be explained by
each component of a singular value decomposition. The dataset should
include age, a single sex or gender category, and one or more
classifying variables.

## Usage

``` r
percent_variance(
  data,
  transform,
  measure = "value",
  cols = c("country", "time"),
  n_comp = 5,
  eps = 1e-05
)
```

## Arguments

- data:

  A data frame

- transform:

  Transformation to apply to the data: `"log"`, `"logit"`, or `"none"`.

- measure:

  Name of the rate or proportion being modelled. Default is `"value"`.

- cols:

  Names of the categorical variables used to distinguish different age
  profiles.

- n_comp:

  Number of components to give percentages for. Default is `5`.

- eps:

  Parameter controlling truncation. See below for details. Default is
  `0.00001`.

## Value

A numeric vector of length `n_comp`.

## Details

`data` must contain a variable called `"age"`. It must also contain the
variables named by the `measure` and `cols` arguments. The age and
`cols` arguments must uniquely identify all cells in the dataset.

When `transform` is `"log"` or `"logit"`, `percent_variance()` converts
any values for the measure variable that are less than `eps` to values
greater than or equal to `eps` before transforming. The converted value
is based on on a main effects model.

When `transform` is `"logit"`,
`precent_variange() converts any values greater than `1-eps`to values less than or equal to`1-eps\`
before transforming. The converted value is based on main effects model.

## Examples

``` r
asfr_subset |>
  tidy_hfd() |>
  percent_variance(transform = "log")
#> Component 1 Component 2 Component 3 Component 4 Component 5 
#>    99.29102    99.60120    99.71626    99.80913    99.86552 
```
