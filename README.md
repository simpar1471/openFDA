
<!-- README.md is generated from README.Rmd. Please edit that file -->

# openFDA

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/openFDA)](https://CRAN.R-project.org/package=openFDA)
[![R-CMD-check](https://github.com/simpar1471/openFDA/actions/workflows/R-CMD-check/badge.svg)](https://github.com/simpar1471/openFDA/actions/workflows/R-CMD-check.yaml)
[![R-CMD-check-no-suggests](https://github.com/simpar1471/openFDA/actions/workflows/check-no-suggests/badge.svg)](https://github.com/simpar1471/openFDA/actions/workflows/check-no-suggests.yaml)
[![Codecov test
coverage](https://codecov.io/gh/simpar1471/openFDA/graph/badge.svg)](https://app.codecov.io/gh/simpar1471/openFDA)
<!-- badges: end -->

openFDA makes querying the [openFDA API](https://open.fda.gov/apis/)
from R a breeze. The API itself serves publicly available data from the
FDA about foods, drugs, devices, and more. This data includes data such
as recall enforcement reports, adverse events, manufacturer details,
and - again - even more! Note that the data on openFDA has not been
validated for clinical or production use.

## Installation

``` r
# The easiest way to get openFDA is to install it from CRAN:
install.packages("openFDA")
```

### Dev version

Alternatively, you can install the development version of openFDA from
GitHub using either [pak](https://pak.r-lib.org/):

``` r
# install.packages("pak")
pak::pkg_install("simpar1471/openFDA")
```

## Using openFDA

``` r
library(openFDA)
```

The full documentation for the API is online, so look at the [openFDA
website](https://open.fda.gov/apis/) to get a full feel for the API
itself.

The R package lets you query the API directly from R, using
[httr2](https://httr2.r-lib.org/).

``` r
search <- openFDA(
  search = "openfda.generic_name:furosemide",
  limit = 5
)

search
#> <httr2_response>
#> GET
#> https://api.fda.gov/drug/drugsfda.json?api_key=[API_KEY]&search=openfda.generic_name:furosemide&limit=5
#> Status: 200 OK
#> Content-Type: application/json
#> Body: In memory (26060 bytes)
```

The underlying response is JSON data - you can use
`httr2::resp_body_json()` to get the JSON data as a nested list, then
extract the fields you want.

``` r
json <- httr2::resp_body_json(search)

json$results[[1]]$openfda$brand_name
#> [[1]]
#> [1] "FUROSCIX"
json$results[[1]]$openfda$pharm_class_epc
#> [[1]]
#> [1] "Loop Diuretic [EPC]"
```

I’ve found [purrr](https://purrr.tidyverse.org/) to be very useful for
parsing this data quickly.

``` r
purrr::map_chr(
  .x = json$results, 
  .f = \(result) purrr::pluck(result, "openfda", "manufacturer_name", 1)
)
#> [1] "scPharmaceuticals Inc."                 
#> [2] "Graviti Pharmaceuticals Private Limited"
#> [3] "Hikma Pharmaceuticals USA Inc."         
#> [4] "Civica, Inc."                           
#> [5] "Eugia US LLC"
```

## Other R packages for openFDA

The [openfda](https://github.com/rOpenHealth/openfda) package from
`rOpenHealth` also wraps the openFDA API from R, and is available on
GitHub. It’s got a pretty neat structure whereby you build up a query
using individual functions for each parameter - though it’s my personal
preference to keep these as parameters to a single function. It also
makes results available in data frames, which is nice, but I think
working with the response object and parsing the underlying JSON
yourself permits more powerful interactions with the API.

There is also [FDAopenR](https://github.com/ck2136/FDAopenR/), which I
couldn’t quite wrap my head around. The package appears to be in working
order, though!
