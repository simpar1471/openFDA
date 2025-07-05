
<!-- README.md is generated from README.Rmd. Please edit that file -->

# openFDA

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/openFDA)](https://CRAN.R-project.org/package=openFDA)
[![R-CMD-check](https://github.com/simpar1471/openFDA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/simpar1471/openFDA/actions/workflows/R-CMD-check.yaml)
[![check-no-suggests](https://github.com/simpar1471/openFDA/actions/workflows/check-no-suggests.yaml/badge.svg)](https://github.com/simpar1471/openFDA/actions/workflows/check-no-suggests.yaml)
[![Codecov test
coverage](https://codecov.io/gh/simpar1471/openFDA/graph/badge.svg)](https://app.codecov.io/gh/simpar1471/openFDA)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![pkgcheck](https://github.com/simpar1471/openFDA/workflows/pkgcheck/badge.svg)](https://github.com/simpar1471/openFDA/actions?query=workflow%3Apkgcheck)
<!-- badges: end -->

openFDA makes querying the [openFDA API](https://open.fda.gov/apis/)
from R a breeze. The API itself serves publicly available data from the
FDA about foods, drugs, devices, and more. This data includes data such
as recall enforcement reports, adverse events, manufacturer details, and
– again – even more! Note that the data on openFDA has not been
validated for clinical or production use.

## Installation

The easiest way to install openFDA is to get it from CRAN:

``` r
install.packages("openFDA")
```

### Development version

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

Before using the package, you will need to generate one at the [openFDA
webpage](https://open.fda.gov/apis/authentication/). With an API key you
can make 240 requests per minute, and are limited to 120,000 requests
per day (the package will handle this request rate throttling for you).
Without a key, you can only make 1000 requests a day. For this package,
if you try to run `openFDA()` without setting an API key, it will fail:

``` r
search <- openFDA(
  search = "openfda.generic_name:furosemide",
  limit = 5
)
#> Error in `get_api_key()`:
#> ! To use openFDA, you must set an openFDA API key.
#> ℹ Go to <https://open.fda.gov/apis/authentication/> to get an openFDA API key,
#>   then supply it to `set_api_key()` to cache it for use in this session.
```

That’s not good. To fix this error, get an API key from the webpage
linked above. Once you have your API key, use `set_api_key()` to store
your API key. Once you run this function, **openFDA** will ‘know’ your
API key until the session ends. Let’s try using `openFDA()` again, after
setting an API key:

``` r
# n.b. this API key is a fake, just for demonstration
set_api_key(api_key = "iHXeqlbXtlYEwX9MYLwyCBuiZxfn98Sj3oX2FNtSx")
```

``` r
search <- openFDA(
  search = "openfda.generic_name:furosemide",
  limit = 5
)

search
#> <httr2_response>
#> GET
#> https://api.fda.gov/drug/drugsfda.json?search=openfda.generic_name:furosemide&limit=5
#> Status: 200 OK
#> Content-Type: application/json
#> Body: In memory (44596 bytes)
```

The returned `httr2` response object contains JSON data from the API -
you can use `httr2::resp_body_json()` to extract the JSON data as a
nested list, then extract the fields you want.

``` r
json <- httr2::resp_body_json(search)

json$results[[1]]$openfda$brand_name
#> [[1]]
#> [1] "FUROSEMIDE"
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
#> [1] "Hospira, Inc."                  "Hikma Pharmaceuticals USA Inc."
#> [3] "Hikma Pharmaceuticals USA Inc." "Hikma Pharmaceuticals USA Inc."
#> [5] "Camber Pharmaceuticals, Inc."
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

There is also [FDAopenR](https://github.com/ck2136/FDAopenR/), which is
all object-oriented. I couldn’t quite wrap my head around it, but it all
appears to be in working order!
