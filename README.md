
# batchosrm: Access to the OpenStreetMap-based routing service OSRM for fast route extraction

<!-- badges: start -->

<!-- badges: end -->

An interface between R and the OSRM API.
[OSRM](https://github.com/Project-OSRM/osrm-backend) is a routing
service based on [OpenStreetMap](http://project-osrm.org) data.

This package enables the fast computation of routes and is a much faster
alternative to repeated calls to
[`osrm::osrmRoute()`](https://github.com/riatelab/osrm).

## Installation

You can install the development version of `batchosrm` with:

``` r
remotes::install_github("rCarto/batchosrm")
```

## Example

``` r
library(tictoc)
library(batchosrm)
x <- read.csv(system.file("csv/berlin.csv", package = "batchosrm"))
tic(msg = "Query durations, distances and geometries")
res <- routes(
  x = x[1:2000,],
  overview = TRUE,
  max_radius = 1000,
  nc = 8,
  nq = 150,
  server = "http://0.0.0.0:5000/",
  profile = "car"
)
toc()
#> Query durations, distances and geometries: 4.228 sec elapsed

library(sf)
#> Linking to GEOS 3.13.1, GDAL 3.10.3, PROJ 9.6.0; sf_use_s2() is TRUE
library(mapsf)
mf_theme("dracula")
st_transform(res, "EPSG:3035") |>
  mf_map(col = "#ffffff05", lwd = 1.2)
mf_title("Berlin")
```

![](https://i.imgur.com/OSu4GlL.png)<!-- -->

``` r

tic(msg = "Query only durations and distances")
rex <- routes(
  x = x[1:2000,],
  overview = FALSE,
  max_radius = 1000,
  nc = 8,
  nq = 150,
  server = "http://0.0.0.0:5000/",
  profile = "car"
)
toc()
#> Query only durations and distances: 2.709 sec elapsed
rex[1:5,]
#>      duration distance
#> [1,]    21.39    12.70
#> [2,]    34.23    19.49
#> [3,]    20.74     9.60
#> [4,]    11.05     4.81
#> [5,]    14.85    11.56
```
