
# batchosrm: Access to the OpenStreetMap-based routing service OSRM for fast route extraction

<!-- badges: start -->
<!-- badges: end -->

An interface between R and the OSRM API.
[OSRM](https://github.com/Project-OSRM/osrm-backend) is a routing
service based on [OpenStreetMap](http://project-osrm.org) data.

This single function package enables the fast computation of routes and
is a much faster alternative to repeated calls to
[`osrm::osrmRoute()`](https://github.com/riatelab/osrm).

## Installation

You can install the development version of batchosrm with:

``` r
remotes::install_github("rCarto/batchosrm")
```

## Example

``` r
library(tictoc)
library(batchosrm)
library(mapsf)

# build a large dataset of origins and destination from osrm package sample dataset
apt <- read.csv(system.file("csv/apotheke.csv", package = "osrm"))
set.seed(42)
o <- sample(1:100, 1000, replace = T)
set.seed(666)
d <- sample(1:100, 1000, replace = T)
x <- cbind(apt[o, c(2:3)], apt[d, c(2:3)])
row.names(x) <- 1:nrow(x)

# Query duration, distance and geometry
tic()
r <- routes(x, nc = 8, nq = 50, server = "http://0.0.0.0:5000/", overview = "full")
toc()
#> 5.46 sec elapsed

mf_map(r, col = "#94000010", lwd = 1)
```

![](https://i.imgur.com/r9rDFbR.png)<!-- -->

``` r
# Query only duration and distance
tic()
z <- routes(x, nc = 8, nq = 50, server = "http://0.0.0.0:5000/", overview = FALSE)
toc()
#> 4.466 sec elapsed
head(z)
#>   duration distance
#> 1    18.86     8.38
#> 2    34.48    20.08
#> 3    18.33     9.85
#> 4    17.59     9.19
#> 5    15.18     8.61
#> 6    28.56    17.84
```
