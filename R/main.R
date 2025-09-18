#' @title Get Routes with Asynchronous Requests and Parallelism
#' @description
#' This function enables the fast computation of routes with async requests and
#' parallelism.
#'
#' @param x data.frame with x_src, y_src, x_dst, y_dst in WGS84 coordinates
#' (EPSG:4326)
#' @param overview if FALSE only distance and duration are computed and the
#' output is a matrix; if TRUE the "full" geometry is also computed and the
#' result is an sf object.
#' @param max_radius maximum distance between input coordinates and the road
#' network. If this distance is exceeded, then the road is not computed.
#' @param nc number of CPU cores
#' @param nq number of queries per chunk
#' @param server OSRM server address
#' @param profile OSRM profile
#' @importFrom foreach foreach %dopar%
#' @export
#' @return An sf object or a matrix is returned.
#' @examples
#' \dontrun{
#' x <- read.csv(system.file("csv/berlin.csv", package = "batchosrm"))
#' res <- routes(
#'   x = x[1:2000,],
#'   overview = TRUE,
#'   max_radius = 1000,
#'   nc = 8,
#'   nq = 150,
#'   server = "http://0.0.0.0:5000/",
#'   profile = "car"
#' )
#'
#' library(sf)
#' library(mapsf)
#' mf_theme("dracula")
#' st_transform(res, "EPSG:3035") |>
#'   mf_map(col = "#ffffff05", lwd = 1.2)
#' mf_title("Berlin")
#' }
routes <- function(x,
                   overview = FALSE,
                   max_radius = 1000,
                   nc = 1,
                   nq = 150,
                   server = "http://0.0.0.0:5000/",
                   profile = "car") {
  ny <- nrow(x)
  sequence <- unique(c(seq(1, ny, nq), ny + 1))
  lseq <- length(sequence) - 1
  ml <- list()
  for  (i in 1:lseq) {
    ml[[i]] <- as.matrix.data.frame(x[(sequence[i]):(sequence[i + 1] - 1),])
  }

  cl <- parallel::makeCluster(nc)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl))

  res <- foreach(x = ml, .combine = rbind, .inorder = FALSE,
                 .export = c("get_routes")) %dopar%
    {
      get_routes(x, overview, max_radius, server, profile)
    }

  return(res)

}



#' @title Get Routes with Asynchronous Requests and Parallelism
#' @description
#' This function enables the fast computation of routes with async requests.
#'
#' @param x data.frame with x_src, y_src, x_dst, y_dst in WGS84 coordinates
#' (EPSG:4326)
#' @param overview if FALSE only distance and duration are computed and the
#' output is a matrix; if TRUE the "full" geometry is also computed and the
#' result is an sf object.
#' @param max_radius maximum distance between input coordinates and the road
#' network. If this distance is exceeded, then the road is not computed.
#' @param server OSRM server address
#' @param profile OSRM profile
#' @export
#' @return An sf object or a matrix is returned.
#' @examples
#' \dontrun{
#' x <- read.csv(system.file("csv/berlin.csv", package = "batchosrm"))
#' res <- get_routes(
#'   x = x[1:2000,],
#'   overview = TRUE,
#'   max_radius = 1000,
#'   server = "http://0.0.0.0:5000/",
#'   profile = "car"
#' )
#'
#' library(sf)
#' library(mapsf)
#' mf_theme("dracula")
#' st_transform(res, "EPSG:3035") |>
#'   mf_map(col = "#ffffff05", lwd = 1.2)
#' mf_title("Berlin")
#' }
get_routes <- function(x, overview = FALSE, max_radius = 1000, server = "http://0.0.0.0:5000/", profile = "car"){
  x <- round(x, 5)
  urls <- paste0(server, "route/v1/", profile, "/", x[,1],",",x[,2], ";",x[,3], ",", x[,4],
                 "?overview=full")

  reqlist <- lapply(urls, function(x) {
    crul::HttpRequest$new(
      url = x,
      opts = list(
        verbose = FALSE,
        useragent = "batchosrm",
        connecttimeout = 1000000
      ),
      headers = list("Content-Type" = "application/json")
    )$get()
  })

  out <- crul::AsyncQueue$new(.list = reqlist, req_per_min = 1000000)

  out$request()

  res <- out$responses()

  f <- extract_route(overview)

  return(do.call(rbind, lapply(res, f, max_radius)))
}

extract_route <- function(overview){
  if (isFALSE(overview)){
    function(r, max_radius){
      tryCatch(
        expr = {
          res <- RcppSimdJson::fparse(rawToChar(r$content))
          if (any(res$code != "Ok", res$waypoints$distance >=  max_radius )) {
            return(c(duration = NA, distance = NA))
          }
          return(round(c(duration = res$routes$duration / 60,
                  distance = res$routes$distance / 1000),2))
        },
        error = function(cond){
          return(c(duration = NA, distance = NA))
        }
      )
    }
  } else {
    function(r, max_radius){
      tryCatch(
        expr = {
          res <- RcppSimdJson::fparse(rawToChar(r$content))
          if (any(res$code != "Ok", res$waypoints$distance >=  max_radius )) {
            return(sf::st_sf(duration = NA,
                             distance = NA,
                             geometry = sf::st_sfc(sf::st_linestring()),
                             crs = "EPSG:4326"))
          }
          geodf <- googlePolylines::decode(res$routes$geometry)[[1]][, c(2, 1)]
          sf::st_sf(
            duration = round(res$routes$duration / 60, 2),
            distance = round(res$routes$distance / 1000, 2),
            geometry = sf::st_as_sfc(paste0("LINESTRING(", paste0(geodf$lon, " ", geodf$lat, collapse = ", "), ")")),
            crs = "EPSG:4326")
        },
        error = function(cond){
          sf::st_sf(duration = NA,
                    distance = NA,
                    geometry = sf::st_sfc(sf::st_linestring()),
                    crs = "EPSG:4326")
        }
      )
    }
  }
}
