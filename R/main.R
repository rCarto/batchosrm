#' @title Get Routes
#' @description
#' A short description...
#'
#' @param x dataframe with x_src, y_src, x_dst, y_dst in epsg:4326
#' @param overview overview (FALSE, "simplified" or "full)
#' @param nc number of CPU cores
#' @param nq number of queries per chunk
#' @param server server address
#' @param profile profil
#' @importFrom foreach foreach %dopar%
#' @export
#' @return smg
#' @examples
#' \dontrun{
#' library(osrm)
#' apt <- read.csv(system.file("csv/apotheke.csv", package = "osrm"))
#' x <- cbind(apt[c(1:100,1:100,1:100, 1:100), c(2:3)],
#'            apt[c(rep(1,100), rep(10,100), rep(20,100), rep(30, 100)), c(2:3)])
#' server = "http://xxxxxxx:5000/"
#' r <- routes(x, nc = 8, nq = 50, server = server, overview = "full")
#' library(mapsf)
#' op <- par(mfrow = c(2,2))
#' mf_map(r[1:100,], col = "red")
#' mf_map(r[101:200,], col = "blue")
#' mf_map(r[201:300,], col = "darkgreen")
#' mf_map(r[301:400,], col = "purple")
#' par(op)
#' }
routes <- function(x,
                   overview = "simplified",
                   nc = 1, nq = 100,
                   server,
                   profile = "car"){
  ny <- nrow(x)
  sequence <- unique(c(seq(1, ny, nq), ny + 1))
  lseq <- length(sequence) - 1
  ml <- list()
  for  (i in 1:lseq) {
    ml[[i]] <- as.matrix.data.frame(x[(sequence[i]):(sequence[i + 1] - 1),])
  }

  cl <- parallel::makeCluster(nc, setup_strategy = "sequential")
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl))


  if(overview == FALSE){
    res <- foreach(x = ml, .combine = rbind, .inorder = FALSE,
                   .export = c("bq", "cp")) %dopar%
      {
        t(sapply(apply(x, 1, bq, server = server, profile = profile, overview = "full"), cp))
      }


  } else {
    res <- foreach(x = ml, .combine = rbind, .inorder = FALSE,
                   .export = c("bq", "cpgeom")) %dopar%
      {
        do.call(rbind, lapply(apply(x, 1, bq, server = server, profile = profile, overview = "full"), cpgeom))
      }
  }
  return(res)

}








bq <- function(x, server, profile, overview){
  x <- round(x, 5)
  utils::URLencode(paste0(server, "route/v1/", profile, "/",
                          x[1],",",x[2], ";",x[3], ",", x[4],
                          "?alternatives=false&geometries=polyline&steps=false&overview=",
                          tolower(overview),
                          "&generate_hints=false"))


}

cp <- function(q, req_handle){
  tryCatch(
    expr = {
      req_handle <- curl::new_handle(verbose = FALSE)
      curl::handle_setopt(req_handle, useragent = "osrm_R_package")
      r <- curl::curl_fetch_memory(q, handle = req_handle)
      res <- RcppSimdJson::fparse(rawToChar(r$content))
      round(c(duration = res$routes$duration / 60, distance = res$routes$distance / 1000),2)
    },
    error = function(cond){
      return(c(duration = NA, distance = NA))
    }
  )
}

cpgeom <- function(q, req_handle){
  tryCatch(
    expr = {
      req_handle <- curl::new_handle(verbose = FALSE)
      curl::handle_setopt(req_handle, useragent = "osrm_R_package")
      r <- curl::curl_fetch_memory(q, handle = req_handle)
      res <- RcppSimdJson::fparse(rawToChar(r$content))
      geodf <- googlePolylines::decode(res$routes$geometry)[[1]][, c(2, 1)]
      # Convert to LINESTRING
      rcoords <- paste0(geodf$lon, " ", geodf$lat, collapse = ", ")
      rosf <- sf::st_sf(
        duration = round(res$routes$duration / 60, 2),
        distance = round(res$routes$distance / 1000, 2),
        geometry = sf::st_as_sfc(paste0("LINESTRING(", rcoords, ")")),
        crs = "EPSG:4326")

    },
    error = function(cond){
      sf::st_sf(duration = NA, distance = NA,
                geometry = sf::st_sfc(sf::st_linestring()), crs = "EPSG:4326")
    }
  )
}
