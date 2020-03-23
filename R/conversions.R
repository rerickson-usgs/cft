#' Return or save a 2D raster from a 3D cstdata netcdf file dataset using an
#' aggregating function.
#'
#' @param file A single file path to a netcdf file retrieved by `cstdata()`
#' @param agg_fun The aggregating function to applied to the file variable.
#' (e.g., "mean"). (character)
#' @param save  Whether to save to file. If `FALSE`, a RasterLayer object will be
#' returned. Defaults to `FALSE`. (logical)
#' @param save_path If saving, the path to the output file. If left blank, the
#' save_path will be near copy of the input file path, replacing "daily.nc" with
#' '<agg_fun>.tif'. (character) 
#' @param overwrite If saving, whether to overwrite an existing file. Defaults
#' to FALSE. (logical)
#' @param navalue If saving, the number to use for NAN values. Defaults to
#' -9999. (numeric)
#' @param compress If saving, whether to compress the resulting file using
#' using the LZW method. Defaults to FALSE. (logical)
#' @param ncores Number of cores to use (int)
#' 
#' @return A RasterLayer object resulting the aggregation of all time periods at
#' each cell according to the aggregating function.
#' 
#' @examples 
#' \dontrun{
#' file_ref <- cstdata(park = "Acadia National Park", 
#'                     parameters = "pr", 
#'                     years = c(2020, 2021), 
#'                     models = "CCSM4", 
#'                     scenarios = "rcp85")
#' r <- cst_raster(file_ref$path[1],agg_fun = "min")
#' }
#'
#' @export
cst_raster <- function(file, agg_fun = "mean", save = FALSE, save_path, overwrite = FALSE, 
                       navalue = -9999., compress = FALSE, ncores = 1) {

  # Set to folder within the root location directory if no save_dir
  if (missing(save_path) & save) {
    save_path = gsub("daily.nc", paste(agg_fun, "tif", sep = "."), file)
  }
  
  # Match the aggregation function string to a function 
  tryCatch({
    fun <- match.fun(agg_fun)
  }, error = function(e) {
    stop(paste0("The aggregation function '", agg_fun,
                "' is not available."))
  })

  # Open file with raster for some attributes
  b <- raster::brick(x = file)

  # Open it with ncdf4 for the variable name and matrix  
  nc <- ncdf4::nc_open(file)
  variable_name <- names(nc$var)
  values <- ncdf4::ncvar_get(nc, variable_name)
  x <- t(apply(values, c(1, 2), fun))
  r <- raster::raster(x)
  raster::extent(r) <- raster::extent(b)
  raster::crs(r) <- raster::crs(b)

  # Return as raster object, write to file if
  if (save) {
    if (compress) {
      options <- c("COMPRESS=LZW")
    } else {
      options <- NULL
    }
    raster::writeRaster(r, save_path, format = "GTiff", prj = raster::crs(b),
                        overwrite = overwrite, progress = "text", NAflag = navalue,
                        options = options)
  } else {
    return(r)
  }

}
