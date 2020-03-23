reticulate::use_condaenv("cst")

local_dir <- "."
file_refs <- cstdata(park = "Acadia National Park",
                     years = c(2004, 2005),
                     models = c("bcc-csm1-1"),
                     parameters = c("pr", "tasmax"),
                     scenarios = c("rcp45"),
                     local_dir = ".", 
                     ncores = 2)

test_that("cst_raster returns a raster object.", {

  # Get sample file
  file <- file_refs$local_path[[2]]

  # Create 2D raster and save to default
  r <- cst_raster(file, agg_fun = "min")

  # Check path and object
  expect_s4_class(r, "RasterLayer")
})

test_that("cst_raster writes a raster file.", {

  # Get sample file
  file <- file_refs$local_path[[2]]

  # Create 2D raster and save to default
  cst_raster(file, agg_fun = "min", save = TRUE, compress = TRUE)
  
  expec_path <- gsub("daily.nc", paste("min", "tif", sep = "."), file)
  expect_true(file.exists(expec_path))
})
