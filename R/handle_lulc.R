# -----------------------------------------------------------------------------
# This function downloads all mapbiomas land cover data
# (for the years provided).
# It also subsets the downloadeds data for the region of interest
# (must be the same as the region provided to create the reference grid).
download_mapbiomas <-
  function(
    # Years of data to be downloaded (should be a vector of integers)
    years = c(2001L:2020L),
    # The name of a Geopackage file located in the data directory
    # (you must put the file there)
    area_of_interest = "sp"
  ) {
    # Load area of interest
    aoi <- sf::read_sf(dsn = glue::glue("./data/{area_of_interest}.gpkg"))

    # Get bounding box of interest
    bbox <- aoi |>
      sf::st_transform(crs = "EPSG:4326") |>
      sf::st_bbox()

    # Download and subset data for each year
    purrr::walk(
      .x = years,
      \(y) {
        # Create temporary directory
        file_dir <- tempdir()
        fs::dir_create(path = glue::glue("{file_dir}/raster"))
        file_dir <- glue::glue("{file_dir}/raster")

        # Set path for downloaded data
        file_path <- glue::glue("{file_dir}/mapbiomas_{y}.tif")

        # This is the url for the direct download of mapbiomas data
        img_url <-
          glue::glue(
            "https://storage.googleapis.com/mapbiomas-public/initiatives/",
            "brasil/collection_9/lclu/coverage/brasil_coverage_{y}.tif"
          )

        # Download data ----
        # Set download options
        h <- curl::new_handle()
        curl::handle_setopt(handle = h, ssl_verifypeer = FALSE)

        # Download data to temporary directory
        curl::curl_download(url = img_url, destfile = file_path)

        # Load downloaded image
        img <- stars::read_stars(.x = file_path)

        # Transform area of interest bbox to mapbiomas CRS
        stars_bbox <- bbox |>
          sf::st_as_sfc() |>
          sf::st_transform(crs = sf::st_crs(img)) |>
          sf::st_bbox()

        # Crop mapbiomas raster to area of interest bounding box
        img <- img |>
          sf::st_crop(y = stars_bbox)

        # Save mapbiomas raster to final destination
        stars::write_stars(
          obj = img,
          dsn = glue::glue(
            "./data/raw/lulc/mapbiomas_{y}.tif"
          ),
          options = c("COMPRESS=LZW"),
          progress = FALSE
        )

        unlink(x = file_dir, recursive = TRUE)
      }
    )

    return(invisible(NULL))
  }

# -----------------------------------------------------------------------------
# This function processes the downloaded mapbiomas data.
# It calculates the proportion of each land use of interest in the area of
# each cell of the bigger reference grid.
process_mapbiomas <-
  function(
    # Years of data to be processed (should be a vector of integers)
    years = c(2001L:2020L),
    # The name of a Geopackage file located in the data directory
    # (you must put the file there)
    area_of_interest = "sp",
    # A scale factor to downsample the mapbiomas files
    # (the downsample method is the mode)
    scale_factor = 1
    # So if the mapbiomas have spatial resolution of 30 meters,
    # and you supply a `scale_factor` of 3, it will perform the mode of a 3x3
    # window of pixels, generating a new pixel of 90 meters.
    # The scale factor should also make the mapbiomas data resolution close to
    # the smaller reference grid.
  ) {
    # Load area of interest
    aoi <-
      sf::read_sf(dsn = glue::glue("./data/{area_of_interest}.gpkg")) |>
      sf::st_transform(sf::st_crs(readr::read_file("./data/project_crs.txt")))

    ## Prepare to read and process data ---------------------------------------

    # Crop mapbiomas proxy raster to aoi bounding box
    # Get mapbiomas file proxy
    raster_meta <-
      stars::read_stars(
        .x = fs::dir_ls(path = "./data/raw/lulc/")[1],
        proxy = TRUE
      )

    # Get the dimensions of mapbiomas raster proxy
    raster_dim <- stars::st_dimensions(.x = raster_meta)

    # Make a list with rasterio options for reading raster
    # This will use the scale_factor argument to downscale the raster data
    # in the reading process
    rasterio <-
      list(
        nXOff = raster_dim$x$from,
        nYOff = raster_dim$y$from,
        nXSize = raster_dim$x$to - raster_dim$x$from,
        nYSize = raster_dim$y$to - raster_dim$y$from,
        nBufXSize = (raster_dim$x$to - raster_dim$x$from) / scale_factor,
        nBufYSize = (raster_dim$y$to - raster_dim$y$from) / scale_factor,
        resample = "mode" # Method of downscaling
      )

    # Read reference grids
    reference_small <- stars::read_stars(.x = "./data/reference_small.tif")
    reference_big <- stars::read_stars(.x = "./data/reference_big.tif")

    # This creates a new factor to transform the loaded mapbiomas data into
    # the same spatial resolution of the bigger reference grid
    downsample_factor <-
      stars::st_res(x = reference_big)[[1]] /
      stars::st_res(x = reference_small)[[1]] -
      1 # Important to subtract by 1 (read the `stars::st_downsample` docs)

    # This creates a new factor to calculate the proportion of each land use
    # inside the downsampled cells
    proportion_factor <-
      (stars::st_res(x = reference_big)[[1]] /
        stars::st_res(x = reference_small)[[1]])^2

    ## Read and process mapbiomas data ----------------------------------------

    # Process data for each year
    lulc_stars_list <-
      purrr::map(
        .x = years,
        .f = \(y) {
          # Read subset of mapbiomas raster using rasterio
          # Warp the mapbiomas data to the smaller reference grid
          lulc_stars <-
            stars::read_stars(
              .x = glue::glue("./data/raw/lulc/mapbiomas_{y}.tif"),
              RasterIO = rasterio,
              NA_value = 0,
              proxy = TRUE
            ) |>
            stars::st_warp(
              dest = reference_small,
              use_gdal = TRUE,
              method = "near",
              no_data_value = 0
            ) |>
            stars::st_as_stars() |>
            rlang::set_names(nm = "lulc")

          # Reclassify values of land use of interest to 1
          lulc_stars[["urban"]] <-
            ifelse(
              test = lulc_stars[["lulc"]] == 24,
              yes = 1,
              no = 0
            )
          lulc_stars[["forest"]] <-
            ifelse(
              test = lulc_stars[["lulc"]] == 3,
              yes = 1,
              no = 0
            )
          lulc_stars[["agriculture"]] <-
            ifelse(
              test = lulc_stars[["lulc"]] %in%
                c(39, 20, 40, 62, 41, 46, 47, 35, 48),
              yes = 1,
              no = 0
            )
          lulc_stars[["pasture"]] <-
            ifelse(
              test = lulc_stars[["lulc"]] %in% c(15),
              yes = 1,
              no = 0
            )
          lulc_stars[["mosaic"]] <-
            ifelse(
              test = lulc_stars[["lulc"]] %in% c(21),
              yes = 1,
              no = 0
            )

          # Downsample loaded mapbiomas data to match the bigger reference grid
          lulc_stars <-
            lulc_stars |>
            dplyr::select(!lulc) |>
            stars::st_downsample(
              n = downsample_factor,
              # It sums the amount of 1 values inside the bigger
              # reference grid cells
              FUN = sum,
              na.rm = TRUE
            ) |>
            dplyr::mutate(
              # By applying the proportion_factor, we transform the data to a
              # proportion (0 to 1)
              urban = urban / proportion_factor,
              forest = forest / proportion_factor,
              agriculture = agriculture / proportion_factor,
              pasture = pasture / proportion_factor,
              mosaic = mosaic / proportion_factor
            )

          # Redimension the raster so that we have a time dimension
          lulc_stars <- lulc_stars |>
            stars::st_redimension(
              new_dims = stars::st_dimensions(
                x = stars::st_get_dimension_values(lulc_stars, "x"),
                y = stars::st_get_dimension_values(lulc_stars, "y"),
                time = lubridate::ymd(glue::glue("{y}-01-01"))
              )
            )

          gc(verbose = FALSE)

          return(lulc_stars)
        }
      )

    # Merge all mapbiomas processed data along the time dimension
    lulc_stars_cube <-
      purrr::reduce(
        .x = lulc_stars_list,
        .f = c,
        along = 3 # This is the time dimension
      ) |>
      sf::st_crop(
        # Turn to NA all values that falls outside the area of interest (AOI)
        y = aoi,
        crop = FALSE
      )

    # Write the results
    stars::write_mdim(
      x = lulc_stars_cube,
      filename = "./data/processed/lulc_mapbiomas.nc"
    )

    return(invisible(NULL))
  }

# -----------------------------------------------------------------------------
# This function downloads the legend of the mapobiomas data
download_mapbiomas_legenda <-
  function() {
    # Set working directory
    wdir <- here::here()

    base_dir <- glue::glue("{wdir}/data")

    # Create temporary directory
    file_dir <- tempdir()
    fs::dir_create(path = glue::glue("{tempdir()}/legenda"))
    file_dir <- glue::glue("{tempdir()}/legenda")

    # Crate path for downloaded data
    file_path <- glue::glue("{file_dir}/legenda.zip")

    # Create path to the downloaded data to be saved
    dest_path <- glue::glue("{base_dir}/legenda_mapbiomas.csv")

    data_url <-
      glue::glue(
        "https://brasil.mapbiomas.org/wp-content/uploads/sites/",
        "4/2024/08/Codigos-da-legenda-colecao-9.zip"
      )

    # Download data ----
    # Set download options
    h <- curl::new_handle()
    curl::handle_setopt(handle = h, ssl_verifypeer = FALSE)

    # Download data to temporary dir
    curl::curl_download(
      url = glue::glue(data_url),
      destfile = file_path,
      handle = h
    )

    # Extract compressed files
    if (stringr::str_detect(file_path, ".zip")) {
      utils::unzip(
        zipfile = file_path,
        exdir = glue::glue("{file_dir}/")
      )

      # Get path of the downloaded data
      file_path <-
        fs::dir_ls(
          path = glue::glue("{file_dir}/"),
          recurse = TRUE,
          regexp = "Codigos-da-legenda-colecao-9.csv"
        )
    }

    legend_data <-
      readr::read_delim(
        file = file_path,
        delim = "\t",
        show_col_types = FALSE
      ) |>
      janitor::clean_names()

    readr::write_csv(x = legend_data, file = dest_path)

    return(invisible(NULL))
  }

# -----------------------------------------------------------------------------
