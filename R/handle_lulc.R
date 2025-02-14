download_mapbiomas <-
  function(
    year = c(2001:2020),
    area_of_interest = "vale_paraiba"
  ) {

    # Load area of interest
    aoi <-
      sf::read_sf(
        dsn = glue::glue(
          "./data/{area_of_interest}.gpkg"
        )
      )

    # Get bounding box of interest
    bbox <- aoi |>
      sf::st_transform("EPSG:4326") |>
      sf::st_bbox()

    purrr::walk(
      .x = year,
      \(y) {

        # Create temporary directory
        file_dir <- tempdir()
        fs::dir_create(glue::glue("{file_dir}/raster"))
        file_dir <- glue::glue("{file_dir}/raster")

        # Set path for downloaded data
        file_path <-
          glue::glue(
            "{file_dir}/mapbiomas_{y}.tif"
          )

        img_url <-
          glue::glue(
            "https://storage.googleapis.com/mapbiomas-public/initiatives/",
            "brasil/collection_9/lclu/coverage/brasil_coverage_{y}.tif"
          )

        # Download data ----
        # Create curl handle
        h <- curl::new_handle()

        curl::handle_setopt(
          h,
          ssl_verifypeer = FALSE
        )

        # Download data to temporary directory
        curl::curl_download(
          url = img_url,
          destfile = file_path
        )

        # Load image
        img <- stars::read_stars(file_path)

        # Transform area of interest bbox to mapbiomas CRS
        stars_bbox <- bbox |>
          sf::st_as_sfc() |>
          sf::st_transform(sf::st_crs(img)) |>
          sf::st_bbox()

        # Crop mapbiomas raster to area of interest
        img <- img |>
          sf::st_crop(stars_bbox)

        # Save mapbiomas raster to final destination
        stars::write_stars(
          obj = img,
          dsn = glue::glue(
            "./data/raw/lulc/mapbiomas_{y}.tif"
          ),
          options = c("COMPRESS=LZW"),
          progress = FALSE
        )

        unlink(file_dir, recursive = TRUE)

      }
    )

    return(invisible(NULL))

  }

process_mapbiomas <-
  function(
    area_of_interest = "vale_paraiba",
    years = c(2001:2020),
    scale_factor = 1
  ) {

    # Load area of interest
    aoi <-
      sf::read_sf(
        dsn = glue::glue(
          "./data/{area_of_interest}.gpkg"
        )
      )

    # Crop mapbiomas proxy raster to campinas bounding box
    raster_meta <-
      stars::read_stars(fs::dir_ls("./data/raw/lulc/")[1], proxy = TRUE)

    # Get the dimensions of the cropped mapbiomas raster
    raster_dim <- stars::st_dimensions(raster_meta)

    # make a list with rasterio options for reading raster
    rasterio <-
      list(
        nXOff = raster_dim$x$from,
        nYOff = raster_dim$y$from,
        nXSize = raster_dim$x$to - raster_dim$x$from,
        nYSize = raster_dim$y$to - raster_dim$y$from,
        nBufXSize = (raster_dim$x$to - raster_dim$x$from) / scale_factor,
        nBufYSize = (raster_dim$y$to - raster_dim$y$from) / scale_factor,
        resample = "mode"
      )

    reference_small <-
      stars::read_stars(
        "./data/reference_small.tif"
      )

    reference_big <-
      stars::read_stars(
        "./data/reference_big.tif"
      )

    downsample_factor <-
      stars::st_res(reference_big)[[1]] /
      stars::st_res(reference_small)[[1]] - 1

    proportion_factor <-
      (
        stars::st_res(reference_big)[[1]] /
        stars::st_res(reference_small)[[1]]
      ) ^ 2

    lulc_stars_list <-
      purrr::map(
        .x = years,
        .f = \(y) {

          # Read subset of mapbiomas raster using rasterio
          lulc_stars <-
            stars::read_stars(
              .x = glue::glue("./data/raw/lulc/mapbiomas_{y}.tif"),
              RasterIO = rasterio,
              NA_value = 0,
              proxy = FALSE
            ) |>
            setNames("lulc") |>
            stars::st_warp(dest = reference_small)

          lulc_stars_agg <-
            lulc_stars |>
            dplyr::mutate(
              urban = dplyr::if_else(lulc == 24, 1, 0),
              forest = dplyr::if_else(lulc == 3, 1, 0),
              agriculture = dplyr::if_else(
                lulc %in% c(39, 20, 40, 62, 41), 1, 0
              )
            ) |>
            dplyr::select(!lulc) |>
            stars::st_downsample(
              downsample_factor,
              FUN = sum,
              na.rm = TRUE
            ) |>
            dplyr::mutate(
              urban = urban / proportion_factor,
              forest = forest / proportion_factor,
              agriculture = agriculture / proportion_factor
            )

          lulc_stars_agg <-
            lulc_stars_agg |>
            stars::st_redimension(
              new_dims = stars::st_dimensions(
                x = stars::st_get_dimension_values(lulc_stars_agg, "x"),
                y = stars::st_get_dimension_values(lulc_stars_agg, "y"),
                time = lubridate::ymd(glue::glue("{y}-01-01"))
              )
            )

        }
      )

    lulc_stars_cube <-
      purrr::reduce(
        .x = lulc_stars_list,
        .f = c,
        along = 3
      ) |>
      sf::st_crop(aoi, crop = FALSE)

    stars::write_mdim(
      x = lulc_stars_cube,
      filename = "./data/processed/lulc_mapbiomas.nc"
    )

    return(invisible(NULL))

  }

download_mapbiomas_legenda <-
  function() {

    # Set working directory
    wdir <- here::here()

    # Set municipality option to title case
    muni <- stringr::str_to_title(muni)

    base_dir <-
      glue::glue(
        "{wdir}/dados"
      )

    # Create temporary directory
    file_dir <- tempdir()
    fs::dir_create(glue::glue("{tempdir()}/legenda"))
    file_dir <- glue::glue("{tempdir()}/legenda")

    # Crate path for downloaded data
    file_path <- glue::glue("{file_dir}/legenda.zip")

    # Create path to the downloaded data to be saved
    dest_path <- glue::glue("{base_dir}/legenda_mapbiomas.csv")

    data_url <-
      glue::glue(
        "https://brasil.mapbiomas.org/wp-content/uploads/sites/",
        "4/2023/09/Codigos-da-legenda-colecao-9-csv.zip"
      )

    # Download data ----
    # Create curl handle
    h <- curl::new_handle()

    curl::handle_setopt(
      h,
      ssl_verifypeer = FALSE
    )

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
      readr::read_delim(file_path, delim = ";", show_col_types = FALSE) |>
      janitor::clean_names()

    readr::write_csv(legend_data, dest_path)

    return(invisible(NULL))

  }