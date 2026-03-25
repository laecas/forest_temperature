# -----------------------------------------------------------------------------
# This function creates a land cover scenario where the forest cover
# complies with the environmental legislation of the Brazilian Forest Code
create_scenario <-
  function(
    scenario_year = 2020L,
    scale_factor = 3L
  ) {
    # Gather information to open process lulc data
    reference_small <-
      stars::read_stars(.x = "./data/reference_small.tif") |>
      rlang::set_names(nm = "id") |>
      dplyr::mutate(id = NA)

    # Read reference grid
    reference_big <- stars::read_stars(.x = "./data/reference_big.tif")

    # This creates a new factor to transform the loaded land use data into
    # the same spatial resolution of the bigger reference grid
    downsample_factor <-
      stars::st_res(reference_big)[[1]] /
      stars::st_res(reference_small)[[1]] -
      1 # Important to subtract by 1 (read the `stars::st_downsample` docs)

    # This creates a new factor to calculate the proportion of each land use
    # inside the downsampled cells
    proportion_factor <-
      (stars::st_res(reference_big)[[1]] /
        stars::st_res(reference_small)[[1]])^2

    # Crop mapbiomas proxy raster to aoi bounding box
    # Get mapbiomas file proxy
    raster_meta <-
      stars::read_stars(fs::dir_ls("./data/raw/lulc/")[1], proxy = TRUE)

    # Get the dimensions of the cropped mapbiomas raster
    raster_dim <- stars::st_dimensions(raster_meta)

    # Make a list with rasterio options for reading raster
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

    # Transform protected areas (APP) data to raster
    app <-
      fs::dir_ls("./data/raw/permanent_protection_area/", glob = "*.shp") |>
      purrr::map(
        \(app_file) {
          # Remove polygons of APPs that were cancelled
          # Transform data to match the CRS of the reference grid
          # Rasterize the data
          app_raster <- sf::read_sf(app_file) |>
            dplyr::filter(
              stringr::str_detect(
                .data[["des_condic"]],
                "Cancelado",
                negate = TRUE
              )
            ) |>
            dplyr::mutate(value = 1) |>
            dplyr::select(value) |>
            sf::st_transform(sf::st_crs(reference_small)) |>
            stars::st_rasterize(template = reference_small, proxy = FALSE)

          app_raster[is.na(app_raster[])] <- 0 # Turn NAs to zero

          return(app_raster)
        }
      ) |>
      purrr::reduce(`+`) |> # Sum the app rasters into one
      dplyr::mutate(value = dplyr::if_else(value >= 1, 1, value))

    # Transform legal reserves (RL) data to raster
    # Remove polygons of RLs that were cancelled
    # Transform data to match the CRS of the reference grid
    # Rasterize the data
    rl <-
      sf::read_sf("./data/raw/legal_reserve/RESERVA_LEGAL_1.shp") |>
      dplyr::filter(
        stringr::str_detect(
          .data[["des_condic"]],
          "Cancelado",
          negate = TRUE
        )
      ) |>
      dplyr::mutate(value = 1) |>
      dplyr::select(value) |>
      sf::st_transform(sf::st_crs(reference_small)) |>
      stars::st_rasterize(template = reference_small, proxy = FALSE)

    rl[is.na(rl[])] <- 0 # Turn NAs to zero

    # Load mapbiomas lulc data
    mb <-
      stars::read_stars(
        .x = glue::glue("./data/raw/lulc/mapbiomas_{scenario_year}.tif"),
        RasterIO = rasterio,
        NA_value = 0,
        proxy = TRUE
      ) |>
      stars::st_warp(
        dest = reference_small,
        use_gdal = TRUE,
        method = "near",
        no_data_value = 99999
      ) |>
      stars::st_as_stars() |>
      rlang::set_names("lulc")

    # Create binary raster of forest cover from lulc data
    forest <- mb |>
      dplyr::mutate(lulc = ifelse(lulc == 3, 1, 0))

    # Sum APP, RL and Mapbiomas forest data
    # This represents the forest cover of our scenario
    scenario <- purrr::reduce(list(app, rl, forest), `+`)

    # Reclassify land cover of interest from Mapbiomas
    scenario[scenario >= 1] <- 1000
    mb[["lulc"]] <- ifelse(mb[["lulc"]] == 3, 300, mb[["lulc"]]) # Forest
    mb[["lulc"]] <- ifelse(mb[["lulc"]] == 24, 100, mb[["lulc"]]) # Urban
    mb[["lulc"]] <- # Agriculture
      ifelse(
        mb[["lulc"]] %in% c(39, 20, 40, 62, 41),
        200,
        mb[["lulc"]]
      )
    mb[["lulc"]] <- ifelse(mb[["lulc"]] == 15, 400, mb[["lulc"]]) # Pasture
    mb[["lulc"]] <- ifelse(mb[["lulc"]] == 21, 500, mb[["lulc"]]) # Mosaic
    mb[["lulc"]] <- # Turn other land uses to zero
      ifelse(
        mb[["lulc"]] %in% c(100, 200, 300, 400, 500),
        mb[["lulc"]],
        0
      )

    # Merge data to create scenario
    mb[["lulc"]] <- mb[["lulc"]] + scenario[["value"]]
    mb[["lulc"]] <- ifelse(mb[["lulc"]] >= 1000, 300, mb[["lulc"]])

    # create land use layers of scenario
    mb[["urban"]] <- ifelse(mb[["lulc"]] == 100, 1, 0)
    mb[["forest"]] <- ifelse(mb[["lulc"]] == 300, 1, 0)
    mb[["agriculture"]] <- ifelse(mb[["lulc"]] == 200, 1, 0)
    mb[["pasture"]] <- ifelse(mb[["lulc"]] == 400, 1, 0)
    mb[["mosaic"]] <- ifelse(mb[["lulc"]] == 500, 1, 0)

    # Downsample scenario data to match the bigger reference grid
    mb_scenario <- mb |>
      dplyr::select(!lulc) |>
      stars::st_downsample(
        downsample_factor,
        FUN = sum,
        na.rm = TRUE
      ) |>
      dplyr::mutate(
        urban = urban / proportion_factor,
        forest = forest / proportion_factor,
        agriculture = agriculture / proportion_factor,
        pasture = pasture / proportion_factor,
        mosaic = mosaic / proportion_factor
      )

    # Set land uses as dimension and crop scenario data to the area of interest
    mb_scenario <- mb_scenario |>
      stars::st_redimension() |>
      rlang::set_names(nm = "lulc") |>
      sf::st_crop(
        y = sf::st_transform(
          x = sf::read_sf("./data/sp.gpkg"),
          crs = sf::st_crs(scenario)
        )
      )

    # Write scenario data
    stars::write_stars(
      obj = mb_scenario,
      dsn = glue::glue("./data/processed/lulc_scenario_{scenario_year}.tif")
    )
  }

# -----------------------------------------------------------------------------
