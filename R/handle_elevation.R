download_elevation <-
  function(
    area_of_interest = "vale_paraiba",
    mpc_key = "dc843c0012ab40bc8f13732ce2e82379"
  ) {

    # Load area of interest
    aoi <-
      sf::read_sf(
        dsn = glue::glue(
          "./data/{area_of_interest}.gpkg"
        )
      )

    # Get bounding box
    bbox <- aoi |>
      sf::st_transform("EPSG:4326") |>
      sf::st_bbox()

    # Connect to Microsoft Planetary Computer STAC API
    mpc_stac <-
      rstac::stac(
        base_url = "https://planetarycomputer.microsoft.com/api/stac/v1/"
      )

    asset_it <- mpc_stac |>
      rstac::stac_search(
        collections = "cop-dem-glo-30",
        bbox = c(
          bbox[["xmin"]], bbox[["ymin"]],
          bbox[["xmax"]], bbox[["ymax"]]
        ),
        datetime = "2021-04-22/2021-04-23",
        limit = 1000
      ) |>
      rstac::get_request() |>
      rstac::items_sign(
        sign_fn = rstac::sign_planetary_computer(
          headers = c(
            "Ocp-Apim-Subscription-Key" = mpc_key
          )
        )
      )

    asset_c <-
      gdalcubes::stac_image_collection(
        s = asset_it$features,
        asset_names = "data"
      )

    asset_view <-
      gdalcubes::cube_view(
        srs = sf::st_crs(bbox)$wkt,
        extent = list(
          t0 = "2021-04-22",
          t1 = "2021-04-23",
          left = bbox["xmin"], right = bbox["xmax"],
          top = bbox["ymax"], bottom = bbox["ymin"]
        ),
        dx = 30 * 0.00001, dy = 30 * 0.00001,
        dt = "P1M",
        aggregation = "mean",
        resampling = "bilinear"
      )

    gdalcubes::gdalcubes_options(parallel = TRUE)

    asset_cube <-
      gdalcubes::raster_cube(
        image_collection = asset_c,
        view = asset_view
      ) |>
      gdalcubes::select_bands("data")

    asset_cube <-
      gdalcubes::reduce_time(
        asset_cube,
        expr = "mean(data)"
      )

    asset_cube <- gdalcubes::write_tif(asset_cube)

    asset_stars <-
      stars::read_stars(asset_cube) |>
      stats::setNames("values")

    stars::write_stars(
      obj = asset_stars,
      dsn = "./data/raw/elevation/copernicus_dem.tif",
      options = c("COMPRESS=LZW", "BIGTIFF=YES")
    )

    return(invisible(NULL))

  }

process_elevation <-
  function(
    area_of_interest = "vale_paraiba"
  ) {

    # Load area of interest
    aoi <-
      sf::read_sf(
        dsn = glue::glue(
          "./data/{area_of_interest}.gpkg"
        )
      )

    reference_big <-
      stars::read_stars(
        "./data/reference_big.tif"
      )

    stars::read_stars("./data/raw/elevation/copernicus_dem.tif") |>
      stars::st_warp(
        dest = reference_big,
        use_gdal = TRUE,
        method = "med",
        no_data_value = -1
      ) |>
      setNames("elevation") |>
      sf::st_crop(aoi, crop = FALSE) |>
      stars::write_mdim(
        filename = "./data/processed/copernicus_dem.nc"
      )

    return(invisible(NULL))

  }