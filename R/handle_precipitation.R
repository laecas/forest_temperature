download_precipitation <-
  function(
    years = c(2001:2020)
  ) {

    months <- stringr::str_pad(lubridate::month(1:12), 2, pad = "0")

    url_list <-
      glue::glue(
        "https://data.chc.ucsb.edu/products/",
        "CHIRPS/v3.0/monthly/global/tifs/chirps-v3.0.",
        "{sort(rep(years, 12))}.{rep(months, length(years))}.tif"
      )

    path_list <-
      glue::glue(
        "./data/raw/precipitation/chirps_precipitation_",
        "{sort(rep(years, 12))}_{rep(months, length(years))}.tif"
      )

    httr2::req_perform_sequential(
      reqs = purrr::map(url_list, \(u) httr2::request(u)),
      paths = path_list
    )

    return(invisible(NULL))

  }

process_precipitation <-
  function(
    area_of_interest = "vale_paraiba"
  ) {

    # Load area of interest
    aoi <-
      sf::read_sf(
        dsn = glue::glue(
          "./data/{area_of_interest}.gpkg"
        )
      ) |>
      sf::st_transform(
        sf::st_crs(readr::read_file("./data/project_crs.txt"))
      )

    reference_big <-
      stars::read_stars(
        "./data/reference_big.tif"
      )

    years <-
      fs::dir_ls("./data/raw/precipitation/") |>
      stringr::str_sub(start = -11, end = -8)

    months <-
      fs::dir_ls("./data/raw/precipitation/") |>
      stringr::str_sub(start = -6, end = -5)

    precipitation_stars <-
      stars::read_stars(
        .x = fs::dir_ls("./data/raw/precipitation/"),
        proxy = TRUE
      ) |>
      sf::st_crop(
        aoi |> sf::st_transform("EPSG:4326") |> sf::st_bbox()
      ) |>
      stars::st_redimension() |>
      stars::st_as_stars() |>
      stars::st_warp(
        dest = reference_big,
        use_gdal = TRUE,
        method = "bilinear",
        no_data_value = -9999
      ) |>
      stars::st_set_dimensions(
        which = 3,
        values = lubridate::ymd(glue::glue("{years}-{months}-01")),
        names = "time"
      ) |>
      setNames("precipitation") |>
      dplyr::mutate(precipitation = precipitation) |>
      sf::st_crop(aoi, crop = FALSE)

    stars::write_mdim(
      x = precipitation_stars,
      filename = "./data/processed/precipitation_chirps.nc"
    )

    return(invisible(NULL))

  }