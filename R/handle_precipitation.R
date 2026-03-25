# -----------------------------------------------------------------------------
# This function downloads precipitation data from CHIRPS
download_precipitation <-
  function(
    # Years of data to be downloaded (should be a vector of integers)
    years = 2001L:2020L
  ) {
    # Create directory to store the data
    fs::dir_create(path = "./data/raw/precipitation/")

    # Set months numbers
    months <-
      stringr::str_pad(
        string = lubridate::month(x = 1:12),
        width = 2,
        pad = "0"
      )

    # Set URLs list
    url_list <-
      glue::glue(
        "https://data.chc.ucsb.edu/products/",
        "CHIRPS/v3.0/monthly/global/tifs/chirps-v3.0.",
        "{sort(rep(years, 12))}.{rep(months, length(years))}.tif"
      )

    # Set paths to write downloaded data
    path_list <-
      glue::glue(
        "./data/raw/precipitation/chirps_precipitation_",
        "{sort(rep(years, 12))}_{rep(months, length(years))}.tif"
      )

    # Download and write precipitation data
    httr2::req_perform_sequential(
      reqs = purrr::map(.x = url_list, \(u) httr2::request(u)),
      paths = path_list
    )

    return(invisible(NULL))
  }

# -----------------------------------------------------------------------------
# This function process the precipitation data to match the bigger
# reference grid
process_precipitation <-
  function(
    # Years of data to be downloaded (should be a vector of integers)
    years = 2001L:2020L,
    # Months of data to be downloaded (should be a vector of integers)
    months = 1L:3L,
    # The name of a Geopackage file located in the data directory
    # (you must put the file there)
    area_of_interest = "sp"
  ) {
    # Load area of interest
    aoi <-
      sf::read_sf(dsn = glue::glue("./data/{area_of_interest}.gpkg")) |>
      sf::st_transform(
        crs = sf::st_crs(readr::read_file("./data/project_crs.txt"))
      )

    # Load the bigger reference grid
    reference_big <- stars::read_stars(dsn = "./data/reference_big.tif")

    # Sort all the dates of interest
    dates <- tidyr::crossing(years, months) |>
      glue::glue_data(
        "{years}_{stringr::str_pad(months, width = 2, pad = '0')}"
      )

    # Get list of files for processing
    files_list <-
      fs::dir_ls(path = "./data/raw/precipitation/") |>
      stringr::str_subset(pattern = stringr::str_c(dates, collapse = "|"))

    # Warp precipitation data to the bigger reference grid
    precipitation_stars <-
      stars::read_stars(.x = files_list, proxy = TRUE) |>
      sf::st_crop(y = sf::st_bbox(sf::st_transform(aoi, "EPSG:4326"))) |>
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
        values = lubridate::ymd(dates, truncated = 2),
        names = "time"
      ) |>
      rlang::set_names(nm = "precipitation") |>
      dplyr::mutate(precipitation = precipitation) |>
      sf::st_crop(y = aoi, crop = FALSE)

    stars::write_mdim(
      x = precipitation_stars,
      filename = "./data/processed/precipitation_chirps.nc"
    )

    return(invisible(NULL))
  }

# -----------------------------------------------------------------------------
