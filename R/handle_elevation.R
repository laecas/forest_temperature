# ------------------------------------------------------------------------------
# This function downloads elevation data from ANADEM
download_elevation <-
  function(
    area_of_interest = "sp" # The name of a Geopackage file located in the data directory (you must put the file there)
  ) {
    # Create directory to store the data
    fs::dir_create(path = "./data/raw/elevation/")

    # Load area of interest
    aoi <- sf::read_sf(dsn = glue::glue("./data/{area_of_interest}.gpkg"))

    # Create temporary dir to store the zip file
    temp_file <- fs::file_temp()

    # Download grid that organizes the ANADEM data
    curl::curl_download(
      url = "https://www.ufrgs.br/hge/wp-content/uploads/2024/04/anadem_mgrs.zip",
      destfile = glue::glue("{temp_file}.zip")
    )

    # Extract the files to a temporary folder
    archive::archive_extract(
      archive = glue::glue("{temp_file}.zip"),
      dir = glue::glue("{temp_file}/")
    )

    # Read the DEM data and transform to the same CRS as the area of interest
    mgrs_grid <-
      sf::read_sf(dsn = glue::glue("{temp_file}/anadem_mgrs/mgrs.shp")) |>
      sf::st_transform(crs = sf::st_crs(aoi))

    mgrs_id <- mgrs_grid |>
      sf::st_filter(y = aoi) |>
      tibble::as_tibble() |>
      dplyr::pull(.data[["mgrs"]])

    # Set URLs list
    url_list <-
      glue::glue(
        "https://metadados.snirh.gov.br/files/anadem_v1_tiles/",
        "anadem_v1_{mgrs_id}.tif"
      )

    # Set paths to write downloaded data
    path_list <-
      glue::glue("./data/raw/elevation/anadem_elevation_{mgrs_id}.tif")

    # Download and write precipitation data
    httr2::req_perform_sequential(
      reqs = purrr::map(url_list, \(u) httr2::request(u)),
      paths = path_list
    )

    return(invisible(NULL))
  }

# ------------------------------------------------------------------------------
# This function warps the elevation data to match the bigger reference grid
process_elevation <-
  function(
    area_of_interest = "sp"
  ) {
    # Load area of interest
    aoi <-
      sf::read_sf(dsn = glue::glue("./data/{area_of_interest}.gpkg")) |>
      sf::st_transform(
        crs = sf::st_crs(readr::read_file("./data/project_crs.txt"))
      )

    reference_big <- stars::read_stars(dsn = "./data/reference_big.tif")

    # Transform DEM data to the bigger reference grid spatial characteristics
    # Mosaic the data into one
    purrr::map(
      .x = fs::dir_ls(path = "./data/raw/elevation/", glob = "*.tif"),
      \(dem) {
        # Spatial transform the data and crop to the area of interest
        dem |>
          stars::read_stars() |>
          stars::st_warp(
            dest = reference_big,
            use_gdal = TRUE,
            method = "med",
            no_data_value = 99999
          ) |>
          rlang::set_names(nm = "elevation") |>
          sf::st_crop(y = aoi, crop = FALSE)
      }
    ) |>
      purrr::reduce(.f = stars::st_mosaic) |> # Create mosaic
      rlang::set_names(nm = "elevation") |>
      sf::st_crop(y = aoi, crop = FALSE) |>
      stars::write_mdim(
        # Write data to disk
        filename = "./data/processed/elevation_anadem.nc"
      )

    return(invisible(NULL))
  }

# ------------------------------------------------------------------------------
