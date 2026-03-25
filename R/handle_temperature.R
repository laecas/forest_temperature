# -----------------------------------------------------------------------------
# This function downloads temperature data from the Global Seamless
# High-resolution Temperature Dataset (GSHTD).
# For this it is necessary to use the Google Earth Engine API
# and its R wrapper (rgee).
# Check the `rgee` documentation for help on how to set it functioning.
# https://r-spatial.github.io/rgee/
download_temperature_gshtd <-
  function(
    # Temperature variable to download (can be "mean" or "max")
    var = "mean",
    # Years of data to be downloaded (should be a vector of integers)
    years = 2001L:2020L,
    # The name of a Geopackage file located in the data directory
    # (you must put the file there)
    area_of_interest = "sp"
  ) {
    library(rgee)
    library(geojsonio)
    # Initialize GEE session
    rgee::ee_Initialize(drive = TRUE)

    # Load area of interest and load it into the google earth engine
    aoi <-
      sf::read_sf(dsn = glue::glue("./data/{area_of_interest}.gpkg")) |>
      sf::st_transform(crs = "EPSG:4326") |>
      sf::st_bbox() |>
      sf::st_as_sfc() |>
      sf::st_geometry() |>
      rgee::sf_as_ee()

    # Get the asset name of the GSHTD dataset
    asset <-
      paste0(
        "projects/sat-io/open-datasets/GSHTD/T",
        stringr::str_to_upper(string = var)
      )

    # Get all the dates of the temperature data
    gshtd_dates <-
      rgee::ee_get_date_ic(x = ee$ImageCollection(asset)) |>
      dplyr::filter(lubridate::year(x = .data$time_start) %in% years) |>
      dplyr::pull(.data$time_start)

    # Download temperature data for all the dates
    purrr::walk(
      .x = gshtd_dates[48:239],
      \(i) {
        date_filter <- as.character(i)

        date <-
          paste0(
            lubridate::year(x = i),
            "_",
            stringr::str_pad(
              string = lubridate::month(x = i),
              width = 2,
              pad = "0"
            )
          )

        # Load temperature data on GEE
        img <- try(
          silent = TRUE,
          rgee::ee$ImageCollection(asset)$filterDate(
            date_filter
          )$first()$multiply(0.02)$subtract(273.15)$clip(aoi)
        )
        img <- try(
          silent = TRUE,
          rgee::ee$ImageCollection(asset)$filterDate(
            date_filter
          )$first()$multiply(0.02)$subtract(273.15)$clip(aoi)
        )

        rightnow <- format(Sys.time(), "%Y-%m-%d_%X")
        googledrive_file <-
          glue::glue(
            "gshtd_{var}_temperature_{date}_{rightnow}"
          )

        # Download data to google drive
        proc <- rgee::ee_image_to_drive(
          image = img,
          fileFormat = "GEO_TIFF",
          region = aoi,
          fileNamePrefix = googledrive_file,
          timePrefix = FALSE,
          folder = "rgee_gshtd"
        )
        proc$start()
        rgee::ee_monitoring(
          proc,
          task_time = 25,
          max_attempts = 100,
          quiet = TRUE
        )

        # Write temperature data locally
        googledrive::drive_download(
          file = glue::glue(
            "rgee_gshtd/{googledrive_file}.tif"
          ),
          path = glue::glue(
            "./data/raw/temperature_gshtd/gshtd_{var}_temperature_{date}.tif"
          ),
          overwrite = TRUE
        )

        try(rgee::ee_manage_cancel_all_running_task(), silent = TRUE)
      }
    )

    return(invisible(NULL))
  }

# -----------------------------------------------------------------------------
# This function process the GSHTD temperature data to match the bigger
# reference grid
process_temperature_gshtd <-
  function(
    # Temperature variable to download (can be "mean" or "max")
    temperature_metric = "mean",
    # Years of data to be processed (should be a vector of integers)
    years = 2001L:2020L,
    # Months of data to be processed (should be a vector of integers)
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

    # Load bigger reference grid
    reference_big <- stars::read_stars(.x = "./data/reference_big.tif")

    # Set the dates for the data processing
    dates <- tidyr::crossing(years, months) |>
      glue::glue_data(
        "{years}_{stringr::str_pad(months, width = 2, pad = '0')}"
      )

    # List all files to processed
    files_list <-
      fs::dir_ls(
        path = "./data/raw/temperature_gshtd/",
        regexp = temperature_metric
      ) |>
      stringr::str_subset(pattern = stringr::str_c(dates, collapse = "|"))

    # Load temperature data and warp them to the bigger reference grid
    # Turn to NA all values that falls outside the area of interest (AOI)
    # Transform the data to degree Celsius
    temperature_stars <-
      stars::read_stars(.x = files_list, NA_value = 0) |>
      stars::st_redimension(name = "time") |>
      stars::st_warp(
        dest = reference_big,
        use_gdal = TRUE,
        method = "bilinear",
        no_data_value = 0
      ) |>
      rlang::set_names(nm = "temperature") |>
      stars::st_set_dimensions(
        which = "band",
        values = lubridate::ymd(
          dates,
          truncated = 2
        ),
        names = "time"
      ) |>
      sf::st_crop(y = aoi, crop = FALSE)

    # Write processed data
    stars::write_mdim(
      x = temperature_stars,
      filename = glue::glue(
        "./data/processed/temperature_gshtd_{temperature_metric}.nc"
      )
    )

    return(invisible(NULL))
  }

# -----------------------------------------------------------------------------
# Function to download weather station data from the National Centers
# for Environmental Information (NCEI)
download_temperature_ncdc <-
  function(
    # Years of data to be downloaded (should be a vector of integers)
    years = 2001L:2020L
  ) {
    # Create directory to store the data
    fs::dir_create("./data/raw/temperature_ncdc/")

    # Load table with all stations from Brazil
    station_ids <-
      readr::read_csv(
        file = "./data/stations_daily.csv",
        show_col_types = FALSE
      ) |>
      janitor::clean_names() |>
      dplyr::pull(.data[["station_id"]])

    # Download data for each year
    purrr::walk(
      .x = years,
      \(y) {
        # Perform request for NCEI database
        ncdc_resp <-
          httr2::request(
            glue::glue(
              "https://www.ncei.noaa.gov/data/",
              "global-summary-of-the-day/access/{y}/"
            )
          ) |>
          httr2::req_perform()

        ncdc_html <- ncdc_resp |> httr2::resp_body_html()
        # Get files list for all stations
        files_list <-
          rvest::html_nodes(ncdc_html, "td") |>
          rvest::html_nodes("a") |>
          rvest::html_attr("href") |>
          stringr::str_subset("csv")

        # Filter brazilian stations
        files_list_sub <-
          purrr::keep(
            .x = files_list,
            .p = stringr::str_detect(
              files_list,
              paste(station_ids, collapse = "|")
            )
          )

        # Perform download for each brazilian station
        purrr::walk(
          .x = files_list_sub,
          \(f) {
            file_req <-
              httr2::request(
                glue::glue(
                  "https://www.ncei.noaa.gov/data/",
                  "global-summary-of-the-day/access/{y}/{f}"
                )
              ) |>
              httr2::req_retry(max_tries = 100) |>
              httr2::req_throttle(rate = 30 / 60)

            httr2::req_perform(
              file_req,
              path = glue::glue(
                "./data/raw/temperature_ncdc/ncdc_daily_temperature_{y}_{f}"
              ),
              verbosity = 0
            )
          }
        )
      }
    )

    return(invisible(NULL))
  }
# -----------------------------------------------------------------------------
# This function process meteorological data from NCEI
process_temperature_ncdc <-
  function(
    # Months of data to be processed (should be a vector of integers)
    months = 1L:3L,
    # The name of a Geopackage file located in the data directory
    # (you must put the file there)
    area_of_interest = "sp"
  ) {
    # Load area of interest
    aoi <-
      sf::read_sf(dsn = glue::glue("./data/{area_of_interest}.gpkg")) |>
      sf::st_transform(sf::st_crs(readr::read_file("./data/project_crs.txt")))

    # Merge and filter data
    ncdc_stations <-
      purrr::map(
        .x = fs::dir_ls("./data/raw/temperature_ncdc/"),
        \(f) {
          readr::read_csv(
            f,
            progress = FALSE,
            col_types = "dDdddcdididididididddcdcdcdd"
          )
        }
      ) |>
      purrr::list_rbind() |>
      janitor::clean_names() |>
      dplyr::select(station:elevation, temp, max, min, prcp) |>
      sf::st_as_sf(coords = c("longitude", "latitude")) |>
      sf::st_set_crs("EPSG:4326") |>
      sf::st_transform(sf::st_crs(aoi)) |>
      sf::st_filter(aoi) |>
      dplyr::mutate(
        # Transform temperature to degree Celcius
        dplyr::across(.cols = c("temp", "max", "min"), ~ (.x - 32) / (9 / 5))
      )

    ncdc_points <- ncdc_stations |>
      tibble::as_tibble() |>
      dplyr::select(station, geometry) |>
      dplyr::distinct(station, .keep_all = TRUE) |>
      sf::st_as_sf()

    ncdc_table <- ncdc_stations |>
      sf::st_drop_geometry() |>
      dplyr::rename(
        temperature_mean_ncdc = temp,
        temperature_max_ncdc = max,
        precipitation_ncdc = prcp,
        elevation_ncdc = elevation,
        time = date
      ) |>
      dplyr::mutate(
        precipitation_ncdc = dplyr::if_else(
          precipitation_ncdc == 99.99,
          NA_real_,
          precipitation_ncdc * 25.4 # Convert from inches to mm
        )
      ) |>
      dplyr::group_by(
        station,
        year = lubridate::year(time),
        month = lubridate::month(time)
      ) |>
      dplyr::summarise(
        temperature_mean_ncdc = mean(temperature_mean_ncdc, na.rm = TRUE),
        temperature_max_ncdc = max(temperature_max_ncdc, na.rm = TRUE),
        precipitation_ncdc = sum(precipitation_ncdc, na.rm = TRUE),
        elevation_ncdc = mean(elevation_ncdc, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        time = lubridate::ymd(glue::glue("{year}-{month}"), truncated = 1)
      ) |>
      dplyr::select(!dplyr::any_of(c("year", "month"))) |>
      dplyr::filter(
        temperature_max_ncdc < 60,
        temperature_mean_ncdc < 60,
        lubridate::month(time) %in% months
      ) |>
      dplyr::mutate(
        precipitation_ncdc = dplyr::na_if(precipitation_ncdc, 0)
      ) |>
      dplyr::left_join(ncdc_points, by = dplyr::join_by("station")) |>
      sf::st_as_sf()

    sf::write_sf(
      obj = ncdc_table,
      dsn = "./data/processed/temperature_ncdc.gpkg"
    )

    return(invisible(NULL))
  }

# -----------------------------------------------------------------------------
