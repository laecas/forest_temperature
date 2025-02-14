download_temperature_gshtd <-
  function(
    area_of_interest = "vale_paraiba",
    var = "mean",
    years = c(2001:2020)
  ) {

    library(rgee)

    # Inicializar conexão com GEE
    ee_Initialize()

    # Load area of interest
    aoi <-
      sf::read_sf(
        dsn = glue::glue(
          "./data/{area_of_interest}.gpkg"
        )
      ) |>
      sf::st_transform("EPSG:4326") |>
      sf::st_bbox() |>
      sf::st_as_sfc() |>
      sf::st_geometry() |>
      sf_as_ee()

    asset <-
      paste0(
        "projects/sat-io/open-datasets/GSHTD/T",
        stringr::str_to_upper(var)
      )

    # Obter as datas do dado de temperatura
    gshtd_dates <-
      ee_get_date_ic(
        ee$ImageCollection(asset)
      ) |>
      dplyr::filter(lubridate::year(.data$time_start) %in% years) |>
      dplyr::pull(.data$time_start)

    # Baixar imagens para todas as datas
    purrr::walk(
      .x = gshtd_dates,
      .f = \(i) {

        date_filter <- as.character(i)

        date <-
          paste0(
            lubridate::year(i),
            "_",
            stringr::str_pad(lubridate::month(i), 2, pad = "0")
          )

        # Acessar imagens no GEE
        img <-
          ee$ImageCollection(asset)$
          filterDate(date_filter)$
          first()

        # Abrir imagem localmente como objeto stars
        stars <-
          ee_as_stars(
            image = img,
            region = aoi,
            via = "drive"
          ) |>
          setNames(date_filter) # Colocar nome da imagem como sua data

        stars::write_stars(
          stars::st_as_stars(stars),
          glue::glue(
            "./data/raw/temperature_gshtd/gshtd_{var}_temperature_{date}.tif"
          ),
          options = c("COMPRESS=LZW"),
          progress = FALSE
        )

      }
    )

    return(invisible(NULL))

  }

process_temperature_gshtd <-
  function(
    area_of_interest = "vale_paraiba",
    temperature_metric = "mean",
    years = 2001:2020,
    months = 1:12
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

    years <-
      fs::dir_ls(
        "./data/raw/temperature_gshtd/",
        regexp = temperature_metric
      ) |>
      stringr::str_sub(start = -11, end = -8)

    months <-
      fs::dir_ls(
        "./data/raw/temperature_gshtd/",
        regexp = temperature_metric
      ) |>
      stringr::str_sub(start = -6, end = -5)

    temperature_stars <-
      stars::read_stars(
        .x = fs::dir_ls(
          "./data/raw/temperature_gshtd/",
          regexp = temperature_metric
        ),
        NA_value = 0
      ) |>
      stars::st_redimension(name = "time") |>
      stars::st_warp(
        dest = reference_big,
        use_gdal = TRUE,
        method = "bilinear",
        no_data_value = 0
      ) |>
      setNames("temperature") |>
      stars::st_set_dimensions(
        which = "band",
        values = lubridate::ymd(
          glue::glue("{years}-{stringr::str_pad(months, 2, pad = '0')}"),
          truncated = 2
        ),
        names = "time"
      ) |>
      sf::st_crop(aoi, crop = FALSE) |>
      dplyr::mutate(temperature = .data$temperature * 0.02 - 273.15)

    stars::write_mdim(
      x = temperature_stars,
      filename = glue::glue(
        "./data/processed/temperature_gshtd_{temperature_metric}.nc"
      )
    )

    return(invisible(NULL))

  }

download_temperature_ncdc <-
  function(
    years = c(2001:2020)
  ) {

    station_ids <-
      readr::read_csv(
        file = "./data/stations_daily.csv",
        show_col_types = FALSE
      ) |>
      janitor::clean_names() |>
      dplyr::pull(.data[["station_id"]])

    purrr::walk(
      .x = years,
      .f = \(y) {

        ncdc_resp <-
          httr2::request(
            glue::glue(
              "https://www.ncei.noaa.gov/data/",
              "global-summary-of-the-day/access/{y}/"
            )
          ) |>
          httr2::req_perform()

        ncdc_html <- ncdc_resp |> httr2::resp_body_html()

        files_list <-
          rvest::html_nodes(ncdc_html, "td") |>
          rvest::html_nodes("a") |>
          rvest::html_attr("href") |>
          stringr::str_subset("csv")

        files_list_sub <-
          purrr::keep(
            .x = files_list,
            .p = stringr::str_detect(
              files_list, paste(station_ids, collapse = "|")
            )
          )

        purrr::walk(
          .x = files_list_sub,
          .f = \(f) {

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

process_temperature_ncdc <-
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

    ncdc_stations <-
      purrr::map(
        .x = fs::dir_ls("./data/raw/temperature_ncdc/"),
        .f = \(f) {
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
      sf::st_as_sf(
        coords = c("longitude", "latitude")
      ) |>
      sf::st_set_crs("EPSG:4326") |>
      sf::st_transform(sf::st_crs(aoi)) |>
      sf::st_filter(aoi) |>
      dplyr::mutate(
        dplyr::across(.cols = c("temp", "max", "min"), ~ (.x - 32) / (9 / 5))
      )

    sf::write_sf(
      obj = ncdc_stations,
      dsn = "./data/processed/temperature_ncdc.gpkg"
    )

    return(invisible(NULL))

  }