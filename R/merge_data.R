analysis_merge_data <-
  function(
    months = 1:3
  ) {

    gshtd_temperature_mean <-
      stars::read_mdim("./data/processed/temperature_gshtd_mean.nc") |>
      dplyr::filter(lubridate::month(time) %in% months) |>
      aggregate(
        by = "1 year",
        FUN = mean,
        na.rm = TRUE
      )  |>
      tibble::as_tibble() |>
      tidyr::drop_na() |>
      dplyr::rename(temperature_mean = temperature)

    gshtd_temperature_max <-
      stars::read_mdim("./data/processed/temperature_gshtd_max.nc") |>
      dplyr::filter(lubridate::month(time) %in% months) |>
      aggregate(
        by = "1 year",
        FUN = mean,
        na.rm = TRUE
      )  |>
      tibble::as_tibble() |>
      tidyr::drop_na() |>
      dplyr::rename(temperature_max = temperature)

    copernicus_elevation <-
      stars::read_mdim("./data/processed/copernicus_dem.nc") |>
      tibble::as_tibble() |>
      tidyr::drop_na()

    mapbiomas_lulc <-
      stars::read_mdim("./data/processed/lulc_mapbiomas.nc") |>
      tibble::as_tibble() |>
      tidyr::drop_na()

    chirps_precipitation <-
      stars::read_mdim("./data/processed/precipitation_chirps.nc") |>
      dplyr::filter(lubridate::month(time) %in% months) |>
      aggregate(
        by = "1 year",
        FUN = sum,
        na.rm = TRUE
      ) |>
      tibble::as_tibble() |>
      tidyr::drop_na()

    merged_table <-
      dplyr::full_join(
        gshtd_temperature_mean,
        gshtd_temperature_max,
        by = dplyr::join_by(x, y, time)
      ) |>
      dplyr::full_join(
        copernicus_elevation,
        by = dplyr::join_by(x, y)
      ) |>
      dplyr::full_join(
        mapbiomas_lulc,
        by = dplyr::join_by(x, y, time)
      ) |>
      dplyr::full_join(
        chirps_precipitation,
        by = dplyr::join_by(x, y, time)
      ) |>
      tidyr::drop_na()

    readr::write_csv(
      merged_table,
      "./data/processed/merged_table.csv"
    )

  }

analysis_merge_tif <-
  function(
    area_of_interest = "vale_paraiba",
    years = 2001:2020,
    months = 1:3
  ) {

    gshtd_temperature_mean <-
      stars::read_mdim("./data/processed/temperature_gshtd_mean.nc") |>
      dplyr::filter(lubridate::month(time) %in% months) |>
      aggregate(
        by = "1 year",
        FUN = mean,
        na.rm = TRUE
      ) |>
      aperm(c(2, 3, 1))

    gshtd_temperature_max <-
      stars::read_mdim("./data/processed/temperature_gshtd_max.nc") |>
      dplyr::filter(lubridate::month(time) %in% months) |>
      aggregate(
        by = "1 year",
        FUN = mean,
        na.rm = TRUE
      ) |>
      aperm(c(2, 3, 1))

    copernicus_elevation <-
      stars::read_mdim("./data/processed/copernicus_dem.nc")

    mapbiomas_lulc <-
      stars::read_mdim("./data/processed/lulc_mapbiomas.nc")

    chirps_precipitation <-
      stars::read_mdim("./data/processed/precipitation_chirps.nc") |>
      dplyr::filter(lubridate::month(time) %in% months) |>
      aggregate(
        by = "1 year",
        FUN = sum,
        na.rm = TRUE
      ) |>
      aperm(c(2, 3, 1))

    purrr::walk(
      .x = years,
      .f = \(a) {

        merged_stars <-
          c(
            gshtd_temperature_mean |>
              dplyr::filter(lubridate::year(time) == a) |>
              split() |>
              setNames("temperature_mean"),
            gshtd_temperature_max |>
              dplyr::filter(lubridate::year(time) == a) |>
              split() |>
              setNames("temperature_max"),
            mapbiomas_lulc |>
              dplyr::filter(lubridate::year(time) == a) |>
              split(),
            copernicus_elevation,
            chirps_precipitation |>
              dplyr::filter(lubridate::year(time) == a) |>
              split() |>
              setNames("precipitation")
          ) |>
          stars::st_redimension(names = "bands") |>
          setNames("values")

        stars::write_stars(
          merged_stars,
          glue::glue(
            "./data/processed/merged_tifs/merged_tif_{a}.tif"
          ),
          options = c("COMPRESS=LZW"),
          progress = FALSE
        )

      }
    )

  }


validation_merge_data <-
  function() {

    ncdc_stations <-
      sf::read_sf("./data/processed/temperature_ncdc.gpkg")

    ncdc_points <-
      ncdc_stations |>
      tibble::as_tibble() |>
      dplyr::select(station, geom) |>
      dplyr::distinct(station, .keep_all = TRUE) |>
      sf::st_as_sf()

    ncdc_temp_prcp <-
      ncdc_stations |>
      sf::st_drop_geometry() |>
      dplyr::rename(
        temperature_mean_sncdc = temp,
        temperature_max_sncdc = max,
        precipitation_sncdc = prcp,
      ) |>
      dplyr::select(!dplyr::any_of(c("min", "elevation"))) |>
      dplyr::rename(time = date) |>
      dplyr::group_by(
        station,
        year = lubridate::year(time),
        month = lubridate::month(time)
      ) |>
      dplyr::summarise(
        temperature_mean_sncdc = mean(temperature_mean_sncdc),
        temperature_max_sncdc = mean(temperature_max_sncdc),
        precipitation_sncdc = sum(precipitation_sncdc),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        time = lubridate::ymd(glue::glue("{year}-{month}"), truncated = 1)
      ) |>
      dplyr::select(!dplyr::any_of(c("year", "month"))) |>
      dplyr::filter(
        temperature_max_sncdc < 60,
        temperature_mean_sncdc < 60,
        lubridate::month(time) %in% c(1, 2, 3)
      )

    gshtd_temperature_mean <-
      stars::read_mdim("./data/processed/temperature_gshtd_mean.nc") |>
      stars::st_extract(at = ncdc_points) |>
      tibble::as_tibble() |>
      dplyr::rename(temperature_mean_smodel = temperature) |>
      sf::st_as_sf() |>
      sf::st_join(ncdc_points) |>
      sf::st_drop_geometry()

    gshtd_temperature_max <-
      stars::read_mdim("./data/processed/temperature_gshtd_max.nc") |>
      stars::st_extract(at = ncdc_points) |>
      tibble::as_tibble() |>
      dplyr::rename(temperature_max_smodel = temperature) |>
      sf::st_as_sf() |>
      sf::st_join(ncdc_points) |>
      sf::st_drop_geometry()

    chirps_precipitation <-
      stars::read_mdim("./data/processed/precipitation_chirps.nc") |>
      stars::st_extract(at = ncdc_points) |>
      tibble::as_tibble() |>
      dplyr::rename(precipitation_smodel = precipitation) |>
      sf::st_as_sf() |>
      sf::st_join(ncdc_points) |>
      sf::st_drop_geometry()

    merged_tables_wide <-
      gshtd_temperature_mean |>
      dplyr::full_join(
        gshtd_temperature_max,
        by = dplyr::join_by(time, station)
      ) |>
      dplyr::full_join(
        chirps_precipitation,
        by = dplyr::join_by(time, station)
      ) |>
      dplyr::inner_join(
        ncdc_temp_prcp,
        by = dplyr::join_by(time, station)
      ) |>
      dplyr::group_by(year = lubridate::year(time), station) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::starts_with("temperature"),
          \(x) mean(x, na.rm = TRUE)
        ),
        dplyr::across(
          dplyr::starts_with("precipitation"),
          \(x) sum(x, na.rm = TRUE)
        ),
        .groups = "drop"
      )

    readr::write_csv(
      merged_tables_wide,
      "./data/validation/validation_wide.csv"
    )

    merged_tables_long <-
      merged_tables_wide |>
      tidyr::pivot_longer(
        cols = dplyr::any_of(
          c(
            "temperature_mean_smodel", "temperature_mean_sncdc",
            "temperature_max_smodel", "temperature_max_sncdc",
            "precipitation_smodel", "precipitation_sncdc"
          )
        ),
        names_to = c("var", "source"),
        names_sep = "_s"
      )

    readr::write_csv(
      merged_tables_long,
      "./data/validation/validation_long.csv"
    )

    merged_tables_long_s <-
      merged_tables_long |>
      tidyr::pivot_wider(
        names_from = "source",
        values_from = "value"
      )

    readr::write_csv(
      merged_tables_long_s,
      "./data/validation/validation_long_s.csv"
    )

  }