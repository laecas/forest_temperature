# -----------------------------------------------------------------------------
# This function merges all data into one table
merge_data <-
  function() {
    # Load bigger reference grid
    reference_big <- stars::read_stars(.x = "data/reference_big.tif") |>
      rlang::set_names(nm = "id")

    # Load NCDC meteorological station data to be merged
    ncdc_stations <-
      sf::read_sf(dsn = "./data/processed/temperature_ncdc.gpkg") |>
      dplyr::group_by(station, year = lubridate::year(.data[["time"]])) |>
      dplyr::summarise(
        temperature_mean_ncdc = mean(temperature_mean_ncdc, na.rm = TRUE),
        temperature_max_ncdc = mean(temperature_max_ncdc, na.rm = TRUE),
        precipitation_ncdc = sum(precipitation_ncdc, na.rm = TRUE),
        elevation_ncdc = mean(elevation_ncdc, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(time = lubridate::ymd(glue::glue("{year}-01-01"))) |>
      dplyr::select(!dplyr::any_of(c("year"))) |>
      sf::st_join(y = sf::st_as_sf(reference_big, as_points = FALSE))

    # Load GSHTD temperature data to be merged
    # Mean temperature
    gshtd_temperature_mean <-
      stars::read_mdim(
        filename = "./data/processed/temperature_gshtd_mean.nc"
      ) |>
      aggregate(by = "1 year", FUN = mean, na.rm = TRUE) |>
      tibble::as_tibble() |>
      tidyr::drop_na() |>
      dplyr::rename(temperature_mean = temperature)

    # Maximum temperature
    gshtd_temperature_max <-
      stars::read_mdim(
        filename = "./data/processed/temperature_gshtd_max.nc"
      ) |>
      aggregate(by = "1 year", FUN = mean, na.rm = TRUE) |>
      tibble::as_tibble() |>
      tidyr::drop_na() |>
      dplyr::rename(temperature_max = temperature)

    # Load ANADEM elevation data to be merged
    anadem_elevation <-
      stars::read_mdim(filename = "./data/processed/elevation_anadem.nc") |>
      tibble::as_tibble() |>
      tidyr::drop_na()

    # Load CHIRPS precipitation data to be merged
    chirps_precipitation <-
      stars::read_mdim(
        filename = "./data/processed/precipitation_chirps.nc"
      ) |>
      aggregate(by = "1 year", FUN = sum, na.rm = TRUE) |>
      tibble::as_tibble() |>
      tidyr::drop_na()

    # Load Mapbiomas land use and land cover data to be merged
    mapbiomas_lulc <-
      stars::read_mdim(filename = "./data/processed/lulc_mapbiomas.nc") |>
      tibble::as_tibble() |>
      tidyr::drop_na()

    # Load land use and land cover scenario to be merged
    lulc_scenario <-
      stars::read_stars(
        .x = fs::dir_ls("./data/processed/", regexp = "lulc_scenario")
      ) |>
      split() |>
      c(reference_big) |> # Get cell index
      tibble::as_tibble() |>
      tidyr::drop_na() |>
      dplyr::mutate(
        time = basename(
          fs::dir_ls("./data/processed/", regexp = "lulc_scenario")
        ) |>
          stringr::str_extract(pattern = "\\d{4}") |>
          lubridate::ymd(truncated = 2)
      ) |>
      dplyr::rename_with(
        .fn = \(x) glue::glue("scenario_{x}"),
        .cols = dplyr::all_of(
          c(
            "urban",
            "forest",
            "agriculture",
            "pasture",
            "mosaic"
          )
        )
      )

    # Merge data
    merged_table <-
      dplyr::full_join(
        x = gshtd_temperature_mean,
        y = gshtd_temperature_max,
        by = dplyr::join_by(x, y, time)
      ) |>
      dplyr::full_join(
        y = anadem_elevation,
        by = dplyr::join_by(x, y)
      ) |>
      dplyr::full_join(
        y = mapbiomas_lulc,
        by = dplyr::join_by(x, y, time)
      ) |>
      dplyr::full_join(
        y = chirps_precipitation,
        by = dplyr::join_by(x, y, time)
      ) |>
      tidyr::drop_na() |>
      sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(reference_big))

    merged_table <- reference_big |>
      rlang::set_names(nm = "id") |>
      sf::st_as_sf() |>
      sf::st_join(y = merged_table) |>
      tidyr::drop_na() |>
      tibble::as_tibble() |>
      dplyr::left_join(
        y = ncdc_stations |> sf::st_drop_geometry(),
        by = dplyr::join_by("id", "time")
      ) |>
      dplyr::left_join(
        y = lulc_scenario,
        by = dplyr::join_by("id", "time")
      ) |>
      dplyr::mutate(time = lubridate::ymd(time, tz = NULL)) |>
      dplyr::select(!dplyr::all_of(c("x", "y")))

    # Retrieve land cover values from neighboring cells
    buffer_size <- stars::st_res(x = reference_big) * 1.8

    final_merged_table <-
      purrr::map(
        .x = merged_table[["time"]] |> unique(),
        \(y) {
          sub_table <- merged_table |>
            dplyr::filter(time == y) |>
            sf::st_as_sf()

          buffer_table <- sub_table |>
            sf::st_buffer(dist = buffer_size)

          final_table <- sub_table |>
            dplyr::rename(ids = id) |>
            sf::st_join(y = buffer_table |> dplyr::select(id, geometry)) |>
            tibble::as_tibble() |>
            dplyr::filter(id != ids) |>
            dplyr::summarise(
              dplyr::across(
                .cols = dplyr::matches(c("urban|forest|agriculture")),
                .fns = \(x) mean(x, na.rm = TRUE)
              ),
              .by = "id"
            ) |>
            dplyr::rename_with(
              .cols = dplyr::matches(c("urban|forest|agriculture")),
              .fn = \(x) paste0("neighbor_", x)
            ) |>
            dplyr::left_join(y = sub_table, by = "id") |>
            sf::st_as_sf()
        }
      ) |>
      purrr::list_rbind()

    clean_merged_table <- final_merged_table |>
      tibble::as_tibble() |>
      dplyr::relocate("id", "time", "station") |>
      dplyr::relocate(dplyr::matches("temperature"), .after = "station") |>
      dplyr::relocate(
        dplyr::matches("elevation|precipitation"),
        .after = "temperature_max_ncdc"
      ) |>
      dplyr::relocate(
        dplyr::matches("scenario"),
        .after = dplyr::last_col()
      ) |>
      dplyr::relocate("geometry", .after = dplyr::last_col()) |>
      dplyr::mutate(
        geometry = geoarrow::as_geoarrow_vctr(.data[["geometry"]])
      )

    # Wrtie merged data to disk as parquet file
    arrow::write_parquet(
      clean_merged_table,
      "./data/processed/merged/merged_table.parquet",
      version = 2.6
    )

    return(invisible(NULL))
  }

# -----------------------------------------------------------------------------
