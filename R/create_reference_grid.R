create_reference_grid <-
  function(
    big = 900,
    small = 30
  ) {

    # Set Project CRS
    project_crs <-
      sf::st_crs(readr::read_file("./data/project_crs.txt"))

    temperature_stars <-
      stars::read_stars(
        fs::dir_ls("./data/raw/temperature_gshtd/")[1]
      )

    reference_big <-
      temperature_stars |>
      stars::st_warp(
        crs = project_crs,
        cellsize = c(big, big)
      ) |>
      setNames("value") |>
      dplyr::mutate(
        cell_id = seq_along(value)
      ) |>
      dplyr::select("cell_id")

    stars::write_stars(
      reference_big,
      "./data/reference_big.tif"
    )

    reference_small <-
      reference_big |>
      stars::st_warp(
        crs = project_crs,
        cellsize = c(small, small)
      ) |>
      setNames("value") |>
      dplyr::mutate(
        cell_id = seq_along(value)
      ) |>
      dplyr::select("cell_id")

    stars::write_stars(
      reference_small,
      "./data/reference_small.tif"
    )

  }