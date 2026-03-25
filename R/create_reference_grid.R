# -----------------------------------------------------------------------------
# This function creates two spatial grids (as raster data) that must be
# perfectly aligned. So their resolution must be multiple of each other.
create_reference_grid <-
  function(
    # The name of a Geopackage file located in the data directory
    # (you must put the file there)
    area_of_interest = "sp",
    # Spatial resolution (same unit as the CRS) of the bigger reference grid
    big = 900,
    # Spatial resolution (same unit as the CRS) of the smaller reference grid
    small = 30
  ) {
    # Get Project CRS
    project_crs <-
      sf::st_crs(x = readr::read_file(file = "./data/project_crs.txt"))

    # Get bounding box of the region of interest
    reference_bbox <-
      sf::read_sf(dsn = glue::glue("./data/{area_of_interest}.gpkg")) |>
      sf::st_transform(crs = project_crs) |>
      sf::st_bbox()

    # Create a reference grid as a raster (bigger grid)
    reference_big <-
      stars::st_as_stars(
        .x = sf::st_bbox(reference_bbox),
        dx = big,
        dy = big
      ) |>
      rlang::set_names(nm = "value") |>
      dplyr::mutate(cell_id = seq_along(value)) |>
      dplyr::select("cell_id")

    # Write bigger reference grid to disk
    stars::write_stars(
      obj = reference_big,
      dsn = "./data/reference_big.tif"
    )

    # Create a reference grid as a raster (smaller grid)
    reference_small <-
      stars::st_as_stars(
        .x = sf::st_bbox(reference_big),
        dx = small,
        dy = small
      ) |>
      rlang::set_names(nm = "value") |>
      dplyr::mutate(cell_id = seq_along(value)) |>
      dplyr::select("cell_id")

    # Write smaller reference grid to disk
    stars::write_stars(
      obj = reference_small,
      dsn = "./data/reference_small.tif"
    )
  }

# -----------------------------------------------------------------------------
