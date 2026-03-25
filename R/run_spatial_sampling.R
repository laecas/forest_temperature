# -----------------------------------------------------------------------------
# This function creates spatial samples from a regular grid.
# The spatial samples are created respecting a minimum distance between
# training and testing observations.
create_spatial_samples <-
  function(
    area_of_interest,
    n_samples = 100
  ) {
    # Load area of interest
    # Transform its coordinate reference system (CRS) to match the project CRS
    aoi <- sf::read_sf(dsn = glue::glue("./data/{area_of_interest}.gpkg")) |>
      sf::st_transform(sf::st_crs(readr::read_file("./data/project_crs.txt")))

    # Load a raster file (reference_big.tif), rename its band to "id"
    # Crop it to the AOI extent, then convert raster cells into points (sf object)
    reference_big <- stars::read_stars("./data/reference_big.tif") |>
      rlang::set_names("id") |>
      sf::st_crop(aoi) |>
      sf::st_as_sf(as_points = TRUE)

    # Generate multiple spatial sample splits
    spatial_data_split <-
      purrr::map(
        .progress = TRUE,
        .x = 1:n_samples,
        .f = \(s) {
          set.seed(s)

          # Randomly select 10% of the raster points
          # Then perform spatial buffer v-fold cross-validation
          spatial_data_split <-
            reference_big |>
            dplyr::slice_sample(prop = 0.1) |>
            spatialsample::spatial_buffer_vfold_cv(
              radius = 1800, # Radius for test points cluster
              buffer = 9000, # Buffer distance to separate train/test
              v = 20 # Number of folds
            )

          # For each fold, extract train/test sets and annotate them
          purrr::map2(
            .x = spatial_data_split$splits,
            .y = spatial_data_split$id,
            .f = \(fold, fold_id) {
              # Extract test cells and label them
              test_cells <-
                fold |>
                spatialsample::assessment() |>
                dplyr::mutate(split_spatial = "test")

              # Extract training cells and label them
              train_cells <-
                fold |>
                spatialsample::analysis() |>
                dplyr::mutate(split_spatial = "train")

              # Combine train and test cells, add sample and fold IDs
              sample_cells <- test_cells |>
                dplyr::bind_rows(train_cells) |>
                dplyr::mutate(
                  sample_id = s,
                  fold_id = fold_id,
                )

              return(sample_cells)
            }
          ) |>
            purrr::list_rbind() |>
            tibble::as_tibble() |>
            dplyr::select(!dplyr::all_of("geometry"))
        }
      ) |>
      purrr::list_rbind() # Bind all sample iterations together

    # Save the final dataset as a GeoPackage file
    sf::write_sf(
      spatial_data_split,
      "./data/spatial_data_sample_split.gpkg"
    )
  }

# -----------------------------------------------------------------------------
