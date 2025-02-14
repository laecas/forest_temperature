create_spatial_samples <-
  function(
    year_subset = 2015,
    n_samples = 100
  ) {

    merged_table <-
      readr::read_csv(
        "./data/processed/merged_table.csv",
        show_col_types = FALSE,
        progress = FALSE
      ) |>
      dplyr::filter(lubridate::year(time) == year_subset) |>
      dplyr::mutate(id = dplyr::row_number()) |>
      dplyr::rename(temperature = temperature_max)

    spatial_data_split <-
      purrr::map(
        .x = 1:n_samples,
        .f = \(s) {

          set.seed(s)
          spatial_data_split <-
            merged_table |>
            dplyr::slice_sample(prop = 0.10) |>
            sf::st_as_sf(coords = c("x", "y")) |>
            sf::st_set_crs(
              sf::st_crs(readr::read_file("./data/project_crs.txt"))
            ) |>
            spatialsample::spatial_buffer_vfold_cv(
              radius = NULL,
              buffer = 4000,
              v = 4
            ) |>
            spatialsample::get_rsplit(1)

          test_cells <-
            spatial_data_split |>
            spatialsample::assessment() |>
            sf::st_drop_geometry() |>
            dplyr::mutate(split_spatial = "test")

          train_cells <-
            spatial_data_split |>
            spatialsample::analysis() |>
            sf::st_drop_geometry() |>
            dplyr::mutate(split_spatial = "train")

          test_cells |>
            dplyr::bind_rows(train_cells) |>
            dplyr::mutate(sample_id = s)

        }
      ) |>
      purrr::list_rbind()

    readr::write_csv(
      spatial_data_split,
      "./data/spatial_data_sample_split.csv"
    )

  }

lm_direct_effect <-
  function(
    year_subset = 2015
  ) {

    merged_table <-
      readr::read_csv(
        "./data/processed/merged_table.csv",
        show_col_types = FALSE,
        progress = FALSE
      ) |>
      dplyr::filter(lubridate::year(time) == year_subset) |>
      dplyr::rename(temperature = temperature_max)

    # Calculate the direct effect of forest cover
    # on temperature using orthogonalization
    forest_residual <-
      lm(
        "forest ~ elevation + urban + precipitation + agriculture",
        data = merged_table
      ) |>
      predict() - merged_table$forest

    temperature_residual <-
      lm(
        "temperature ~ elevation + urban + precipitation + agriculture",
        data = merged_table
      ) |>
      predict() - merged_table$temperature

    lm_orth <-
      tibble::tibble(temperature = temperature_residual, forest = forest_residual)

    forest_direct_effect <- lm_orth |>
      lm(
        formula = "temperature ~ forest"
      ) |>
      broom::tidy() |>
      dplyr::filter(term == "forest") |>
      dplyr::pull(estimate) |>
      round(digits = 5)

    merged_table |>
      lm(
        formula = "temperature ~ forest + elevation + urban + precipitation + agriculture"
      ) |>
      broom::tidy()

    set.seed(1)
    lm_orth |>
      dplyr::slice_sample(prop = 0.5) |>
      ggplot2::ggplot(ggplot2::aes(x = temperature, y = forest)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(
        method = "lm", formula = "y ~ x", color = "#ad1a29"
      ) +
      ggplot2::annotate(
        geom = "label",
        x = -2.7,
        y = 1200,
        label = "Linear Regression: temperature ~ forest + elevation + urban + precipitation + agriculture",
        hjust = 0
      ) +
      ggplot2::annotate(
        geom = "label",
        x = -2.7,
        y = 1100,
        label = glue::glue(
          "Direct Effect (ATE): {forest_direct_effect}",
          " degrees celcius per pixel of forest cover",
          " | {round(forest_direct_effect / 0.09, 5)}",
          " degrees celcius per hectare of forest cover"
        ),
        hjust = 0
      ) +
      ggplot2::labs(
        title = "Dispersion of Forest Cover and Max Temperature residuals",
        x = "Residual Max Temperature",
        y = "Residual Forest Cover"
      ) +
      ggplot2::theme_dark()

  }

rf_direct_effect <-
  function(
    year_subset = 2015
  ) {

    spatial_data_split <-
      readr::read_csv(
        "./data/spatial_data_sample_split.csv"
      )

    effect_estimates <-
      purrr::map(
        .x = unique(spatial_data_split$sample_id),
        .f = \(s) {

          spatial_data_split_sub <-
            spatial_data_split |>
            dplyr::filter(sample_id == s)

          train_data <-
            spatial_data_split_sub |>
            dplyr::filter(split_spatial == "train")

          test_data <-
            spatial_data_split_sub |>
            dplyr::filter(split_spatial == "test")

          rf_estimates <-
            purrr::map(
              .x = c(forest = "forest", temperature = "temperature"),
              .f = \(m) {

                model_pred <-
                  aorsf::orsf(
                    formula = glue::glue(
                      "{m} ~ elevation + urban + precipitation + agriculture"
                    ) |> rlang::parse_expr(),
                    data = train_data
                  ) |>
                  predict(new_data = test_data)

                model_res <- test_data[[m]] - model_pred[ ,1]

                return(model_res)

              }
            ) |>
            dplyr::bind_cols()

          forest_direct_effect <- rf_estimates |>
            lm(
              formula = "temperature ~ forest"
            ) |>
            broom::tidy() |>
            dplyr::filter(term == "forest") |>
            dplyr::pull(estimate) |>
            round(digits = 5)

        }
      )

    results_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_histogram(
        data = tibble::as_tibble(unlist(effect_estimates)),
        ggplot2::aes(x = value),
        color = "#000000"
      ) +
      ggplot2::labs(
        x = "Average Treatment Effect (ATE)",
        y = "Count"
      ) +
      ggplot2::scale_x_continuous(
        labels = ~ format(.x, scientific = FALSE)
      ) +
      ggplot2::theme_bw(base_size = 12)

    ggplot2::ggsave(
      "./figures/results/rf_direct_effect_hist.png",
      results_plot,
      units = "cm",
      width = 15,
      height = 8
    )

  }