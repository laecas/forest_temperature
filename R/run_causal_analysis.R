# -----------------------------------------------------------------------------
# This function performs calculations of the effect of forest cover over air
# temperatures.
calc_effect <-
  function(
    variable, # outcome variable of interest
    year # year used to filter the merged dataset
  ) {
    # Load the merged dataset from parquet file
    # Filter rows for the specified year
    # Convert geometry column into spatial objects (sf)
    # Replace geometries with their centroids
    merged_data <-
      arrow::open_dataset(
        "./data/processed/merged/merged_table.parquet"
      ) |>
      dplyr::filter(lubridate::year(time) == {{ year }}) |>
      dplyr::collect() |>
      dplyr::mutate(geometry = sf::st_as_sfc(geometry)) |>
      sf::st_as_sf() |>
      dplyr::mutate(geometry = sf::st_centroid(geometry)) |>
      dplyr::mutate(latitude = sf::st_coordinates(geometry)[, 2]) # Get latitude

    # Extract covariates (predictor variables)
    covariates <- merged_data |>
      sf::st_drop_geometry() |>
      tibble::as_tibble() |>
      dplyr::select(
        dplyr::all_of(
          c(
            "urban",
            "elevation",
            "precipitation",
            "agriculture",
            "pasture",
            "mosaic"
          )
        )
      )

    # Extract treatment variable (forest cover)
    treatment <- merged_data |>
      sf::st_drop_geometry() |>
      tibble::as_tibble() |>
      dplyr::pull(.data[["forest"]])

    # Extract outcome variable (specified by user)
    outcome <- merged_data |>
      sf::st_drop_geometry() |>
      tibble::as_tibble() |>
      dplyr::pull(.data[[variable]])

    # Fit causal forest model
    crf <-
      grf::causal_forest(
        X = covariates,
        Y = outcome,
        W = treatment,
        num.trees = 5000, # number of trees
        mtry = 6, # number of variables tried at each split
        alpha = 0.15, # significance level for splitting
        imbalance.penalty = 2, # penalty for imbalance
        seed = 1 # seed for reproducibility
      )

    # Compute average treatment effect (ATE)
    ate <- grf::average_treatment_effect(crf)

    # Predict conditional average treatment effects (CATE)
    cate <- predict(crf)[["predictions"]]

    # Best linear projection of treatment effects
    blp <- grf::best_linear_projection(crf, covariates)

    # Store results in a list
    crf_results <- list(ate, cate, blp)

    return(crf_results)
  }

# -----------------------------------------------------------------------------
# This function creates visualizations for the results of the causal forest
# model, used to calculate the effect of forest cover over air temperature.
run_effect_results <-
  function(
    model_results
  ) {
    # Average Treatment Effect (ATE) histogram
    ate_plot <- purrr::imap(
      .x = model_results,
      \(m, mid) {
        # Extract the first element (ATE estimates) from each model result
        m[[1]] |>
          tibble::as_tibble() |>
          dplyr::mutate(
            var = mid,
            name = c("estimate", "std_error"),
            .before = dplyr::everything()
          )
      }
    ) |>
      unname() |>
      purrr::list_rbind() |> # Combine list of tibbles into one tibble
      tidyr::pivot_wider() |>
      dplyr::mutate(
        var = dplyr::case_when(
          var == "tmax" ~ "Maximum Temperature",
          var == "tmean" ~ "Mean Temperature"
        )
      ) |>
      ggplot2::ggplot() +
      ggplot2::geom_pointrange(
        mapping = ggplot2::aes(
          x = var,
          y = estimate,
          ymax = estimate + std_error,
          ymin = estimate - std_error
        )
      ) +
      ggplot2::geom_text(
        # Annotate mean value
        mapping = ggplot2::aes(
          x = var,
          y = estimate,
          label = round(estimate, digits = 2)
        ),
        hjust = 0.5,
        vjust = -1
      ) +
      ggplot2::scale_y_continuous(
        n.breaks = 4,
        labels = ~ format(.x, scientific = FALSE)
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = NULL,
        y = "ATE (°C)"
      ) +
      cowplot::theme_half_open(font_size = 11) +
      cowplot::background_grid(major = "x")

    # Conditional Average Treatment Effect (CATE) histogram
    cate_hist <-
      purrr::imap(
        .x = model_results,
        \(m, mid) {
          # Extract the first element (ATE estimates) from each model result
          tibble::tibble({{ mid }} := m[[2]]) |>
            dplyr::slice_sample(prop = 0.001)
        }
      ) |>
      unname() |>
      purrr::list_cbind() |>
      tidyr::pivot_longer(cols = dplyr::everything()) |>
      ggplot2::ggplot() +
      ggridges::geom_density_ridges(
        # Density of CATE values
        ggplot2::aes(x = value, y = name),
        bandwidth = 0.2,
        quantile_lines = TRUE,
        quantiles = c(0.025, 0.975),
        alpha = 0.7,
        jittered_points = TRUE,
        position = "raincloud",
        scale = 0.8,
        point_size = 0.5,
        point_alpha = 0.5
      ) +
      ggplot2::geom_vline(
        # Vertical dashed line at mean
        mapping = ggplot2::aes(xintercept = 0),
        linetype = "dashed"
      ) +
      ggplot2::labs(
        y = NULL,
        x = "CATE (°C)"
      ) +
      ggplot2::scale_x_continuous(
        labels = ~ format(.x, scientific = FALSE)
      ) +
      cowplot::theme_half_open(font_size = 11) +
      cowplot::background_grid(major = "x") +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(
          linetype = "solid",
          linewidth = 0.8,
          color = "#000000"
        ),
        strip.placement = "outside"
      )

    # Combine ATE and TE plots side by side
    effect_plot <-
      cowplot::plot_grid(
        ate_plot,
        cate_hist,
        ncol = 2,
        rel_widths = c(1, 1),
        labels = "auto",
        label_size = 11,
        label_x = c(0.48, 0.03)
      )

    # Save combined histogram plot
    ggplot2::ggsave(
      "./figures/results/effect_histograms.png",
      effect_plot,
      units = "cm",
      width = 15,
      height = 5,
      dpi = 600
    )

    # Plot model parameter distributions
    model_parameters <-
      purrr::imap(
        .x = model_results,
        \(m, mid) {
          # Extract model parameters (third element of results)
          results <- m[[3]] |>
            broom::tidy() |>
            dplyr::mutate(name = mid)
        }
      ) |>
      unname() |>
      purrr::list_rbind() |>
      dplyr::mutate(term = stringr::str_to_title(term)) |>
      dplyr::filter(
        stringr::str_detect(
          term,
          pattern = "Intercept",
          negate = TRUE
        )
      ) |>
      ggplot2::ggplot() +
      ggplot2::facet_grid(
        ~name,
        scales = "free_y",
        labeller = ggplot2::labeller(
          name = c(tmax = "Maximum Temperature", tmean = "Mean Temperature")
        )
      ) +
      ggplot2::geom_pointrange(
        # Histogram of parameter estimates
        ggplot2::aes(
          x = estimate,
          y = term,
          xmax = estimate + std.error,
          xmin = estimate - std.error
        )
      ) +
      ggplot2::geom_vline(
        # Reference line at 0
        xintercept = 0,
        linetype = "dashed"
      ) +
      ggplot2::labs(
        y = NULL,
        x = "Estimates (°C)"
      ) +
      ggplot2::scale_x_continuous(labels = ~ format(.x, scientific = FALSE)) +
      cowplot::theme_half_open(font_size = 11) +
      cowplot::background_grid(major = "xy") +
      ggplot2::theme(
        strip.background = ggplot2::element_rect(
          linetype = "solid",
          linewidth = 0.8,
          color = "#000000"
        ),
        strip.text.y = ggplot2::element_text(size = 8),
        strip.placement = "outside"
      )

    # Save parameter distribution plot
    ggplot2::ggsave(
      "./figures/results/effect_parameters.png",
      model_parameters,
      units = "cm",
      width = 15,
      height = 7,
      dpi = 600
    )
  }

# -----------------------------------------------------------------------------
