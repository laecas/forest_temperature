temperature_scatterplot <-
  function() {

    validation_data <-
      readr::read_csv("./data/validation/validation_long_s.csv") |>
      dplyr::filter(
        stringr::str_detect(var, "temperature")
      )

    # Calculate Mean Average Error
    mae <- validation_data |>
      dplyr::mutate(difference = ncdc - model) |>
      dplyr::summarise(mae = mean(abs(difference)), .by = "var")

    # Calculate bias and pbias
    bias <-  validation_data |>
      dplyr::mutate(difference = model - ncdc) |>
      dplyr::summarise(
        bias = mean(difference),
        pbias = sum(difference) / sum(ncdc) * 100,
        .by = "var"
      )

    # Create table with metrics
    metrics <- mae |>
      dplyr::full_join(bias, by = "var")

    validation_plot <-
      validation_data |>
      ggplot2::ggplot() +
      ggplot2::facet_grid(
        ~ forcats::fct_relevel(var, "temperature_mean", "temperature_max"),
        labeller = ggplot2::as_labeller(
          c(
            `temperature_mean` = "Mean Temperature (C°)",
            `temperature_max` = "Maximum Temperature (C°)"
          )
        )
      ) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = ncdc, y = model),
        size = 1.5
      ) +
      ggplot2::geom_abline(
        intercept = 0,
        slope = 1,
        linetype = "dashed"
      ) +
      ggplot2::geom_text(
        data = metrics,
        size = 3,
        color = "#000000",
        ggplot2::aes(
          x = 15,
          y = 33,
          hjust = 0,
          vjust = 1,
          fontface = "bold",
          label = glue::glue(
            "MAE = {round(mae, 2)}",
            "\n",
            "BIAS = {round(bias, 2)}",
            "\n",
            "PBIAS = {round(pbias, 2)}"
          )
        )
      ) +
      ggplot2::labs(
        x = "Station Observations",
        y = "GSHTD Estimations"
      ) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::coord_fixed()

    ggplot2::ggsave(
      "./figures/validation/temperature_scatterplot.png",
      validation_plot,
      units = "cm",
      width = 15,
      height = 8
    )

    return(validation_plot)

  }

temperature_timeseries <-
  function() {

    validation_data <-
      readr::read_csv("./data/validation/validation_long.csv") |>
      dplyr::filter(
        stringr::str_detect(var, "temperature")
      ) |>
      dplyr::summarise(
        value = mean(value),
        .by = c("year", "var", "source")
      ) |>
      dplyr::mutate(
        year = lubridate::ymd(year, truncated = 2),
        source = forcats::fct_recode(
          source,
          "Station Observations" = "ncdc",
          "GSHTD Estimations" = "model"
        ),
        var = forcats::fct_recode(
          var,
          "Mean Temperature (C°)" = "temperature_mean",
          "Max Temperature (C°)" = "temperature_max"
        )
      )

    validation_data_s <-
      readr::read_csv("./data/validation/validation_long_s.csv") |>
      dplyr::filter(
        stringr::str_detect(var, "temperature")
      ) |>
      dplyr::mutate(
        difference = model - ncdc,
        year = lubridate::ymd(year, truncated = 2),
        var = forcats::fct_recode(
          var,
          "Mean Temperature (C°)" = "temperature_mean",
          "Max Temperature (C°)" = "temperature_max"
        )
      )

    validation_plot_a <-
      validation_data |>
      ggplot2::ggplot() +
      ggplot2::geom_line(
        mapping = ggplot2::aes(
          x = year, y = value, color = var, linetype = source
        ),
        size = 1.1
      ) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          x = year, y = value, color = var, shape = source
        ),
        size = 2,
        fill = "#FFFFFF"
      ) +
      ggplot2::labs(
        x = "Year",
        y = "Temperature (C°)",
        color = "Variable",
        shape = "Data Source",
        linetype = "Data Source"
      ) +
      ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      ggplot2::scale_shape_manual(values = c(21, 23)) +
      scico::scale_color_scico_d(
        palette = "berlin", begin = 0.2, end = 0.8, direction = -1
      ) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.title = ggplot2::element_blank(),
        legend.direction = "vertical"
      )

    validation_plot_b <-
      validation_data_s |>
      ggplot2::ggplot() +
      ggplot2::stat_summary(
        geom = "crossbar",
        fun.data = ggplot2::mean_se,
        mapping = ggplot2::aes(x = year, y = difference)
      ) +
      ggplot2::geom_jitter(
        mapping = ggplot2::aes(
          x = year, y = difference, color = var, shape = var, fill = var
        ),
        size = 2,
        width = 40
      ) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::labs(
        x = "Year",
        y = "Error (Estimated - Observed)",
        color = "Variable",
        shape = "Variable",
        fill = "Variable"
      ) +
      ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      ggplot2::scale_shape_manual(values = c(24, 25)) +
      scico::scale_color_scico_d(
        palette = "berlin", begin = 0.2, end = 0.8, direction = -1
      ) +
      scico::scale_fill_scico_d(
        palette = "berlin", begin = 0.2, end = 0.8, direction = -1
      ) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.title = ggplot2::element_blank()
      )

    validation_plot <-
      cowplot::plot_grid(
        validation_plot_a,
        validation_plot_b,
        ncol = 1,
        labels = c("a", "b"),
        label_size = 12,
        label_x = 0.11,
        label_y = 0.98
      )

    ggplot2::ggsave(
      "./figures/validation/temperature_timeseries.png",
      validation_plot,
      units = "cm",
      width = 15,
      height = 17
    )

    return(validation_plot)

  }