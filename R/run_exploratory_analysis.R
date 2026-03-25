# -----------------------------------------------------------------------------
# This function creates an exploration plot, consisting of map, histogram and
# time series, of a certtain variable
describe_tempspat <-
  function(
    # Label for the variable
    vars_lab,
    # Name of the variable
    vars,
    # A number defining the density curve kernel, for density plots
    density_curve_kernel
  ) {
    # Load merged data
    merged_table <-
      arrow::open_dataset("./data/processed/merged/merged_table.parquet") |>
      dplyr::select(dplyr::any_of(c("time", vars))) |>
      dplyr::collect() |>
      tidyr::pivot_longer(cols = dplyr::any_of(vars)) |>
      dplyr::mutate(
        name = stringr::str_replace(name, "_", " ") |>
          stringr::str_to_title()
      ) |>
      dplyr::mutate(
        name = forcats::fct_relevel(
          name,
          vars |> stringr::str_replace("_", " ") |> stringr::str_to_title()
        ),
        time = forcats::as_factor(lubridate::year(time))
      ) |>
      dplyr::mutate(
        name = dplyr::case_when(
          name == "Temperature Max" ~ "Maximum Temperature",
          name == "Temperature Mean" ~ "Mean Temperature"
        )
      ) |>
      tidyr::drop_na()

    # Create histogram for selected variable
    histogram_plot <-
      merged_table |>
      ggplot2::ggplot() +
      ggplot2::facet_wrap(ggplot2::vars(name), ncol = 1, scales = "free_y") +
      ggplot2::geom_histogram(
        ggplot2::aes(x = value, y = ggplot2::after_stat(density)),
        color = "#000000",
        bins = 30
      ) +
      # ggplot2::geom_density(
      #   ggplot2::aes(x = value, y = ggplot2::after_stat(density)),
      #   color = "#975151",
      #   linewidth = 0.3,
      #   bw = density_curve_kernel
      # ) +
      ggplot2::labs(
        x = vars_lab,
        y = "Density"
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
        )
      )

    # Create map for selected variable
    reference_big <- stars::read_stars("./data/reference_big.tif")

    # Transform merged data to stars object
    merged_stars <-
      arrow::open_dataset("./data/processed/merged/merged_table.parquet") |>
      dplyr::select(dplyr::any_of(c("time", "id", vars))) |>
      dplyr::collect() |>
      dplyr::full_join(
        reference_big |>
          rlang::set_names("id") |>
          tibble::as_tibble(),
        by = "id"
      ) |>
      dplyr::summarise(
        dplyr::across(.cols = dplyr::all_of(vars), \(x) mean(x, na.rm = TRUE)),
        .by = c("id", "x", "y")
      ) |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::all_of(vars),
          \(x) scales::rescale(x, to = c(-1, 1))
        )
      ) |>
      tidyr::pivot_longer(cols = dplyr::any_of(vars)) |>
      dplyr::mutate(
        name = dplyr::case_when(
          name == "temperature_max" ~ "Maximum Temperature",
          name == "temperature_mean" ~ "Mean Temperature"
        )
      ) |>
      tidyr::drop_na()

    # Start time series plot
    if (stringr::str_detect(vars_lab, "Cover")) {
      timeseries_plot <- merged_table |>
        dplyr::summarise(
          value = sum(
            value * (stars::st_res(reference_big)[[1]]^2 * 0.000001)
          ),
          .by = c("time", "name")
        ) |>
        ggplot2::ggplot() +
        ggplot2::geom_col(
          ggplot2::aes(
            y = time,
            x = value,
            fill = forcats::fct_rev(name)
          ),
          position = "dodge",
          color = "#000000",
          linewidth = 0.3
        )
    } else {
      timeseries_plot <-
        merged_table |>
        ggplot2::ggplot() +
        ggridges::geom_density_ridges(
          ggplot2::aes(x = value, y = time, fill = name),
          bandwidth = density_curve_kernel,
          alpha = 0.7
        ) +
        ggplot2::stat_summary(
          fun.data = ggplot2::mean_se,
          ggplot2::aes(x = value, y = time, group = name),
          size = 0.3
        )
    }

    # Finish time series plot
    timeseries_plot <-
      timeseries_plot +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = stringr::str_remove(vars_lab, " Proportion"),
        y = "Years",
        fill = NULL
      ) +
      ggplot2::scale_x_continuous(
        labels = ~ format(.x, scientific = FALSE)
      ) +
      ggplot2::scale_y_discrete(
        breaks = seq(2000, 2020, by = 2)
      ) +
      scico::scale_fill_scico_d(
        palette = "vikO",
        begin = 0.2,
        end = 0.8,
        direction = -1
      ) +
      cowplot::theme_half_open(font_size = 11) +
      cowplot::background_grid(major = "y", minor = "y") +
      ggplot2::theme(
        legend.text = ggplot2::element_text(
          margin = ggplot2::margin(r = 20, l = 5)
        ),
        strip.background = ggplot2::element_rect(
          linetype = "solid",
          linewidth = 0.8,
          color = "#000000"
        )
      )

    # Create map plot
    map_plot <- merged_stars |>
      ggplot2::ggplot() +
      ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value)) +
      ggplot2::facet_wrap(
        ggplot2::vars(name),
        nrow = 2,
        labeller = ggplot2::labeller(
          name = \(x) {
            stringr::str_replace(x, "_", " ") |>
              stringr::str_to_title()
          }
        )
      ) +
      ggplot2::coord_equal() +
      ggplot2::labs(
        x = NULL,
        y = NULL,
        fill = glue::glue("{vars_lab} Variation")
      ) +
      scico::scale_fill_scico(
        palette = "vikO",
        begin = 0.1,
        end = 0.9,
        breaks = c(-1, 0, 1)
      ) +
      cowplot::theme_map(font_size = 11) +
      ggplot2::theme(
        legend.position = "bottom",
        axis.text = ggplot2::element_blank(),
        legend.key.height = ggplot2::unit(2, "mm"),
        legend.title = ggplot2::element_text(margin = ggplot2::margin(r = 15)),
        strip.background = ggplot2::element_rect(
          linetype = "solid",
          linewidth = 0.8,
          color = "#000000"
        )
      )

    # Create upper part of final plot composition
    upper_plot <-
      cowplot::plot_grid(
        map_plot,
        histogram_plot,
        ncol = 2,
        rel_widths = c(0.65, 0.35),
        labels = c("a", "b"),
        label_x = c(0, -0.08),
        label_size = 10
      )

    # Finish final plot composition by adding the lower part
    final_plot <-
      cowplot::plot_grid(
        upper_plot,
        timeseries_plot,
        ncol = 1,
        rel_heights = c(0.7, 0.3),
        labels = c("", "c"),
        label_y = 1.17,
        label_size = 10
      )

    # Set figure height
    fig_height <- 11

    if (vars_lab == "Elevation") {
      final_plot <- upper_plot
      fig_height <- 7
    }

    # Set file name
    file_name <- stringr::str_to_lower(vars_lab) |>
      stringr::str_replace_all(" ", "_")

    # Save final plot
    ggplot2::ggsave(
      glue::glue("./figures/exploration/tempspat_{file_name}.png"),
      final_plot,
      units = "cm",
      width = 15,
      height = fig_height
    )

    return(final_plot)
  }

# -----------------------------------------------------------------------------
# This function creates the results of the exploratory analysis.
# It is a plot that shows time series as boxplots of temperature,
# for each land cover for each year.
# It also shows the distributions of differences between the temperature
# difference between 2001-2005 and 2016-2020, for each land cover.
describe_lulc_temperature <-
  function() {
    # Load merged data
    merged_data <-
      arrow::open_dataset(
        "./data/processed/merged/merged_table.parquet"
      ) |>
      dplyr::filter(
        .data[["urban"]] > 0.9 |
          .data[["forest"]] > 0.9 |
          .data[["agriculture"]] > 0.9 |
          .data[["pasture"]] > 0.9 |
          .data[["mosaic"]] > 0.9
      ) |>
      dplyr::select(
        dplyr::all_of(
          c(
            "id",
            "urban",
            "forest",
            "agriculture",
            "pasture",
            "mosaic",
            "temperature_mean",
            "temperature_max",
            "time"
          )
        )
      ) |>
      dplyr::collect() |>
      tidyr::pivot_longer(
        cols = c("urban", "forest", "agriculture", "pasture", "mosaic"),
        names_to = "lulc"
      )

    # Create time series plot
    lulc_temperature_timeseries <- merged_data |>
      dplyr::filter(value != 0) |>
      tidyr::pivot_longer(
        cols = c("temperature_mean", "temperature_max"),
        names_to = "variable",
        values_to = "temperature"
      ) |>
      dplyr::mutate(
        lulc = stringr::str_to_title(lulc) |>
          forcats::fct_relevel(
            c("Mosaic", "Pasture", "Agriculture", "Urban", "Forest")
          )
      ) |>
      dplyr::mutate(
        variable = dplyr::case_when(
          variable == "temperature_max" ~ "Maximum Temperature",
          variable == "temperature_mean" ~ "Mean Temperature"
        )
      ) |>
      ggplot2::ggplot() +
      ggplot2::facet_wrap(
        ~variable,
        ncol = 1,
        scales = "free",
        labeller = ggplot2::labeller(
          variable = \(x) {
            stringr::str_replace(x, "_", " ") |>
              stringr::str_to_title()
          }
        )
      ) +
      ggplot2::geom_boxplot(
        mapping = ggplot2::aes(
          x = time,
          y = temperature,
          fill = lulc,
          group = interaction(time, lulc)
        ),
        position = "dodge",
        outliers = FALSE,
        alpha = 0.8
      ) +
      # ggplot2::geom_smooth(
      #   mapping = ggplot2::aes(
      #     x = time,
      #     y = temperature,
      #     fill = lulc,
      #     color = lulc
      #   ),
      #   se = FALSE
      # ) +
      scico::scale_fill_scico_d(
        palette = "vikO",
        begin = 0.2,
        end = 0.8,
        direction = -1
      ) +
      scico::scale_color_scico_d(
        palette = "vikO",
        begin = 0.2,
        end = 0.8,
        direction = -1
      ) +
      ggplot2::labs(y = "Air Temperature (°C)", x = "Year") +
      cowplot::theme_half_open(font_size = 11) +
      cowplot::background_grid(major = "y") +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        #legend.position = "none",
        strip.background = ggplot2::element_rect(
          linetype = "solid",
          linewidth = 0.8,
          color = "#000000"
        ),
        strip.placement = "outside"
      )

    # Create differences distributions plot
    lulc_temperature_diff <- merged_data |>
      dplyr::filter(
        value > 0.9,
        (lubridate::year(time) >= 2016 | lubridate::year(time) <= 2005)
      ) |>
      dplyr::mutate(
        time = dplyr::case_when(
          lubridate::year(time) >= 2016 ~ "after",
          lubridate::year(time) <= 2005 ~ "before"
        )
      ) |>
      dplyr::group_by(id) |>
      dplyr::mutate(id = dplyr::cur_group_id()) |>
      dplyr::filter(dplyr::n() > 1 & dplyr::n_distinct(lulc) == 1) |>
      dplyr::group_by(id, time, lulc) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::starts_with("temperature"),
          mean
        ),
        .groups = "drop_last"
      ) |>
      tidyr::pivot_longer(
        cols = c("temperature_mean", "temperature_max"),
        names_to = "variable",
        values_to = "temperature"
      ) |>
      dplyr::mutate(
        variable = dplyr::case_when(
          variable == "temperature_max" ~ "Maximum Temperature",
          variable == "temperature_mean" ~ "Mean Temperature"
        )
      ) |>
      tidyr::pivot_wider(
        names_from = time,
        values_from = temperature,
        id_cols = c("lulc", "id", "variable")
      ) |>
      tidyr::drop_na() |>
      dplyr::ungroup() |>
      dplyr::mutate(temperature_difference = after - before) |>
      dplyr::mutate(
        lulc = stringr::str_to_title(lulc) |>
          forcats::fct_relevel(
            c("Mosaic", "Pasture", "Agriculture", "Urban", "Forest")
          )
      ) |>
      ggplot2::ggplot() +
      ggplot2::facet_wrap(
        ~variable,
        ncol = 2,
        scales = "free_x",
        labeller = ggplot2::labeller(
          variable = \(x) {
            stringr::str_replace(x, "_", " ") |>
              stringr::str_to_title()
          }
        )
      ) +
      ggplot2::geom_density(
        mapping = ggplot2::aes(
          x = temperature_difference,
          y = ggplot2::after_stat(scaled),
          fill = lulc
        ),
        alpha = 0.7
      ) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
      scico::scale_fill_scico_d(
        palette = "vikO",
        begin = 0.2,
        end = 0.8,
        direction = -1
      ) +
      ggplot2::labs(x = "Air Temperature Difference (°C)", y = NULL) +
      cowplot::theme_half_open(font_size = 11) +
      cowplot::background_grid(major = "x") +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(
          linetype = "solid",
          linewidth = 0.8,
          color = "#000000"
        ),
        strip.placement = "outside",
        legend.margin = ggplot2::margin(r = 5)
      )

    # Create the final plot composition
    final_plot <-
      cowplot::plot_grid(
        lulc_temperature_timeseries,
        lulc_temperature_diff,
        ncol = 1,
        rel_heights = c(1, 0.5),
        align = "v",
        axis = "l",
        labels = "auto",
        label_size = 10
      )

    # Save final plot
    ggplot2::ggsave(
      "./figures/results/exploration.png",
      final_plot,
      units = "cm",
      width = 15,
      height = 13,
      dpi = 600
    )

    return(final_plot)
  }

# -----------------------------------------------------------------------------
