describe_hist <-
  function(
    vars_lab = "Temperature",
    vars = c("temperature_max", "temperature_mean"),
    distr = sgt::dsgt,
    distr_args = list(
      c(mu = 27, sigma =  1.5, lambda = 0.1),
      c(mu = 22, sigma = 1.5, lambda = 0.2)
    )
  ) {

    merged_table <-
      readr::read_csv(
        "./data/processed/merged_table.csv",
        show_col_types = FALSE,
        progress = FALSE
      ) |>
      dplyr::select(
        dplyr::any_of(
          c("time", vars)
        )
      ) |>
      tidyr::pivot_longer(
        cols = dplyr::any_of(vars)
      ) |>
      dplyr::mutate(
        name = stringr::str_replace(name, "_", " ") |>
          stringr::str_to_title()
      ) |>
      dplyr::mutate(
        name = forcats::fct_relevel(
          name,
          vars |> stringr::str_replace("_", " ") |>
            stringr::str_to_title()
        )
      )

    density_dist <-
      purrr::map2(
        .x = distr_args,
        .y = unique(merged_table$name),
        .f = \(f, l) {

          distr_table <-
            rlang::exec(
              distr,
              seq(
                min(merged_table$value),
                max(merged_table$value),
                length = 100
              ),
              !!!f
            ) |>
            tibble::as_tibble() |>
            dplyr::mutate(
              dens = value,
              name = l,
              value = seq(
                min(merged_table$value),
                max(merged_table$value),
                length = 100
              )
            )

          return(distr_table)

        }
      ) |>
      purrr::list_rbind()

    exploration_plot <-
      merged_table |>
      ggplot2::ggplot() +
      ggplot2::facet_grid(~ name) +
      ggplot2::geom_histogram(
        ggplot2::aes(x = value, y = ggplot2::after_stat(density)),
        color = "#000000"
      ) +
      ggplot2::geom_line(
        ggplot2::aes(x = value, y = dens),
        data = density_dist,
        color = "#975151",
        linewidth = 1
      ) +
      ggplot2::labs(
        x = vars_lab,
        y = "Density"
      ) +
      ggplot2::scale_x_continuous(
        labels = ~ format(.x, scientific = FALSE)
      ) +
      ggplot2::theme_bw(base_size = 12)

    file_name <- stringr::str_to_lower(vars_lab) |>
      stringr::str_replace_all(" ", "_")

    ggplot2::ggsave(
      glue::glue("./figures/exploration/hist_{file_name}.png"),
      exploration_plot,
      units = "cm",
      width = 15,
      height = 8
    )

    return(exploration_plot)

  }

describe_tempspat <-
  function(
    area_of_interest = "vale_paraiba",
    vars_lab = "Temperature",
    vars = c("temperature_max", "temperature_mean")
  ) {

    merged_table <-
      readr::read_csv(
        "./data/processed/merged_table.csv",
        show_col_types = FALSE,
        progress = FALSE
      ) |>
      dplyr::select(
        dplyr::any_of(
          c("time", vars)
        )
      ) |>
      tidyr::pivot_longer(
        cols = dplyr::any_of(vars)
      ) |>
      dplyr::mutate(
        name = stringr::str_replace(name, "_", " ") |>
          stringr::str_to_title()
      ) |>
      dplyr::mutate(
        name = forcats::fct_relevel(
          name,
          vars |>
            stringr::str_replace("_", " ") |>
            stringr::str_to_title()
        ),
        time = forcats::as_factor(lubridate::year(time))
      ) |>
      dplyr::filter(value > 0)

    merged_tif <-
      stars::read_stars(
        fs::dir_ls("./data/processed/merged_tifs/"),
        NA_value = 0
      ) |>
      stars::st_redimension(name = "time") |>
      stars::st_set_dimensions(
        "time",
        values = lubridate::ymd(glue::glue("{2000:2020}-01-01"), truncated = 2)
      ) |>
      split("band") |>
      dplyr::select(dplyr::all_of(vars)) |>
      aggregate(by = "20 years", FUN = mean)

    merged_tif <-
      purrr::map(
        vars,
        \(v) {

          merged_tif |>
            dplyr::select(dplyr::any_of(c(v))) |>
            dplyr::mutate({{ v }} := scales::rescale(.data[[v]], to = c(-1, 1)))

        }
      ) |>
      purrr::reduce(c) |>
      stars::st_redimension(name = "band") |>
      setNames("value")

    if (length(stars::st_dimensions(merged_tif)) == 3) {

      merged_tif <-
        merged_tif |>
        stars::st_set_dimensions(
          which = "time",
          values = vars,
          name = "band"
        )

    }

    if (stringr::str_detect(vars_lab, "Cover")) {

      exploration_plot_b <-
        merged_table |>
        dplyr::summarise(
          value = sum(value * (stars::st_res(merged_tif)[[1]] ^ 2 * 0.000001)),
          .by = c("time", "name")
        ) |>
        ggplot2::ggplot() +
        ggplot2::geom_col(
          ggplot2::aes(
            y = time, x = value, fill = forcats::fct_rev(name)
          ),
          position = "dodge",
          color = "#000000",
          linewidth = 0.3
        )

    } else {

      exploration_plot_b <-
        merged_table |>
        ggplot2::ggplot() +
        ggridges::geom_density_ridges(
          ggplot2::aes(x = value, y = time, fill = name)
        ) +
        ggplot2::stat_summary(
          fun.data = ggplot2::mean_se,
          ggplot2::aes(x = value, y = time, group = name),
          size = 0.5
        )

    }

    exploration_plot_b <-
      exploration_plot_b +
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
        palette = "vikO", begin = 0.3, end = 0.7, direction = -1
      ) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.text = ggplot2::element_text(
          margin = ggplot2::margin(r = 20, l = 5)
        )
      )

    exploration_plot_a <-
      ggplot2::ggplot() +
      stars::geom_stars(
        data = merged_tif,
        na.action = na.omit
      ) +
      ggplot2::geom_sf(
        data = sf::read_sf(
          glue::glue("./data/{area_of_interest}.gpkg")
        ),
        fill = "transparent",
        linewidth = 0.2,
        color = "#000000"
      ) +
      ggplot2::facet_grid(
        ~ band,
        labeller = ggplot2::labeller(
          band = \(x) {
            stringr::str_replace(x, "_", " ") |>
              stringr::str_to_title()
          }
        )
      ) +
      ggplot2::labs(
        x = NULL,
        y = NULL,
        fill = glue::glue("{vars_lab} Variation")
      ) +
      scico::scale_fill_scico(
        palette = "vikO", begin = 0.1, end = 0.9
      ) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(
        legend.position = "bottom",
        axis.text = ggplot2::element_blank(),
        legend.key.height = ggplot2::unit(2, "mm"),
        legend.title = ggplot2::element_text(margin = ggplot2::margin(r = 35))
      )

    exploration_plot <-
      cowplot::plot_grid(
        exploration_plot_a,
        exploration_plot_b,
        ncol = 1
      )

    fig_height <- 14

    if (vars_lab == "Elevation") {
      exploration_plot <- exploration_plot_a
      fig_height <- 7
    }

    file_name <- stringr::str_to_lower(vars_lab) |>
      stringr::str_replace_all(" ", "_")

    ggplot2::ggsave(
      glue::glue("./figures/exploration/tempspat_{file_name}.png"),
      exploration_plot,
      units = "cm",
      width = 15,
      height = fig_height
    )

    return(exploration_plot)

  }