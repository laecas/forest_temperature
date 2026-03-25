# -----------------------------------------------------------------------------
# This function trains a series of oblique random forest over the land cover
# data of 2020 .Than, it makes predictions of mean and maximum temperature for
# both land cover scenarios.
calc_scenarios <- function(year, data_sample, variable) {
  # Create ID row for data samples
  data_sample <- data_sample |>
    dplyr::mutate(
      model_id = dplyr::cur_group_id(),
      .by = c("sample_id", "fold_id")
    )

  # Load merged data for a specific year
  merged_data <-
    arrow::open_dataset("./data/processed/merged/merged_table.parquet") |>
    dplyr::filter(lubridate::year(time) == {{ year }}) |>
    dplyr::collect() |>
    dplyr::mutate(geometry = sf::st_as_sfc(geometry)) |>
    sf::st_as_sf() |>
    dplyr::mutate(geometry = sf::st_centroid(geometry)) |>
    dplyr::mutate(latitude = sf::st_coordinates(geometry)[, 2]) # Get latitude

  # Create a list containing the data of each land use scenario
  scenarios_list <-
    list(
      # As is scenario, land use is the same as original data
      "asis" = merged_data |>
        sf::st_drop_geometry() |>
        tibble::as_tibble() |>
        dplyr::select(
          dplyr::all_of(
            c(
              "forest",
              "urban",
              "elevation",
              "precipitation",
              "agriculture",
              "pasture",
              "mosaic"
            )
          )
        ),
      # If was scenario, all APPs and RLs areas become forest formations
      "ifwas" = merged_data |>
        sf::st_drop_geometry() |>
        tibble::as_tibble() |>
        dplyr::select(
          dplyr::all_of(
            c(
              "forest" = "scenario_forest",
              "urban" = "scenario_urban",
              "elevation",
              "precipitation",
              "agriculture" = "scenario_agriculture",
              "pasture" = "scenario_pasture",
              "mosaic" = "scenario_mosaic"
            )
          )
        )
    )

  # Run random forest models for each data sample
  # And make predictions for each land use scenario
  rf_results <-
    purrr::map(
      .progress = TRUE,
      .x = unique(data_sample$model_id)[1:3],
      \(mid) {
        # Get the data sample
        model_sample <- data_sample |>
          dplyr::filter(model_id == mid, split_spatial == "train") |>
          dplyr::select(dplyr::all_of(c("id", "split_spatial"))) |>
          dplyr::inner_join(merged_data, by = "id")

        test_sample <- data_sample |>
          dplyr::filter(model_id == mid, split_spatial == "test") |>
          dplyr::select(dplyr::all_of(c("id", "split_spatial"))) |>
          dplyr::inner_join(merged_data, by = "id")

        # Train oblique random forest model on original data
        rf <- aorsf::orsf(
          formula = glue::glue(
            "{variable} ~ forest + elevation + urban + agriculture + pasture + mosaic + precipitation"
          ) |>
            rlang::parse_expr(),
          data = model_sample |> sf::st_drop_geometry(),
          importance = "none"
        )

        model_validation <-
          predict(
            rf,
            new_data = test_sample,
            pred_type = "mean"
          ) |>
          as.data.frame() |>
          tibble::as_tibble() |>
          rlang::set_names("estimates") |>
          dplyr::mutate(
            original = test_sample |> dplyr::pull(.data[[variable]]),
            id = mid
          )

        # Make model predictions for the land use scenarios
        scenario_predictions <-
          purrr::imap(
            .x = scenarios_list,
            \(x, idx) {
              model_pred <-
                predict(
                  rf,
                  new_data = x,
                  pred_type = "mean"
                ) |>
                as.data.frame() |>
                tibble::as_tibble() |>
                rlang::set_names("estimates") |>
                dplyr::mutate(
                  id = merged_data[["id"]],
                  scenario = idx
                )
            }
          ) |>
          purrr::list_rbind()

        return(list(scenario_predictions, model_validation))
      }
    )

  rf_scenarios_results <-
    rf_results |>
    purrr::map(\(x) x[[1]]) |>
    purrr::list_rbind() |>
    dplyr::summarise(
      estimates = median(estimates),
      .by = c("id", "scenario")
    ) |>
    dplyr::mutate(var = variable) |>
    dplyr::full_join(
      stars::read_stars("./data/reference_big.tif") |>
        rlang::set_names("id") |>
        tibble::as_tibble(),
      by = "id"
    )

  rf_validation <- rf_results |>
    purrr::map(\(x) x[[2]]) |>
    purrr::list_rbind() |>
    dplyr::mutate(variable = variable)

  # Turn scenarios predictions into raster objects
  rf_stars <-
    purrr::map(
      .x = c("ifwas", "asis"),
      \(scen) {
        rf_scenarios_results |>
          dplyr::select(!id) |>
          tidyr::drop_na(scenario) |>
          dplyr::filter(scenario == scen) |>
          sf::st_as_sf(coords = c("x", "y")) |>
          stars::st_rasterize(
            template = stars::read_stars(
              "./data/reference_big.tif"
            ) |>
              rlang::set_names("value") |>
              dplyr::mutate(value = NA_real_)
          ) |>
          rlang::set_names(scen)
      }
    ) |>
    purrr::reduce(c)

  return(list(rf_stars, rf_validation))
}

# -----------------------------------------------------------------------------
# This function creates vizualisations to show the results of the simulations
run_scenarios_results <-
  function(
    scenarios_results,
    year
  ) {
    # Build scenarios table with temperature data
    scenarios_table <- purrr::imap(
      .x = scenarios_results,
      \(m, mid) {
        m[[1]] |>
          tibble::as_tibble() |> # Convert scenario results to tibble
          tidyr::drop_na() |>
          dplyr::mutate(var = mid) |>
          dplyr::inner_join(
            # Join with reference raster IDs
            stars::read_stars(
              "./data/reference_big.tif"
            ) |>
              rlang::set_names("id") |>
              tibble::as_tibble(),
            by = c("x", "y")
          ) |>
          dplyr::slice_sample(prop = 0.5)
      }
    ) |>
      purrr::list_rbind() |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(c("ifwas", "asis")),
        names_to = "scenario",
        values_to = "temperature"
      )

    # Load and prepare merged data for given year
    merged_data <-
      arrow::open_dataset("./data/processed/merged/merged_table.parquet") |>
      dplyr::filter(lubridate::year(time) == {{ year }}) |>
      dplyr::collect() |>
      dplyr::select(dplyr::all_of(c("forest", "scenario_forest", "id"))) |>
      dplyr::rename(asis = forest, ifwas = scenario_forest) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(c("asis", "ifwas")),
        names_to = "scenario",
        values_to = "forest"
      )

    # Scatter plot: forest increase vs temperature reduction
    plot_scatter <- scenarios_table |>
      dplyr::inner_join(merged_data, by = c("id", "scenario")) |> # Merge datasets
      tidyr::pivot_wider(
        names_from = "scenario",
        values_from = c("temperature", "forest")
      ) |>
      dplyr::mutate(
        forest = forest_ifwas - forest_asis, # Forest increase
        temperature = (temperature_ifwas - temperature_asis) * -1 # Temperature reduction
      ) |>
      dplyr::filter(forest > 0.01) |> # Keep meaningful increases
      dplyr::mutate(
        temperature_mean = mean(temperature, na.rm = TRUE),
        .by = "var"
      ) |>
      dplyr::select(
        dplyr::all_of(
          c(
            "x",
            "y",
            "var",
            "temperature",
            "forest",
            "temperature_mean"
          )
        )
      ) |>
      ggplot2::ggplot() +
      ggplot2::facet_wrap(
        ~var,
        labeller = ggplot2::labeller(
          var = c(tmax = "Maximum Temperature", tmean = "Mean Temperature")
        )
      ) +
      ggplot2::geom_point(
        # Scatter points
        ggplot2::aes(x = forest, y = temperature),
        alpha = 0.2
      ) +
      ggplot2::geom_hline(
        # Mean line
        ggplot2::aes(yintercept = temperature_mean),
        linetype = "dashed"
      ) +
      ggplot2::labs(
        y = " Air Temperature Reduction (°C)",
        x = "Forest Increase"
      ) +
      cowplot::theme_half_open(font_size = 11) +
      cowplot::background_grid() +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(
          linetype = "solid",
          linewidth = 0.8,
          color = "#000000"
        ),
        strip.placement = "outside"
      )

    # Ridge plot: distribution of temperature reduction by forest cover increase
    plot_ridges <- scenarios_table |>
      dplyr::inner_join(merged_data, by = c("id", "scenario")) |>
      tidyr::pivot_wider(
        names_from = "scenario",
        values_from = c("temperature", "forest")
      ) |>
      dplyr::mutate(
        forest = forest_ifwas - forest_asis, # Forest increase
        temperature = (temperature_ifwas - temperature_asis) * -1, # Temperature reduction
      ) |>
      dplyr::select(
        dplyr::all_of(
          c(
            "x",
            "y",
            "var",
            "temperature",
            "forest"
          )
        )
      ) |>
      dplyr::group_by(var) |>
      dplyr::mutate(
        # Bin forest increase into fractions
        forest = santoku::chop(
          forest,
          c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
          labels = santoku::lbl_dash(symbol = "-")
        )
      ) |>
      dplyr::ungroup() |>
      ggplot2::ggplot() +
      ggplot2::facet_wrap(
        ~var,
        labeller = ggplot2::labeller(
          var = c(tmax = "Maximum Temperature", tmean = "Mean Temperature")
        )
      ) +
      ggridges::geom_density_ridges_gradient(
        # Ridge density plot
        mapping = ggplot2::aes(
          y = forest,
          x = temperature,
          group = forest,
          fill = ggplot2::after_stat(x)
        ),
        bandwidth = 0.2
      ) +
      scico::scale_fill_scico(palette = "vikO", direction = -1, midpoint = 0) +
      ggplot2::coord_flip(clip = "off") +
      ggplot2::labs(
        x = "Air Temperature Reduction (°C)",
        y = "Forest Fraction Increase"
      ) +
      cowplot::theme_half_open(font_size = 11) +
      cowplot::background_grid(major = "y") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 310, hjust = 0.1),
        legend.position = "none",
        strip.text = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10))
      )

    # Combine scatter and ridge plots vertically
    scenarios_plot <-
      cowplot::plot_grid(
        plot_scatter,
        plot_ridges,
        ncol = 1,
        labels = "auto",
        label_size = 11,
        label_y = c(1, 1.02),
        align = "v",
        rel_heights = c(1, 1.2)
      )

    # Save combined plot
    ggplot2::ggsave(
      "./figures/results/scenarios_result.png",
      scenarios_plot,
      units = "cm",
      width = 15,
      height = 15,
      dpi = 600
    )

    # Create model validation scatterplots
    scenarios_validation_plot <- purrr::imap(
      .x = scenarios_results,
      \(m, mid) {
        m[[2]] |>
          tibble::as_tibble() |> # Convert scenario results to tibble
          tidyr::drop_na() |>
          dplyr::mutate(var = mid)
      }
    ) |>
      purrr::list_rbind() |>
      ggplot2::ggplot() +
      ggplot2::facet_wrap(
        ~var,
        labeller = ggplot2::labeller(
          var = c(tmax = "Maximum Temperature", tmean = "Mean Temperature")
        )
      ) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = original, y = estimates),
        alpha = 0.5
      ) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "#ac6161ff") +
      ggplot2::labs(
        y = "Predicted Air Temperature (°C)",
        x = "Original Air Temperature (°C)"
      ) +
      cowplot::theme_half_open(font_size = 11) +
      cowplot::background_grid(major = "xy") +
      ggplot2::theme(
        strip.background = ggplot2::element_rect(
          linetype = "solid",
          linewidth = 0.8,
          color = "#000000"
        ),
        strip.placement = "outside"
      )

    # Save validation plot
    ggplot2::ggsave(
      "./figures/results/scenarios_validation.png",
      scenarios_validation_plot,
      units = "cm",
      width = 15,
      height = 7,
      dpi = 600
    )
  }

# -----------------------------------------------------------------------------
# This function creates visualizations combining the results from the simulations
# with demographic data
run_population_results <- function(
  scenarios_results,
  year,
  area_of_interest
) {
  # Load population grid shapefile
  population_grid <-
    sf::read_sf("data/raw/population/BR1KM.shp")

  # Load reference raster (used for alignment)
  reference_big <- stars::read_stars("./data/reference_big.tif") |>
    rlang::set_names("id")

  # Transform population grid to match reference raster CRS
  population_grid_sub <- population_grid |>
    sf::st_transform(sf::st_crs(reference_big)) |>
    sf::st_filter(sf::st_bbox(reference_big) |> sf::st_as_sfc()) |>
    janitor::clean_names() |>
    dplyr::select(population = total)

  # Rasterize population grid at 1km resolution and crop to area of interest
  population_stars <- population_grid_sub |>
    stars::st_rasterize(
      template = stars::st_as_stars(
        sf::st_bbox(population_grid_sub),
        dx = 1000,
        dy = 1000
      )
    ) |>
    sf::st_crop(
      y = sf::st_transform(
        sf::read_sf(glue::glue("./data/{area_of_interest}.gpkg")),
        sf::st_crs(population_grid_sub)
      ),
      crop = FALSE
    )

  # Forest change raster (difference between scenarios)
  forest_stars <-
    arrow::open_dataset("./data/processed/merged/merged_table.parquet") |>
    dplyr::filter(lubridate::year(time) == {{ year }}) |>
    dplyr::collect() |>
    dplyr::select(dplyr::all_of(c("forest", "scenario_forest", "id"))) |>
    dplyr::rename(asis = forest, ifwas = scenario_forest) |>
    dplyr::inner_join(
      reference_big |> tibble::as_tibble(),
      by = dplyr::join_by(id)
    ) |>
    dplyr::mutate(forest_change = ifwas - asis) |>
    sf::st_as_sf(coords = c("x", "y")) |>
    dplyr::select(dplyr::all_of("forest_change")) |>
    stars::st_rasterize(template = reference_big, align = TRUE) |>
    stars::st_warp(
      # Align with population raster
      population_stars,
      use_gdal = TRUE,
      method = "med",
      no_data_value = 0
    ) |>
    rlang::set_names("forest_change")

  # Warp temperature scenarios to population grid
  population_temperature <-
    purrr::imap(
      .x = scenarios_results,
      \(m, mid) {
        stars::st_warp(
          m[[1]] |> stars::st_redimension(),
          population_stars,
          use_gdal = TRUE,
          method = "med",
          no_data_value = 0
        ) |>
          split() |>
          rlang::set_names(c("ifwas", "asis")) |>
          stars::st_redimension(name = "scenario")
      }
    ) |>
    purrr::reduce(c) |> # Combine results
    rlang::set_names(c("tmax", "tmean"))

  # Scatter plot: population vs forest change
  forest_plot <-
    c(forest_stars, population_stars) |>
    tibble::as_tibble() |>
    tidyr::drop_na() |>
    dplyr::filter(.data[["population"]] > 0) |>
    ggplot2::ggplot() +
    ggplot2::geom_point(
      ggplot2::aes(y = forest_change, x = population),
      alpha = 0.3
    ) +
    ggplot2::scale_x_continuous(
      transform = scales::log2_trans(), # Log scale for population
      n.breaks = 10
    ) +
    ggplot2::labs(x = "Population", y = "Forest Change") +
    cowplot::theme_half_open(font_size = 11) +
    cowplot::background_grid() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 310, hjust = 0.1)
    )

  # Scatter plot: population vs temperature reduction, colored by forest change
  temperature_plot <-
    c(population_temperature |> split(), population_stars, forest_stars) |>
    tibble::as_tibble() |>
    tidyr::drop_na() |>
    dplyr::filter(.data[["population"]] > 0) |>
    dplyr::mutate(
      tmax = (tmax.ifwas - tmax.asis) * -1, # Temperature reduction
      tmean = (tmean.ifwas - tmean.asis) * -1 # Temperature reduction
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(
          "tmax",
          "tmean",
          "population",
          "forest_change"
        )
      )
    ) |>
    dplyr::slice_sample(prop = 1) |>
    tidyr::pivot_longer(
      cols = dplyr::contains(c("tmax", "tmean")),
      names_to = "var",
      values_to = "temperature"
    ) |>
    # dplyr::mutate(
    #   population = santoku::chop_proportions(
    #     population,
    #     proportions = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
    #   )
    # ) |>
    ggplot2::ggplot() +
    ggplot2::geom_point(
      ggplot2::aes(
        y = temperature,
        x = population,
        color = forest_change
      ),
      alpha = 0.5
    ) +
    ggplot2::facet_grid(
      ~var,
      labeller = ggplot2::labeller(
        var = c(tmax = "Maximum Temperature", tmean = "Mean Temperature")
      )
    ) +
    ggplot2::scale_x_continuous(
      transform = scales::log2_trans(),
      n.breaks = 10
    ) +
    scico::scale_color_scico(palette = "bamako", direction = -1) +
    cowplot::theme_half_open(font_size = 11) +
    cowplot::background_grid() +
    ggplot2::labs(
      y = "Air Temperature Reduction (°C)",
      x = "Population",
      color = "Forest\nChange"
    ) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(
        linetype = "solid",
        linewidth = 0.8,
        color = "#000000"
      ),
      strip.placement = "outside",
      axis.text.x = ggplot2::element_text(angle = 310, hjust = 0.1)
    )

  # Combine scatter plots into one figure
  full_plot <-
    cowplot::plot_grid(
      temperature_plot,
      forest_plot,
      ncol = 1,
      labels = "auto",
      label_size = 11
    )

  # Save combined scatter plots
  ggplot2::ggsave(
    "./figures/results/population_result.png",
    full_plot,
    units = "cm",
    width = 15,
    height = 15,
    dpi = 600
  )

  # Maps: spatial visualization of population, forest change, and temperature change
  maps_plot <-
    c(
      population_temperature |> split(),
      population_stars,
      forest_stars
    ) |>
    stars::st_downsample(n = 20, FUN = mean, na.rm = TRUE) |>
    # sf::st_crop(
    #   y = sf::st_transform(
    #     sf::read_sf(glue::glue("./data/{area_of_interest}.gpkg")),
    #     sf::st_crs(population_grid_sub)
    #   ),
    #   crop = FALSE
    # ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      population = dplyr::if_else(population == 0, NA_real_, population),
      maximum_temperature_change = (tmax.ifwas - tmax.asis),
      mean_temperature_change = (tmean.ifwas - tmean.asis)
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(
          "x",
          "y",
          "maximum_temperature_change",
          "mean_temperature_change",
          "population",
          "forest_change"
        )
      )
    ) |>
    dplyr::slice_sample(prop = 1) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(
        c(
          "maximum_temperature_change",
          "mean_temperature_change",
          "population",
          "forest_change"
        )
      ),
      names_to = "var",
      values_to = "values"
    ) |>
    dplyr::mutate(
      var = forcats::fct_relevel(
        var,
        c(
          "maximum_temperature_change",
          "mean_temperature_change",
          "forest_change",
          "population"
        )
      )
    ) |>
    tidyr::drop_na() |>
    dplyr::group_by(.data[["var"]]) |>
    dplyr::group_map(
      .keep = TRUE,
      ~ {
        if (unique(.x[["var"]]) == "population") {
          fill_pal <- "lajolla"
          dir_pal <- -1
        } else if (unique(.x[["var"]]) == "forest_change") {
          fill_pal <- "bamako"
          dir_pal <- -1
        } else {
          fill_pal <- "vikO"
          dir_pal <- 1
        }

        subplot <-
          ggplot2::ggplot(.x) +
          ggplot2::facet_wrap(
            ggplot2::vars(var),
            ncol = 1,
            nrow = 1,
            labeller = ggplot2::labeller(
              var = \(x) {
                stringr::str_replace_all(x, "_", " ") |>
                  stringr::str_to_title()
              }
            )
          ) +
          ggplot2::geom_tile(
            ggplot2::aes(x = x, y = y, fill = values),
            na.rm = TRUE,
            color = "#000000"
          ) +
          scico::scale_fill_scico(
            palette = fill_pal,
            begin = 0.1,
            end = 0.9,
            na.value = "transparent",
            direction = dir_pal
          ) +
          ggplot2::labs(fill = "") +
          ggplot2::coord_equal() +
          cowplot::theme_map(font_size = 11) +
          cowplot::background_grid(major = "xy") +
          ggplot2::theme(
            legend.position = "bottom",
            axis.text = ggplot2::element_blank(),
            legend.key.height = ggplot2::unit(2, "mm"),
            legend.key.width = ggplot2::unit(10, 'mm'),
            legend.title = ggplot2::element_text(
              margin = ggplot2::margin(r = 15)
            ),
            strip.background = ggplot2::element_rect(
              linetype = "solid",
              linewidth = 0.8,
              color = "#000000"
            )
          )

        # if (unique(.x[["var"]]) == "population") {
        #   subplot <- subplot +
        #     scico::scale_fill_scico(
        #       palette = "vikO",
        #       begin = 0.1,
        #       end = 0.9,
        #       na.value = "transparent",
        #       transform = "log10"
        #     )
        # }

        return(subplot)
      }
    )

  # Combine all maps into one plot
  final_plot <-
    cowplot::plot_grid(
      maps_plot[[1]],
      maps_plot[[2]],
      maps_plot[[3]],
      maps_plot[[4]],
      ncol = 2,
      align = "vh"
    )

  # Save final plot
  ggplot2::ggsave(
    "./figures/results/maps_result.png",
    final_plot,
    units = "cm",
    width = 15,
    height = 12,
    dpi = 600
  )
}
# -----------------------------------------------------------------------------
