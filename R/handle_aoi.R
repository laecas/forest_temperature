download_municipality <-
  function(
    muni,
    muni_file = NA_character_,
    state = NA_character_
  ) {

    # Set working directory
    wdir <- here::here()

    # Set municipality option to title case
    muni <- stringr::str_to_title(muni)

    # Set file name
    if (is.na(muni_file)) {
      muni_file <- muni
    }

    # Set list of municipalities
    muni_list <- glue::glue("'{muni}'") |>
      stringr::str_c(collapse = ',')

    base_dir <-
      glue::glue(
        "{wdir}/data"
      )

    # Create temporary directory
    file_dir <- tempdir()
    fs::dir_create(glue::glue("{tempdir()}/muni"))
    file_dir <- glue::glue("{tempdir()}/muni")

    # Crate path for downloaded data
    file_path <- glue::glue("{file_dir}/muni.zip")

    # Create path to the downloaded data to be saved
    dest_path <-
      glue::glue("{base_dir}/{stringr::str_to_lower(muni_file)}.gpkg")

    data_url <-
      glue::glue(
        "https://geoftp.ibge.gov.br/organizacao_do_territorio/",
        "malhas_territoriais/malhas_municipais/municipio_2022/",
        "Brasil/BR/BR_Municipios_2022.zip"
      )

    # Download data ----
    # Create curl handle
    h <- curl::new_handle()

    curl::handle_setopt(
      h,
      ssl_verifypeer = FALSE
    )

    # Download data to temporary dir
    curl::curl_download(
      url = glue::glue(data_url),
      destfile = file_path,
      handle = h
    )

    # Extract compressed files
    if (stringr::str_detect(file_path, ".zip")) {

      utils::unzip(
        zipfile = file_path,
        exdir = glue::glue("{file_dir}/")
      )

      # Get path of the downloaded data
      file_path <-
        fs::dir_ls(
          path = glue::glue("{file_dir}/"),
          recurse = TRUE,
          regexp = "BR_Municipios_2022.shp"
        )

    }

    # Get data layer name
    layer_name <- sf::st_layers(file_path[1])[[1]]

    # Filter municipality from data
    spatial_data <-
      sf::read_sf(
        file_path[1],
        query = glue::glue(
          "SELECT * FROM \"{layer_name}\" WHERE NM_MUN in ({muni_list})"
        )
      ) |>
      janitor::clean_names() |>
      sf::st_transform(
        sf::st_crs(readr::read_file("./data/project_crs.txt"))
      )

    if (!is.na(state)) {
      spatial_data <-
        spatial_data |>
        dplyr::filter(
          .data$sigla_uf == state
        )
    }

    # Save data
    sf::write_sf(
      obj = spatial_data,
      layer = stringr::str_to_lower(muni_file),
      dsn = dest_path
    )

    return(invisible(NULL))

  }

download_biome <-
  function(
    biome = "Mata Atlântica",
    biome_file = "mata_atlantica"
  ) {

    base_dir <-
      glue::glue(
        "./data"
      )

    # Create temporary directory
    file_dir <- tempdir()
    fs::dir_create(glue::glue("{tempdir()}/biome"))
    file_dir <- glue::glue("{tempdir()}/biome")

    # Crate path for downloaded data
    file_path <- glue::glue("{file_dir}/biome.zip")

    # Create path to the downloaded data to be saved
    dest_path <-
      glue::glue("{base_dir}/{stringr::str_to_lower(biome_file)}.gpkg")

    data_url <-
      glue::glue(
        "https://geoftp.ibge.gov.br/informacoes_ambientais/",
        "estudos_ambientais/biomas/vetores/Biomas_250mil.zip"
      )

    # Download data ----
    # Create curl handle
    h <- curl::new_handle()

    curl::handle_setopt(
      h,
      ssl_verifypeer = FALSE
    )

    # Download data to temporary dir
    curl::curl_download(
      url = glue::glue(data_url),
      destfile = file_path,
      handle = h
    )

    # Extract compressed files
    if (stringr::str_detect(file_path, ".zip")) {

      utils::unzip(
        zipfile = file_path,
        exdir = glue::glue("{file_dir}/")
      )

      # Get path of the downloaded data
      file_path <-
        fs::dir_ls(
          path = glue::glue("{file_dir}/"),
          recurse = TRUE,
          regexp = "lm_bioma_250.shp"
        )

    }

    # Get data layer name
    layer_name <- sf::st_layers(file_path[1])[[1]]

    # Filter municipality from data
    spatial_data <-
      sf::read_sf(file_path[1]) |>
      janitor::clean_names() |>
      dplyr::filter(bioma == biome) |>
      sf::st_transform(
        sf::st_crs(readr::read_file("./data/project_crs.txt"))
      )

    # Save data
    sf::write_sf(
      obj = spatial_data,
      layer = stringr::str_to_lower(biome_file),
      dsn = dest_path
    )

    return(invisible(NULL))

  }
