# -----------------------------------------------------------------------------
# This function downloads population data from IBGE
download_population <-
  function() {
    # Download population data, and store it in temporary directory
    temp_file <-
      httr2::request(
        base_url = "https://geoftp.ibge.gov.br/recortes_para_fins_estatisticos/"
      ) |>
      httr2::req_url_path_append(
        "grade_estatistica/censo_2022/grade_1km/BR1KM_20251002.zip"
      ) |>
      httr2::req_perform(path = fs::file_temp())

    # Extract files to project directory
    archive::archive_extract(
      temp_file$body[[1]],
      dir = "./data/raw/population/"
    )
  }
