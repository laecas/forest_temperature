source("./R/start_project.R")
source("./R/create_reference_grid.R")
source("./R/handle_aoi.R")
source("./R/handle_lulc.R")
source("./R/handle_elevation.R")
source("./R/handle_temperature.R")
source("./R/handle_precipitation.R")
source("./R/merge_data.R")
source("./R/run_statistics.R")
source("./R/run_validation.R")
source("./R/run_exploration.R")

# START PROJECT
start_project()

create_reference_grid()

# DOWNLOAD DATA

download_municipality(
  muni = c(
    "caçapava", "igaratá", "jacareí", "jambeiro", "monteiro lobato",
    "paraibuna", "santa branca", "são josé dos campos", "campos do jordão",
    "lagoinha", "natividade da serra", "pindamonhangaba", "redenção da serra",
    "santo antônio do pinhal", "são bento do sapucaí", "são luiz do paraitinga",
    "taubaté", "tremembé", "aparecida", "cachoeira paulista", "canas", "cunha",
    "guaratinguetá", "lorena", "piquete", "potim", "roseira", "arapeí",
    "areias", "bananal", "cruzeiro", "lavrinhas", "queluz",
    "são josé do barreiro", "silveiras"
  ),
  muni_file = "vale_paraiba",
  state = "SP"
)

download_mapbiomas()

download_mapbiomas_legenda()

download_temperature_gshtd(var = "mean")

download_temperature_gshtd(var = "max", years = 2009:2020)

download_temperature_ncdc()

download_elevation()

download_precipitation()


# PROCESS DATA

process_mapbiomas()

process_temperature_gshtd(temperature_metric = "mean")

process_temperature_gshtd(temperature_metric = "max")

process_elevation()

process_precipitation()

analysis_merge_data()

analysis_merge_tif()


# RUN EXPLORATORY ANALYSIS

describe_hist(
  vars_lab = "Temperature",
  vars = c("temperature_mean", "temperature_max"),
  distr = sgt::dsgt,
  distr_args = list(
    c(mu = 22, sigma = 1.5, lambda = 0.2),
    c(mu = 27, sigma =  1.5, lambda = 0.1)
  )
)

describe_hist(
  vars_lab = "Land Cover Proportion",
  vars = c("forest", "urban", "agriculture"),
  distr = dbeta,
  distr_args = list(
    c(shape1 = 0.2, shape2 = 0.7),
    c(shape1 = 0.02, shape2 = 1),
    c(shape1 = 0.05, shape2 = 1)
  )
)

describe_hist(
  vars_lab = "Elevation",
  vars = c("elevation"),
  distr = sgt::dsgt,
  distr_args = list(
    c(mu = 900, sigma = 280, lambda = 0.8)
  )
)

describe_hist(
  vars_lab = "Precipitation",
  vars = c("precipitation"),
  distr = sgt::dsgt,
  distr_args = list(
    c(mu = 650, sigma = 150, lambda = 0.1)
  )
)

describe_tempspat(
  area_of_interest = "vale_paraiba",
  vars_lab = "Temperature",
  vars = c("temperature_max", "temperature_mean")
)

describe_tempspat(
  area_of_interest = "vale_paraiba",
  vars_lab = "Land Cover Proportion",
  vars = c("forest", "urban", "agriculture")
)

describe_tempspat(
  area_of_interest = "vale_paraiba",
  vars_lab = "Precipitation",
  vars = c("precipitation")
)

describe_tempspat(
  area_of_interest = "vale_paraiba",
  vars_lab = "Elevation",
  vars = c("elevation")
)


# RUN STATISTICAL ANALYSIS

lm_direct_effect()

rf_direct_effect()


# RUN VALIDATION

temperature_scatterplot()

temperature_timeseries()
