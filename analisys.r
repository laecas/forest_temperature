# -----------------------------------------------------------------------------
library(geoarrow)
library(rgee)
# -----------------------------------------------------------------------------
source("./R/start_project.R")
source("./R/create_reference_grid.R")
source("./R/handle_lulc.R")
source("./R/handle_elevation.R")
source("./R/handle_temperature.R")
source("./R/handle_precipitation.R")
source("./R/handle_population.R")
source("./R/handle_scenario.R")
source("./R/merge_data.R")
source("./R/run_exploratory_analysis.R")
source("./R/run_spatial_sampling.R")
source("./R/run_causal_analysis.R")
source("./R/run_scenarios_analysis.R")
# -----------------------------------------------------------------------------
# START PROJECT
# Create the folders and set the CRS of the project
start_project()
# Create two reference grids for the project
# In this project, the area of interest in inside a big portion of
# the state of São Paulo (Brazil), so the reference grids are created based on
# this area of interest.
create_reference_grid(big = 900, small = 90, area_of_interest = "sp")
# -----------------------------------------------------------------------------
# DOWNLOAD DATA
# Download mapbiomas data from 2001 to 2020
download_mapbiomas(years = c(2001L:2020L), area_of_interest = "sp")
# Download the legend of the mapbiomas land cover data
download_mapbiomas_legenda()
# Download the monthly mean air temperature of GSHTD, from 2001 to 2020
# GSHTD = Global Seamless High-resolution Temperature Dataset
# (https://doi.org/10.1016/j.rse.2022.113422)
download_temperature_gshtd(
  var = "mean",
  years = 2001L:2020L,
  area_of_interest = "sp"
)
# Download the monthly maximum air temperature of GSHTD, from 2001 to 2020
download_temperature_gshtd(
  var = "max",
  years = 2001L:2020L,
  area_of_interest = "sp"
)
# Download stations meteorological data from NCEI
download_temperature_ncdc(years = 2001L:2020L)
# Download elevation data from Copernicus
download_elevation(area_of_interest = "sp")
# Download precipitation data from CHIRPS
download_precipitation(years = 2001L:2020L)
# Download population data from IBGE
download_population()
# -----------------------------------------------------------------------------
# PROCESS DATA
# Process mapbiomas data
# (downscaling the data when reading by 3 times its original resolution)
process_mapbiomas(
  scale_factor = 3,
  years = 2001L:2020L,
  area_of_interest = "sp"
)
# Process GSHTD mean temperature data for the hottest months
# (January to March)
process_temperature_gshtd(
  temperature_metric = "mean",
  years = 2001L:2020L,
  months = 1L:3L,
  area_of_interest = "sp"
)
# Process GSHTD maximum temperature data for the hottest months
# (January to March)
process_temperature_gshtd(
  temperature_metric = "max",
  years = 2001L:2020L,
  months = 1L:3L,
  area_of_interest = "sp"
)
# Process meteorological stations data
process_temperature_ncdc(months = 1L:3L, area_of_interest = "sp")
# Process elevation data
process_elevation(area_of_interest = "sp")
# Process CHIRPS precipitation data
process_precipitation(
  years = 2001L:2020L,
  months = 1L:3L,
  area_of_interest = "sp"
)
# Create forest cover compliance scenario
create_scenario(scenario_year = 2020L, scale_factor = 3L)
# Merge raster data into one table
merge_data()
# -----------------------------------------------------------------------------
# RUN EXPLORATORY ANALYSIS
# Create maps and timeseries
describe_tempspat(
  vars_lab = "Air Temperature",
  vars = c("temperature_max", "temperature_mean"),
  density_curve_kernel = 1
)
describe_tempspat(
  vars_lab = "Land Cover Proportion",
  vars = c("forest", "urban", "agriculture", "pasture", "mosaic"),
  density_curve_kernel = 0.01
)
describe_tempspat(
  vars_lab = "Precipitation",
  vars = c("precipitation"),
  density_curve_kernel = 50
)
describe_tempspat(
  vars_lab = "Elevation",
  vars = c("elevation"),
  density_curve_kernel = 100
)
# Describe the relationship between Mapbiomas lulc and GSHTD temperature
describe_lulc_temperature()
# -----------------------------------------------------------------------------
# RUN STATISTICAL ANALYSIS
# Perform data sampling
data_samples <- create_spatial_samples(
  area_of_interest = "sp",
  n_samples = 5
)
data_samples <-
  sf::read_sf("./data/spatial_data_sample_split.gpkg")
# Calculate the effect of forests over temperature
effect_results_tmax <- calc_effect(
  variable = "temperature_max",
  year = 2020
)
effect_results_tmean <- calc_effect(
  data_sample = data_samples,
  variable = "temperature_mean",
  year = 2020
)
# Store effect results
effect_results_list <- list(
  "tmax" = effect_results_tmax,
  "tmean" = effect_results_tmean
)
readr::write_rds(
  x = effect_results_list,
  file = "./data/results/effect_results.rds"
)
# Open effect results
effect_results_list <-
  readr::read_rds(
    file = "./data/results/effect_results.rds"
  )
# Create effects visualizations
run_effect_results(model_results = effect_results_list)
# Calculate the impact of environmental compliance over the temperature
scenarios_results_tmax <- calc_scenarios(
  year = 2020,
  data_sample = data_samples,
  variable = "temperature_max"
)
scenarios_results_tmean <- calc_scenarios(
  year = 2020,
  data_sample = data_samples,
  variable = "temperature_mean"
)
# Store simulations results
scenarios_results_list <- list(
  "tmax" = scenarios_results_tmax,
  "tmean" = scenarios_results_tmean
)
readr::write_rds(
  x = scenarios_results_list,
  file = "./data/results/scenarios_results.rds"
)
# Open simulations results
scenarios_results_list <-
  readr::read_rds(
    file = "./data/results/scenarios_results.rds"
  )
# Create simulations visualizations
run_scenarios_results(
  scenarios_results = scenarios_results_list,
  year = 2020
)
run_population_results(
  scenarios_results = scenarios_results_list,
  year = 2020,
  area_of_interest = "sp"
)
# -----------------------------------------------------------------------------
# RUN VALIDATION OF GSHTD DATA
temperature_scatterplot()
temperature_timeseries()
# -----------------------------------------------------------------------------
