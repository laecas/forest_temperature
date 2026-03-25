AUTHOR: Hugo Tameirão Seixas
CONTACT: seixas.hugo@protonmail.com

DESCRIPTION: Code to run the analysis for the paper (). 
	The "data" directory is where the data used to run the analyses are stored.
	The "figures" directory contains all the figures that shows the results of the analyses.
	The "R" directory contains the functions to run the analyses.
	The "analysis.R" file contains the code to run the functions and perform the analysis.

OBSERVATIONS: On how to download data to perform the analyses:
	Most data can be downloaded automatically by running the download functions in the "analysis.r" script file.
	Only the RLs and APPs data must be downloaded manually at the SICAR website (https://consultapublica.car.gov.br/publico/estados/downloads), and downloading the APPs and RLs of the São Paulo state. After the download, you must extract all the downloaded shapefiles in the directories: "./data/raw/permanent_protection_area/" (for APPs) and  "./data/raw/legal_reserve/" (for RLs).

R VERSION: 4.5.2

PACKAGES: (name of package)-(package version)
	aorsf-0.1.6
	archive-1.1.12.1
	arrow-23.0.0.1
	broom-1.0.12
	cowplot-1.2.0
	curl-7.0.0
	dplyr-1.2.0
	forcats-1.0.1
	fs-1.6.6
	geoarrow-0.4.2
	geojsonio-0.11.3.9000
	ggplot2-4.0.2
	ggridges-0.5.7
	glue-1.8.0
	googledrive-2.1.2
	grf-2.5.0
	here-1.0.2
	httr2-1.2.2
	janitor-2.2.1
	lubridate-1.9.5
	purrr-1.2.1
	readr-2.1.6
	rgee-1.1.8
	rlang-1.1.7
	rvest-1.0.5
	santoku-1.1.0
	scales-1.4.0
	scico-1.5.0
	sf-1.0-24
	spatialsample-0.6.1
	stars-0.7-0
	stringr-1.6.0
	tibble-3.3.1
	tidyr-1.3.2
	utils-4.5.2
