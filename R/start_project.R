start_project <-
  function() {

    # Create data dirs and files
    fs::dir_create("./data/processed/")
    fs::dir_create("./data/merged/")
    fs::dir_create("./data/raw/")
    fs::dir_create("./data/validation/")

    readr::write_file(
      x = glue::glue(
        'PROJCS["Brazil Data Cube",',
        'GEOGCS["unknown",',
        'DATUM["Unknown based on GRS80 ellipsoid",',
        'SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]]],',
        'PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],',
        'UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]]],',
        'PROJECTION["Albers_Conic_Equal_Area"],',
        'PARAMETER["latitude_of_center",-12],',
        'PARAMETER["longitude_of_center",-54],',
        'PARAMETER["standard_parallel_1",-2],',
        'PARAMETER["standard_parallel_2",-22],',
        'PARAMETER["false_easting",5000000],',
        'PARAMETER["false_northing",10000000],',
        'UNIT["metre",1,AUTHORITY["EPSG","9001"]],',
        'AXIS["Easting",EAST],AXIS["Northing",NORTH]]'
      ),
      file = "./data/project_crs.txt"
    )

    # Create figures dirs
    fs::dir_create("./figures/exploration/")
    fs::dir_create("./figures/methods/")
    fs::dir_create("./figures/results/")
    fs::dir_create("./figures/validation/")

  }