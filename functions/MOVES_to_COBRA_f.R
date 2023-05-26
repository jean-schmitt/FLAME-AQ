MOVES_to_COBRA_f <- function() {
  # Set the scenario parameters
  scenario_id <- "default"
  previous_year <- 2045
  year <- 2050
  first_file <- FALSE
  # Load the corresponding data file
  if (isTRUE(first_file)) {
    cobra_data <- read.csv("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/06_COBRA_Files/COBRA_scenario_template.csv")
  } else {
    cobra_data <- read.csv(paste0("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/06_COBRA_Files/COBRA_scenario_", scenario_id, "_", previous_year, ".csv"))
    
  }
  # Calculate the changes in emissions
  temp <- which(cobra_data$TIER1NAME == "Highway Vehicles" & cobra_data$TIER3NAME == "Light Duty")
  SO2_change_CTRL <- 1.007
  NO2_change_CTRL <- 0.930
  PM25_change_CTRL <- 1.007
  # Change the reference data
  if (!isTRUE(first_file)) {
    cobra_data$BASE_NO2[temp] <- cobra_data$CTRL_NO2[temp]
    cobra_data$BASE_SO2[temp] <- cobra_data$CTRL_SO2[temp]
    cobra_data$BASE_PM25[temp] <- cobra_data$CTRL_PM25[temp]
  }
  # Change the scenario data for the correesponding year
  cobra_data$CTRL_NO2[temp] <- cobra_data$BASE_NO2[temp]*NO2_change_CTRL
  cobra_data$CTRL_SO2[temp] <- cobra_data$BASE_SO2[temp]*SO2_change_CTRL
  cobra_data$CTRL_PM25[temp] <- cobra_data$CTRL_PM25[temp]*PM25_change_CTRL
  write.csv(cobra_data, paste0("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/06_COBRA_Files/COBRA_scenario_", scenario_id, "_", year, ".csv"))
  
  #Script to run COBRA using windows cmd
  # Generate .bat files //Add the correct year to the files and generate baseline and scenario files for each year // Add corresponding population data
  first_year <- 2022
  last_year <- 2025
  for (i in first_year:last_year) {
    executable_path <- '"C:/Program Files/COBRA/cobra_console.exe"'
    db_path <- '"C:/Program Files/COBRA/data/cobra.db"'
    A <- '"D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_Updated_FLAME/inputs/air_quality/COBRA_emissions_baseline.csv"'
    B <- '"D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_Updated_FLAME/inputs/air_quality/COBRA_emissions_scenario.csv"'
    C <- '"D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_Updated_FLAME/inputs/air_quality/COBRA_default_population_data.csv"'
    D <- '"D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_Updated_FLAME/inputs/air_quality/COBRA_default_incidence_data.csv"'
    E <- paste0("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_Updated_FLAME/outputs/air_quality/COBRA_results_", i, ".csv")
    F <- "YES"
    G <- 2028
    command <- paste0(executable_path, ' -d ', db_path, ' -b', A, ' -c ', B, ' ', C, ' ', D, ' -o ', E, ' --pct3 ', F, ' -y ', G)
    if (i == first_year) {
      write(command, file = "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_Updated_FLAME/outputs/air_quality/COBRA.bat")
    } else {
      write(command, file = "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_Updated_FLAME/outputs/air_quality/COBRA.bat", append = TRUE)
    }
  }
  path <- "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_Updated_FLAME/outputs/air_quality/COBRA.bat"
  shell.exec(path)
  
  # Import all generated csv files from COBRA and calculate the cumulative benefits
  
  
  
  
  
  
  
  
  termId <- rstudioapi::terminalCreate(show = FALSE)
  terminalExecute(command)
  while (rstudioapi::terminalBusy(termId)) {
    Sys.sleep(0.1)
  }
  print("Terminal available")
  
  # Alternative option to run the files: generate a bat file that will be run manually or through R / all required calculations (each year) can be loaded into the .bat file
  path <- "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_Updated_FLAME/outputs/air_quality/test.bat"
  shell.exec(path)
}
