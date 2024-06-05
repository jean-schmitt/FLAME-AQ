inmap_health_impacts_f <- function(fleet_elec_use_tot_county, vehicles_emissions_county, fleet_fuel_usage_tot_county, oil_gas_activity, scenario_id = NA) {
  attribute_f("inmap_health_impacts_f")
  results_path <- paste0(getwd(), "/outputs/air_quality/", Sys.Date(), "_", scenario_id, "_results")
  ## Generate electricity production emissions
  print("Generate the electricity production emissions for inmap")
  emissions_electricity <- do.call(inmap_electricity_emissions_f, list(fleet_elec_use_tot_county = fleet_elec_use_tot_county))
  write.csv(emissions_electricity, paste0(results_path, "/emissions_electricity.csv"), row.names = FALSE)
  ## Generate vehicles emissions
  print("Generate the vehicles emissions for inmap")
  emissions_vehicles <- do.call(inmap_vehicles_emissions_f, list(vehicles_emissions_county = vehicles_emissions_county))
  write.csv(emissions_vehicles, paste0(results_path, "/emissions_vehicles.csv"), row.names = FALSE)
  ## Generate refining emissions
  print("Generate the refining emissions for inmap")
  emissions_oil_processing <- do.call(inmap_refining_emissions_f, list(fleet_fuel_usage_tot_county = fleet_fuel_usage_tot_county, oil_gas_activity = oil_gas_activity))
  write.csv(emissions_oil_processing, paste0(results_path, "/emissions_oil_processing.csv"), row.names = FALSE)
  
  ## Exports the input dataset for Inmap
  dir.create(paste0(results_path, "/INMAP_emissions"))
  years <- unique(emissions_vehicles$Year)
  fun <- function(i) {
    st_write(filter(emissions_electricity, emissions_electricity$Year == i), paste0(results_path, "/INMAP_emissions/", "emissions_electricity_", i, ".shp"))
    st_write(filter(emissions_vehicles, emissions_vehicles$Year == i), paste0(results_path, "/INMAP_emissions/", "emissions_vehicles_", i, ".shp"))
    st_write(filter(emissions_oil_processing, emissions_oil_processing$Year == i), paste0(results_path, "/INMAP_emissions/", "emissions_oil_processing_", i, ".shp"))
  }
  lapply(years, fun)
  
  ## Generate configuration files for Inmap
  
  ## Generate the batch file for Inmap
  #init <- "cd C:/Users/Jean Schmitt/inmap/"
  #command <- "inmap.exe run steady --config=nei2005Config.toml"
  
  
  ## Run inmap
  #path <- paste0(getwd(), "Desktop/inmap.bat")
  #print("Starting INMAP")
  #shell(path, wait = TRUE)
  
  # Export the electricity emissions datasets
  #emissions_electricity <- filter(power_plants_emissions_inmap, power_plants_emissions_inmap$Year == 2023) %>%
  #  select(-c("Year"))
  
  #st_write(emissions_electricity, "D:/03_University_of_Toronto/15_High_Resolution_Emissions/02_Inmap_files/emissions_electricity.shp", append = FALSE)
  
  # Test: Export the 2050 file
  #emissions_vehicles <- filter(roads_emissions, roads_emissions$Year == 2050) %>%
  #  select(-c("Year"))
  #colnames(emissions_vehicles) <- c("NOx", "SOx", "PM2_5", "NH3", "VOC", "geometry")
  #st_write(emissions_vehicles, "D:/03_University_of_Toronto/15_High_Resolution_Emissions/02_Inmap_files/emissions_vehicles.shp", append = FALSE)
  
  # Test: Export the 2050 file
  #emissions_refining <- filter(refineries_data, refineries_data$Year == 2050) %>%
  #  select(-c("Year"))
  #colnames(emissions_refining) <- c("NOx", "SOx", "NH3", "PM2_5", "VOC", "geometry")
  #st_write(emissions_refining, "D:/03_University_of_Toronto/15_High_Resolution_Emissions/02_Inmap_files/emissions_refining.shp", append = FALSE)
}
