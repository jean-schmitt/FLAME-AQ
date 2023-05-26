air_quality_module_f <- function(fleet_composition_state_breakdown, fleet_composition, fleet_elec_use_tot_state, fleet_elec_use_tot_county, fleet_fuel_use_tot, fleet_fuel_use_tot_state, scenario_id = NA, brake_tire_emissions_modification = NA, fuel_matching_option = NA, fleet_electricity_consumption_source = NA, first_proj_yr = NA, fleet_id = NA, last_yr = NA) {
  attribute_f("air_quality_module_f")
  results_path <- paste0(getwd(), "/outputs/air_quality/", Sys.Date(), "_", scenario_id, "_results")
  # Modify the fleet composition files to MOVES inputs
  fleet_dataset <- fleet_composition_state_breakdown[["fleet_vint_stock_scenario_state_breakdown"]]
  fleet_size <- total_fleet_size_f(fleet_dataset)
  # Adapt the fuel and engine types according to the chosen matching option
  if (fuel_matching_option == "hybrid") {
    correspondence_file <- read.delim(paste0(getwd(), "/inputs/data/FLAME_to_MOVES_correspondance_files/technologies_", fuel_matching_option, ".txt"), header = TRUE, sep = ";", dec = ".")
  } else if (fuel_matching_option == "FLAME") {
    
  } else if (fuel_matching_option == "MOVES") {
    
  }
  for (i in 1:length(correspondence_file$ID)) {
    fleet_dataset$Technology[which(fleet_dataset$Technology == correspondence_file$Technology[i])] <- correspondence_file$ID[i]
  }
  
  # Adapt the naming of vehicles sizes
  vehicles_sizes_file <- read.delim(paste0(getwd(), "/inputs/data/FLAME_to_MOVES_correspondance_files/vehicles_sizes.txt"), header = TRUE, sep = ";", dec = ".")
  for (i in 1:length(vehicles_sizes_file$Sizes)) {
    fleet_dataset$Size[which(fleet_dataset$Size == vehicles_sizes_file$Sizes[i])] <- vehicles_sizes_file$ID[i]
    colnames(fleet_size)[which(colnames(fleet_size) == vehicles_sizes_file$Sizes[i])] <- vehicles_sizes_file$ID[i]
  }
  # Adapt the file structure
  fleet_dataset <- fleet_dataset %>%
    #add_column("Model_Year" = fleet_dataset$Year - fleet_dataset$Age) %>%
    relocate("Year", .after = "Age") %>%
    relocate("Size", .after = "Year") %>%
    relocate("Size", .after = "Year") %>%
    relocate("Technology", .after = "Size") %>%
    relocate("Model_Year", .after = "Technology") %>%
    relocate("State", .before = "Age")
  names <- colnames(fleet_dataset)
  fleet_dataset  <- aggregate(fleet_dataset$Value, by = list(fleet_dataset$State, fleet_dataset$Age, fleet_dataset$Year, fleet_dataset$Size, fleet_dataset$Technology, fleet_dataset$Model_Year), FUN = sum)
  colnames(fleet_dataset) <- names
  # Generate the emissions for the complete fleet and the breakdown by state
  normalized_total_emissions <- get_input_f(input_name = 'normalized_total_emissions')
  for (i in 5:15) {
    normalized_total_emissions[,i] <- normalized_total_emissions[,i]/1E6
  }
  normalized_emissions_by_state <- read.csv(paste0(getwd(), "/inputs/air_quality/normalized_emissions_by_state.csv"))
  if (fleet_id == "2020_fleet") {
    normalized_total_emissions <- filter(normalized_total_emissions, normalized_total_emissions$Year <= min(normalized_total_emissions$Year))
    normalized_emissions_by_state <- filter(normalized_emissions_by_state, normalized_emissions_by_state$Year <= min(normalized_emissions_by_state$Year))
    years <- 2020:last_yr
    normalized_total_emissions_temp <- filter(normalized_total_emissions, normalized_total_emissions$Year == min(normalized_total_emissions$Year))
    normalized_emissions_by_state_temp <- filter(normalized_emissions_by_state, normalized_emissions_by_state$Year == min(normalized_emissions_by_state$Year))
    for (i in years) {
      normalized_total_emissions_temp$Year <- i
      normalized_emissions_by_state_temp$Year <- i
      normalized_total_emissions <- rbind(normalized_total_emissions, normalized_total_emissions_temp)
      normalized_emissions_by_state <- rbind(normalized_emissions_by_state, normalized_emissions_by_state_temp)
    }
  }
  total_emissions_fleet <- normalized_total_emissions
  print("Modification of brake and tire wear coefficients")
  if (brake_tire_emissions_modification != "default") {
    modified_emissions_factors <- do.call(modify_brake_tire_wear_f, list(normalized_total_emissions, normalized_emissions_by_state))
    normalized_total_emissions <- modified_emissions_factors[["normalized_total_emissions"]]
    normalized_emissions_by_state <- modified_emissions_factors[["normalized_emissions_by_state"]]
  }
  m <- 1
  print(paste0("Calculation of the fleet emissions using the scenario: ", scenario_id))
  for (i in 1:dim(normalized_total_emissions)[1]) {
    temp_emissions <- normalized_emissions_by_state[which(normalized_emissions_by_state$Year == normalized_total_emissions$Year[i] & 
                                                      normalized_emissions_by_state$Source == normalized_total_emissions$Source[i] &
                                                      normalized_emissions_by_state$Fuel == normalized_total_emissions$Fuel[i] &
                                                      normalized_emissions_by_state$ModelYr == normalized_total_emissions$ModelYr[i]),]
    temp_vehicle_population <- fleet_dataset[which(fleet_dataset$Year == normalized_total_emissions$Year[i] &
                                                   fleet_dataset$Size == normalized_total_emissions$Source[i] & 
                                                   substr(fleet_dataset$Technology,1,1) == normalized_total_emissions$Fuel[i] &
                                                   fleet_dataset$Model_Year == normalized_total_emissions$ModelYr[i]),]
    temp_emissions <- arrange(temp_emissions, temp_emissions$State)
    temp_state_emissions <- temp_emissions
    temp_vehicle_population <- arrange(temp_vehicle_population, temp_vehicle_population$State)
    for (j in 8:length(colnames(temp_state_emissions))) {
      temp_state_emissions[,j] <- temp_emissions[,j]*temp_vehicle_population$Value
      total_emissions_fleet[i,j-2] <- normalized_total_emissions[i,j-2]*sum(temp_vehicle_population$Value)
    }
    if (m == 1) {
      total_emissions_fleet_state_breakdown <- temp_state_emissions
    } else {
      total_emissions_fleet_state_breakdown <- rbind(total_emissions_fleet_state_breakdown, temp_state_emissions)
    }
    m <- m+1
  }
  # Summarize the emissions by year and export the files
  fleet_direct_emissions <- select(total_emissions_fleet, -c("Source", "Fuel", "ModelYr")) %>%
    add_column("Unit" = "tons")
  fleet_direct_emissions <- aggregate(cbind(NO2, NO, N2O, NOx, Total_PM10, Total_PM25, Brake_PM10, Tire_PM10, Brake_PM25, Tire_PM25, SO2) ~ Year+Unit, data = fleet_direct_emissions, FUN = sum, na.rm = TRUE)
  fleet_direct_emissions_state_breakdown <- select(total_emissions_fleet_state_breakdown, -c("Source", "Fuel", "ModelYr")) %>%
    add_column("Unit" = "tons")
  fleet_direct_emissions_state_breakdown <- aggregate(cbind(NO2, NO, N2O, NOx, Total_PM10, Total_PM25, Brake_PM10, Tire_PM10, Brake_PM25, Tire_PM25, SO2) ~ Year+State+Unit, data = fleet_direct_emissions_state_breakdown, FUN = sum, na.rm = TRUE)
  write.csv(fleet_direct_emissions, paste0(results_path, "/emissions_fleet.csv"))
  write.csv(fleet_direct_emissions_state_breakdown, paste0(results_path, "/emissions_fleet_state_breakdown.csv"))
  
  # Run COBRA and generate the output files containing the cumulative health impacts at the national and state scales
  state_resolved_fleet_direct_emissions <- fleet_direct_emissions_state_breakdown
  print("Air pollutants emissions dataset created")
  print("Calculation of the electricity usage from the additional electric vehicles")
  if (fleet_electricity_consumption_source == "MOVES") {
    fleet_emissions_elec_state <- do.call(electric_grid_emissions_f, list(fleet_composition = fleet_composition, fleet_elec_use_tot_state=fleet_elec_use_tot_state, fleet_elec_use_tot_county = fleet_elec_use_tot_county))[["electricity_consumption"]]
  } else if (fleet_electricity_consumption_source == "FLAME") {
    fleet_emissions_elec_state <- do.call(electric_grid_emissions_f, list(fleet_composition = fleet_composition, fleet_elec_use_tot_state=fleet_elec_use_tot_state, fleet_elec_use_tot_county = fleet_elec_use_tot_county))[["fleet_emissions_elec_state"]]
    fleet_emissions_elec_county <- do.call(electric_grid_emissions_f, list(fleet_composition = fleet_composition, fleet_elec_use_tot_state=fleet_elec_use_tot_state, fleet_elec_use_tot_county = fleet_elec_use_tot_county))[["fleet_emissions_elec_county"]]
  }
  print("Calculation of the well-to-pump emissions from liquid and gaseous fuel production")
  fleet_fuel_usage_US <- do.call(fuel_wtp_ef_f, list(fleet_fuel_use_tot = fleet_fuel_use_tot, fleet_fuel_use_tot_state = fleet_fuel_use_tot_state))[["production_change_matrix"]]
  print("Starting the calculation of health impacts")
  health_impacts <- do.call(cobra_health_impact_f, list(state_resolved_fleet_direct_emissions = state_resolved_fleet_direct_emissions, 
                                                        fleet_emissions_elec_state = fleet_emissions_elec_state,
                                                        fleet_emissions_elec_county = fleet_emissions_elec_county,
                                                        fleet_fuel_usage_US = fleet_fuel_usage_US))
  return(health_impacts)
}
