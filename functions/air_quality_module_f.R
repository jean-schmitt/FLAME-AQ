air_quality_module_f <- function(fleet_composition_state_breakdown, fleet_composition, fleet_elec_use_tot_state, fleet_elec_use_tot_county, fleet_fuel_use_tot, fleet_fuel_use_tot_state, scenario_id = NA, brake_tire_emissions_modification = NA, fuel_matching_option = NA, fleet_electricity_consumption_source = NA, first_proj_yr = NA, fleet_id = NA, last_yr = NA) {
  attribute_f("air_quality_module_f")
  results_path <- paste0(getwd(), "/outputs/air_quality/", Sys.Date(), "_", scenario_id, "_results")
  normalized_total_emissions <- get_input_f(input_name = 'normalized_total_emissions')
  fleet_dataset <- fleet_composition_state_breakdown[["fleet_vint_stock_scenario_state_breakdown"]]
  #write.csv(fleet_dataset, paste0(results_path, "/TEMP_fleet_dataset_input.csv"))
  fleet_size <- total_fleet_size_f(fleet_dataset)
  normalized_emissions_by_state <- read.csv(paste0(getwd(), "/inputs/air_quality/normalized_emissions_by_state.csv"))
  total_emissions_fleet <- normalized_total_emissions
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
  print("Modification of brake and tire wear coefficients")
  #if (brake_tire_emissions_modification != "default") {
  modified_emissions_factors <- do.call(modify_brake_tire_wear_f, list(normalized_total_emissions, normalized_emissions_by_state))
  normalized_total_emissions <- modified_emissions_factors[["normalized_total_emissions"]]
  normalized_emissions_by_state <- modified_emissions_factors[["normalized_emissions_by_state"]]
  #}
  print(paste0("Calculation of the fleet emissions using the scenario: ", scenario_id))
  evaluation <- 1:dim(normalized_total_emissions)[1]
  normalized_total_emissions <- add_column(normalized_total_emissions, "Vehicles_population" = 0)
  normalized_emissions_by_state <- add_column(normalized_emissions_by_state, "Vehicles_population" = 0)
  fleet_dataset <- filter(fleet_dataset, fleet_dataset$Year >= 2021)
  #normalized_emissions_by_state <- filter(normalized_emissions_by_state, normalized_emissions_by_state$State == 1)
  fleet_dataset$Technology <- substr(fleet_dataset$Technology,1,1)
  fleet_dataset <- aggregate(Value ~ Age+Year+Size+Technology+State+Model_Year, data = fleet_dataset, FUN = sum)
  fleet_dataset <- add_column(fleet_dataset, "Order_key" = NA)
  fleet_dataset$Order_key <- as.numeric(paste0(fleet_dataset$State, fleet_dataset$Year, fleet_dataset$Size, fleet_dataset$Technology, fleet_dataset$Model_Year))
  fleet_dataset <- arrange(fleet_dataset, Order_key)
  normalized_emissions_by_state <- add_column(normalized_emissions_by_state, "Order_key" = NA)
  normalized_emissions_by_state$Order_key <- as.numeric(paste0(normalized_emissions_by_state$State, normalized_emissions_by_state$Year, normalized_emissions_by_state$Source, normalized_emissions_by_state$Fuel, normalized_emissions_by_state$ModelYr))
  normalized_emissions_by_state <- arrange(normalized_emissions_by_state, Order_key)
  plan(multisession)
  data <- future_lapply(evaluation, function(i) {
    temp <- which(normalized_emissions_by_state$Year == normalized_total_emissions$Year[i] &
                    normalized_emissions_by_state$Source == normalized_total_emissions$Source[i] &
                    normalized_emissions_by_state$Fuel == normalized_total_emissions$Fuel[i] &
                    normalized_emissions_by_state$ModelYr == normalized_total_emissions$ModelYr[i])
    
    population <- fleet_dataset$Value[Reduce(intersect, list(which(fleet_dataset$Year == normalized_total_emissions$Year[i]),
                                                             which(fleet_dataset$Size == normalized_total_emissions$Source[i]), 
                                                             which(fleet_dataset$Technology == normalized_total_emissions$Fuel[i]), 
                                                             which(fleet_dataset$Model_Year == normalized_total_emissions$ModelYr[i])))]
    return (list(temp = temp, population = population))
  })
  fleet_dataset <- arrange(fleet_dataset, Order_key)
  normalized_emissions_by_state <- arrange(normalized_emissions_by_state, Order_key)
  for (i in 1:dim(normalized_total_emissions)[1]) {
    index <- data[[i]]$temp
    population <- data[[i]]$population
    normalized_emissions_by_state$Vehicles_population[index] <- population
  }
  write.csv(normalized_emissions_by_state, paste0(results_path, "/TEMP_normalized_emissions_by_state.csv"))
  fleet_emissions_by_state <- select(normalized_emissions_by_state, -c("Order_key"))
  fleet_emissions_by_state[,7:16] <- fleet_emissions_by_state[,7:16]*fleet_emissions_by_state$Vehicles_population/1e6
  fleet_emissions_by_state <- select(fleet_emissions_by_state, -c(Source, Fuel, ModelYr, Run, Vehicles_population))
  fleet_emissions_by_state <- aggregate(.~Year+State, data = fleet_emissions_by_state, FUN = sum)
  fleet_emissions_by_state <- add_column(fleet_emissions_by_state, "Unit" = "tons")
  fleet_emissions_national <- aggregate(.~Year+Unit, data = select(fleet_emissions_by_state, -c(State)), FUN = sum)
  colnames(fleet_emissions_by_state)[4] <- "NOx"
  colnames(fleet_emissions_national)[4] <- "NOx"
  write.csv(fleet_emissions_national, paste0(results_path, "/emissions_fleet.csv"))
  write.csv(fleet_emissions_by_state, paste0(results_path, "/emissions_fleet_state_breakdown.csv"))
  # Run COBRA and generate the output files containing the cumulative health impacts at the national and state scales
  state_resolved_fleet_direct_emissions <- fleet_emissions_by_state
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
