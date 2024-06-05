air_quality_module_f <- function(fleet_dataset, scenario_id = NA, brake_tire_emissions_modification = NA, fuel_matching_option = NA, fleet_electricity_consumption_source = NA, first_proj_yr = NA, fleet_id = NA, last_yr = NA, years_simulation= NA, battery_calculation_method = NA, air_quality_model = NA, include_battery_manufacturing=NA, name = NA, fleet_ef_corr = NA) {
  attribute_f("air_quality_module_f")
  results_path <- paste0(getwd(), "/outputs/air_quality/", Sys.Date(), "_", scenario_id, "_results")
  # Upload the normalized emissions from MOVES
  normalized_emissions_by_state <- read.csv(paste0(getwd(), "/inputs/air_quality/normalized_emissions_by_state.csv"))
  # Adapt the technology and size names to the MOVES designation
  fleet_county <- fleet_dataset[["fleet_composition"]][["fleet_vint_stock_county"]]
  correspondence_file <- read.delim(paste0(getwd(), "/inputs/data/FLAME_to_MOVES_correspondance_files/technologies_", fuel_matching_option, ".txt"), header = TRUE, sep = ";", dec = ".")
  for (i in 1:length(correspondence_file$ID)) {
    fleet_county$Technology[which(fleet_county$Technology == correspondence_file$Technology[i])] <- correspondence_file$ID[i]
  }
  vehicles_sizes_file <- read.delim(paste0(getwd(), "/inputs/data/FLAME_to_MOVES_correspondance_files/vehicles_sizes.txt"), header = TRUE, sep = ";", dec = ".")
  for (i in 1:length(vehicles_sizes_file$Sizes)) {
    fleet_county$Size[which(fleet_county$Size == vehicles_sizes_file$Sizes[i])] <- vehicles_sizes_file$ID[i]
  }
  # Modify the tire and brake wear coefficients
  print("Modification of brake and tire wear coefficients")
  modified_emissions_factors <- do.call(modify_brake_tire_wear_f, list(normalized_emissions_by_state))
  normalized_emissions_by_state <- modified_emissions_factors[["normalized_emissions_by_state"]]
  if (fleet_ef_corr == 2014) {
    normalized_emissions_by_state$Nox <- normalized_emissions_by_state$Nox*2
  }
  print(paste0("Calculation of the fleet emissions using the scenario: ", scenario_id))
  fleet_county$Technology <- substr(fleet_county$Technology,1,1)
  colnames(fleet_county) <- c("Age", "Value", "Year", "Source", "Fuel", "FIPS", "State")
  fleet_county$Source <- as.numeric(fleet_county$Source)
  fleet_county$Fuel <- as.numeric(fleet_county$Fuel)
  fleet_county <- add_column(fleet_county, "ModelYr" = fleet_county$Year-fleet_county$Age)
  ## Aggregation of the fleet composition and the emissions factors
  print("Generating the vehicles emissions datasets")
  vehicles_emissions_county <- left_join(normalized_emissions_by_state, fleet_county, by = c("Year", "Source", "Fuel", "State", "ModelYr"))
  #if (scenario_id == "ZEV_All_2014_mix_benefits_per_ev") {
  #  vehicles_emissions_county$NO2[which(vehicles_emissions_county$Fuel == "")]
  #}
  vehicles_emissions_county[,6:15] <- vehicles_emissions_county[,6:15]*vehicles_emissions_county$Value/1e6
  vehicles_emissions_county <- select(vehicles_emissions_county, -c(Source, Fuel, ModelYr, Value, Age))
  vehicles_emissions_county <- add_column(vehicles_emissions_county, "Unit" = "tons")
  colnames(vehicles_emissions_county)[4] <- "NOx"
  vehicles_emissions_county <- aggregate(.~Year+State+FIPS+Unit, data = vehicles_emissions_county, FUN = sum)
  
  if (scenario_id == "no_ldvs") {
    vehicles_emissions_county$NOx <- vehicles_emissions_county$NOx*1
    vehicles_emissions_county$SO2 <- vehicles_emissions_county$SO2*1
    vehicles_emissions_county$Total_PM25 <- vehicles_emissions_county$Total_PM25*1
    vehicles_emissions_county$Brake_PM25 <- vehicles_emissions_county$Brake_PM25*1
    vehicles_emissions_county$Tire_PM25 <- vehicles_emissions_county$Tire_PM25*1
    vehicles_emissions_county$NH3 <- vehicles_emissions_county$NH3*1
    vehicles_emissions_county$VOC <- vehicles_emissions_county$VOC*1
  }

  print("Aggregation of the vehicles emissions at the state level")
  vehicles_emissions_state <- aggregate(.~Year+State+Unit, data = select(vehicles_emissions_county, -c("FIPS")), FUN = sum)
  print("Aggregation of the vehicles emissions at the national level")
  vehicles_emissions <- aggregate(.~Year+Unit, data = select(vehicles_emissions_state, -c("State")), FUN = sum)
  write.csv(vehicles_emissions, paste0(results_path, "/vehicles_emissions.csv"))
  print("Emissions from vehicles calculated")
  
  ## Export the fuel consumption data
  print("Export the fleet fuel and electricity consumption dataset")
  write.csv(fleet_dataset[["fleet_fuel_use_tot"]], paste0(results_path, "/fleet_fuel_use_tot.csv"), row.names = FALSE)
  write.csv(fleet_dataset[["fleet_fuel_use_tot_state"]], paste0(results_path, "/fleet_fuel_use_tot_state.csv"), row.names = FALSE)
  write.csv(fleet_dataset[["fleet_fuel_use_tot_county"]], paste0(results_path, "/fleet_fuel_use_tot_county.csv"), row.names = FALSE)
  write.csv(fleet_dataset[["fleet_elec_use_tot"]], paste0(results_path, "/fleet_elec_use_tot.csv"), row.names = FALSE)
  write.csv(fleet_dataset[["fleet_elec_use_tot_state"]], paste0(results_path, "/fleet_elec_use_tot_state.csv"), row.names = FALSE)
  write.csv(fleet_dataset[["fleet_elec_use_tot_county"]], paste0(results_path, "/fleet_elec_use_tot_county.csv"), row.names = FALSE)
  write.csv(fleet_dataset[["fleet_vint_fuel_use"]], paste0(results_path, "/fleet_vint_fuel_use.csv"), row.names = FALSE)
  
  # Calculation of the emissions from electricity and liquid fuel production (only valid if COBRA is used)
  #if (air_quality_model == "cobra") {
    print("Calculation of the emissions from electricity production")
    electricity_emissions_county <- do.call(electric_grid_emissions_f, list(fleet_elec_use_tot_county = fleet_dataset[["fleet_elec_use_tot_county"]]))
  #}
  print("Calculation of the well-to-pump emissions from liquid and gaseous fuel production")
  fleet_fuel_usage_US <- do.call(fuel_wtp_ef_f, list(fleet_fuel_usage_tot_state = fleet_dataset[["fleet_fuel_use_tot_state"]],
                                                     ng_demand_fleet = electricity_emissions_county[["ng_demand_fleet"]],
                                                     ng_demand_background = electricity_emissions_county[["ng_demand_background"]],
                                                     ng_frac_usage = electricity_emissions_county[["ng_frac_usage"]]))[["production_change_matrix"]]
  ## Calculation of the damages from battery manufacturing
  print("Calculation of the environmental damages from battery production")
  if (grepl("GREET", battery_calculation_method)) {
    emissions_battery_production <- do.call(health_damages_batteries_f, list(fleet_composition = fleet_dataset$fleet_composition))[["battery_manufacturing_emissions"]]
    if (include_battery_manufacturing == "N") {
      emissions_battery_production[2:6] <- 0
    }
    write.csv(emissions_battery_production, paste0(results_path, "/damages_battery_production.csv"))
  } else if (battery_calculation_method == "Tessum2014") {
    emissions_battery_production <- do.call(health_damages_batteries_f, list(fleet_composition = fleet_dataset$fleet_composition))
    if (include_battery_manufacturing == "N") {
      emissions_battery_production[["batteries_damages_monetized_yearly"]]$Discounted_damages <- 0
      emissions_battery_production[["batteries_damages_monetized_yearly_state"]]$Discounted_damages <- 0
      emissions_battery_production[["batteries_damages_monetized_yearly_county"]]$Discounted_damages <- 0
    }
    write.csv(emissions_battery_production[["batteries_damages_monetized_yearly"]], paste0(results_path, "/damages_battery_production.csv"))
    write.csv(emissions_battery_production[["batteries_damages_monetized_yearly_state"]], paste0(results_path, "/damages_battery_production_state.csv"))
    write.csv(emissions_battery_production[["batteries_damages_monetized_yearly_county"]], paste0(results_path, "/damages_battery_production_county.csv"))
  }
  
  print(paste0("Starting the calculation of health impacts using ", air_quality_model))
  if (air_quality_model == "cobra") {
    do.call(cobra_health_impact_f, list(vehicles_emissions_county = vehicles_emissions_county,
                                        electricity_emissions_fleet = data.frame(electricity_emissions_county[["fleet_elec_emissions"]]),
                                        electricity_emissions_background = data.frame(electricity_emissions_county[["background_elec_emissions"]]),
                                        ng_frac_usage = electricity_emissions_county[["ng_frac_usage"]],
                                        fleet_fuel_usage_US = fleet_fuel_usage_US,
                                        emissions_battery_production = emissions_battery_production))
  } else if (air_quality_model == "inmap") {
    do.call(inmap_health_impacts_f, list(fleet_elec_use_tot_county = fleet_dataset[["fleet_elec_use_tot_county"]],
                                                           vehicles_emissions_county = vehicles_emissions_county,
                                                           fleet_fuel_usage_tot_county = fleet_dataset[["fleet_fuel_use_tot_county"]],
                                                           oil_gas_activity = fleet_fuel_usage_US))
  } else if (air_quality_model == "easiur") {
    # Standard resolution of EASIUR
    do.call(easiur_health_impacts_f, list(fleet_elec_use_tot_county = fleet_dataset[["fleet_elec_use_tot_county"]],
                                                            vehicles_emissions_county = vehicles_emissions_county,
                                                            fleet_fuel_usage_tot_county = fleet_dataset[["fleet_fuel_use_tot_county"]],
                                                            oil_gas_activity = fleet_fuel_usage_US,
                                                            electricity_emissions_county = electricity_emissions_county))
  } else if (air_quality_model == "easiur_county") {
    # EASIUR at the county resolution
    
  }
}
