cobra_health_impact_f <- function(state_resolved_fleet_direct_emissions, fleet_emissions_elec_state, fleet_emissions_elec_county, fleet_fuel_usage_US, last_yr = NA, scenario_id = NA, fleet_electricity_consumption_source = NA, relative_transportation_electricity_demand = NA, calculation_mode = NA, discount_rate = NA) {
  attribute_f("cobra_health_impact_f")
  results_path <- paste0(getwd(), "/outputs/air_quality/", Sys.Date(), "_", scenario_id, "_results")
  first_proj_yr <- 2022
  fleet_direct_emissions_state_breakdown <- state_resolved_fleet_direct_emissions
  GIS_matching_matrix <- read.csv(paste0(getwd(), "/inputs/air_quality/GIS_matching_matrix.csv"))
  # Generate proper baseline and scenario inputs for COBRA
  template_baseline <- get_input_f(input_name = 'COBRA_emissions_baseline_template')
  template_scenario <- get_input_f(input_name = 'COBRA_emissions_scenario_template')
  # Generate state correspondence file
  list_states_MOVES <- read.csv(paste0(getwd(), "/inputs/air_quality/list_states.csv"), sep = ";")
  list_states_COBRA <- get_input_f(input_name = 'COBRA_SOURCEINDX to FIPS crosswalk')
  states_correspondence <- data.frame(unique(list_states_COBRA$STNAME))
  colnames(states_correspondence) <- "name_state"
  states_correspondence <- states_correspondence %>%  
    add_column(COBRA_code = NA) %>%
    add_column(MOVES_code = NA)
  for (i in 1:dim(states_correspondence)[1]) {
    states_correspondence$COBRA_code[i] <- unique(list_states_COBRA$STFIPS[which(list_states_COBRA$STNAME == states_correspondence$name_state[i])])
    states_correspondence$MOVES_code[i] <- list_states_MOVES$stateID[which(list_states_MOVES$stateName == toupper(states_correspondence$name_state[i]))]
  }
  emissions_tiers_definition <- get_input_f(input_name = 'COBRA_EmissionsTier Definitions')
  # Generate baseline and scenario files for each year
  baseline <- template_baseline
  scenario <- template_scenario
  yearly_change_NOx_emissions <- data.frame()
  yearly_change_SO2_emissions <- data.frame()
  yearly_change_PM25_emissions <- data.frame()
  m <- 0
  dir.create(paste0(results_path, "/COBRA_baseline_files"))
  dir.create(paste0(results_path, "/COBRA_scenario_files"))
  emissions_oil_related_activities <- data.frame(matrix(data = NA, nrow = 0, ncol = 8))
  colnames(emissions_oil_related_activities) <- c("State", "Year", "Unit", "NOx", "SO2", "PM25", "NH3", "VOC")
  county_allocation_factor <- get_input_f(input_name = 'normalized_vehicles_distribution_by_county')
  # Identify the indexes in the COBRA baseline and scenario files
  #   Indexes related oil and gas extraction, refining, and distribution 
  index_petroleum <- which(scenario$TIER1 == 6 & scenario$TIER2 == 1 & scenario$TIER3 == 99)
  index_petroleum_storage <- which(scenario$TIER1 == 9 & scenario$TIER2 == 2 & (scenario$TIER3 == 2 | scenario$TIER3 == 4 | scenario$TIER3 == 6 | scenario$TIER3 == 8))
  index_refining <- which(scenario$TIER1 == 6 & scenario$TIER2 == 2)
  index_refined_storage <- which(scenario$TIER1 == 9 & scenario$TIER2 == 2 &(scenario$TIER3 == 1 | scenario$TIER3 == 3 | scenario$TIER3 == 5 | scenario$TIER3 == 7 | scenario$TIER3 == 9 | scenario$TIER3 == 10))
  index_refined_storage_bis <- which(scenario$TIER1 == 9 & scenario$TIER2 == 3 & scenario$TIER3 != 5)
  index_refined_distribution <- which(scenario$TIER1 == 9 & (scenario$TIER2 == 4 | scenario$TIER2 == 5 | scenario$TIER2 == 6))
  index_ethanol <- which(scenario$TIER1 == 7 & scenario$TIER2 == 99 & scenario$TIER3 == 1)
  index_ng <- which(scenario$TIER1 == 6 & scenario$TIER2 == 1 & scenario$TIER3 == 1)
  index_petroleum <- c(index_petroleum, index_petroleum_storage)
  index_total_oil <- c(index_petroleum, index_refining, index_refined_distribution, index_ethanol)
  #   Index related to electricity production
  index_electricity <- which(scenario$TIER1 == 1)
  #   Index related to the LDV fleet
  index_fleet <- which(scenario$TIER1 == 11 & scenario$TIER3 == 2)
  if (calculation_mode == "default" | calculation_mode == "all") {
    for (i in (first_proj_yr+1):last_yr) {
      m <- m+1
      for (j in 1:length(states_correspondence$COBRA_code)) {
        index_state <- which(scenario$stid == states_correspondence$COBRA_code[j])
        padd <- unique(GIS_matching_matrix$PADD[which(GIS_matching_matrix$ST_FIPS == states_correspondence$COBRA_code[j])])
        rel_demand <- fleet_fuel_usage_US$Rel_Demand[which(fleet_fuel_usage_US$PADD == padd & fleet_fuel_usage_US$Year == i)]
        rel_refining <- fleet_fuel_usage_US$Rel_Refining[which(fleet_fuel_usage_US$PADD == padd & fleet_fuel_usage_US$Year == i)]
        rel_crude <- fleet_fuel_usage_US$Rel_Crude_oil[which(fleet_fuel_usage_US$PADD == padd & fleet_fuel_usage_US$Year == i)]
        rel_ethanol <- fleet_fuel_usage_US$Rel_Ethanol[which(fleet_fuel_usage_US$PADD == padd & fleet_fuel_usage_US$Year == i)]
        rel_ng <- fleet_fuel_usage_US$Rel_Natural_Gas[which(fleet_fuel_usage_US$PADD == padd & fleet_fuel_usage_US$Year == i)]
        # Petroleum extraction and storage are linked to the crude oil factor
        scenario$NO2[intersect(index_petroleum, index_state)] <- baseline$NO2[intersect(index_petroleum, index_state)]*rel_crude
        scenario$SO2[intersect(index_petroleum, index_state)] <- baseline$SO2[intersect(index_petroleum, index_state)]*rel_crude
        scenario$NH3[intersect(index_petroleum, index_state)] <- baseline$NH3[intersect(index_petroleum, index_state)]*rel_crude
        scenario$PM25[intersect(index_petroleum, index_state)] <- baseline$PM25[intersect(index_petroleum, index_state)]*rel_crude
        scenario$VOC[intersect(index_petroleum, index_state)] <- baseline$VOC[intersect(index_petroleum, index_state)]*rel_crude
        # Refining and storage of final products is linked to the refining factor
        index_refining <- c(index_refining, index_refined_storage, index_refined_storage_bis)
        scenario$NO2[intersect(index_refining, index_state)] <- baseline$NO2[intersect(index_refining, index_state)]*rel_refining
        scenario$SO2[intersect(index_refining, index_state)] <- baseline$SO2[intersect(index_refining, index_state)]*rel_refining
        scenario$NH3[intersect(index_refining, index_state)] <- baseline$NH3[intersect(index_refining, index_state)]*rel_refining
        scenario$PM25[intersect(index_refining, index_state)] <- baseline$PM25[intersect(index_refining, index_state)]*rel_refining
        scenario$VOC[intersect(index_refining, index_state)] <- baseline$VOC[intersect(index_refining, index_state)]*rel_refining
        # Distribution is linked to the demand factor
        scenario$NO2[intersect(index_refined_distribution, index_state)] <- baseline$NO2[intersect(index_refined_distribution, index_state)]*rel_demand
        scenario$SO2[intersect(index_refined_distribution, index_state)] <- baseline$SO2[intersect(index_refined_distribution, index_state)]*rel_demand
        scenario$NH3[intersect(index_refined_distribution, index_state)] <- baseline$NH3[intersect(index_refined_distribution, index_state)]*rel_demand
        scenario$PM25[intersect(index_refined_distribution, index_state)] <- baseline$PM25[intersect(index_refined_distribution, index_state)]*rel_demand
        scenario$VOC[intersect(index_refined_distribution, index_state)] <- baseline$VOC[intersect(index_refined_distribution, index_state)]*rel_demand
        # Ethanol production
        scenario$NO2[intersect(index_ethanol, index_state)] <- baseline$NO2[intersect(index_ethanol, index_state)]*rel_ethanol
        scenario$SO2[intersect(index_ethanol, index_state)] <- baseline$SO2[intersect(index_ethanol, index_state)]*rel_ethanol
        scenario$NH3[intersect(index_ethanol, index_state)] <- baseline$NH3[intersect(index_ethanol, index_state)]*rel_ethanol
        scenario$PM25[intersect(index_ethanol, index_state)] <- baseline$PM25[intersect(index_ethanol, index_state)]*rel_ethanol
        scenario$VOC[intersect(index_ethanol, index_state)] <- baseline$VOC[intersect(index_ethanol, index_state)]*rel_ethanol
        # Natural gas emissions
        scenario$NO2[intersect(index_ng, index_state)] <- baseline$NO2[intersect(index_ng, index_state)]*rel_ng
        scenario$SO2[intersect(index_ng, index_state)] <- baseline$SO2[intersect(index_ng, index_state)]*rel_ng
        scenario$NH3[intersect(index_ng, index_state)] <- baseline$NH3[intersect(index_ng, index_state)]*rel_ng
        scenario$PM25[intersect(index_ng, index_state)] <- baseline$PM25[intersect(index_ng, index_state)]*rel_ng
        scenario$VOC[intersect(index_ng, index_state)] <- baseline$VOC[intersect(index_ng, index_state)]*rel_ng
        # Generate the output file for oil-related emissions
        emissions_oil_related_activities[nrow(emissions_oil_related_activities)+1,] <- c(states_correspondence$COBRA_code[j], i, "tons", sum(scenario$NO2[intersect(index_total_oil, index_state)]), sum(scenario$SO2[intersect(index_total_oil, index_state)]), sum(scenario$PM25[intersect(index_total_oil, index_state)]), sum(scenario$NH3[intersect(index_total_oil, index_state)]), sum(scenario$VOC[intersect(index_total_oil, index_state)])) 
        # Generation of the county-resolved emissions
        counties <- data.frame(GIS_matching_matrix$COBRA_SOURCEINDX[which(GIS_matching_matrix$ST_FIPS == states_correspondence$COBRA_code[j])], GIS_matching_matrix$FIPS[which(GIS_matching_matrix$ST_FIPS == states_correspondence$COBRA_code[j])])
        colnames(counties) <- c("sourceindx", "FIPS")
        fleet_emissions_state <- fleet_direct_emissions_state_breakdown[which(fleet_direct_emissions_state_breakdown$State == states_correspondence$COBRA_code[j]),]
        for (k in 1:dim(counties)[1]) {
          index_county <- which(scenario$sourceindx == counties$sourceindx[k])
          county_FIPS <- GIS_matching_matrix$FIPS[which(GIS_matching_matrix$COBRA_SOURCEINDX == counties$sourceindx[k])]
          county_pop_factor <- county_allocation_factor$county_allocation_factor[which(county_allocation_factor$FIPS == county_FIPS)]
          if (i == (first_proj_yr+1)) { #In the first year, overwrite the data in the baseline file
            baseline$NO2[intersect(index_fleet, index_county)] <- fleet_emissions_state$NOx[which(fleet_emissions_state$Year == i-1 & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
            baseline$SO2[intersect(index_fleet, index_county)] <- fleet_emissions_state$SO2[which(fleet_emissions_state$Year == i-1 & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
            baseline$PM25[intersect(index_fleet, index_county)] <- (fleet_emissions_state$Total_PM25[which(fleet_emissions_state$Year == i-1 & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]+fleet_emissions_state$Brake_PM25[which(fleet_emissions_state$Year == i-1 & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]+fleet_emissions_state$Tire_PM25[which(fleet_emissions_state$Year == i-1 & fleet_emissions_state$State == states_correspondence$COBRA_code[j])])*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
            baseline$NH3[intersect(index_fleet, index_county)] <- fleet_emissions_state$NH3[which(fleet_emissions_state$Year == i-1 & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
            baseline$VOC[intersect(index_fleet, index_county)] <- fleet_emissions_state$VOC[which(fleet_emissions_state$Year == i-1 & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
          }
          #Calculation of the emissions from the fleet - county-based with data taken directly from MOVES (overwrites original data from COBRA)
          scenario$NO2[intersect(index_fleet, index_county)] <- fleet_emissions_state$NOx[which(fleet_emissions_state$Year == i & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
          scenario$SO2[intersect(index_fleet, index_county)] <- fleet_emissions_state$SO2[which(fleet_emissions_state$Year == i & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
          scenario$PM25[intersect(index_fleet, index_county)] <- (fleet_emissions_state$Total_PM25[which(fleet_emissions_state$Year == i & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]+fleet_emissions_state$Brake_PM25[which(fleet_emissions_state$Year == i & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]+fleet_emissions_state$Tire_PM25[which(fleet_emissions_state$Year == i & fleet_emissions_state$State == states_correspondence$COBRA_code[j])])*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
          scenario$NH3[intersect(index_fleet, index_county)] <- fleet_emissions_state$NH3[which(fleet_emissions_state$Year == i & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
          scenario$VOC[intersect(index_fleet, index_county)] <- fleet_emissions_state$VOC[which(fleet_emissions_state$Year == i & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
          # Calculation of the emissions from electricity
          index_fips <- which(fleet_emissions_elec_county$FIPS == counties$FIPS[k] & fleet_emissions_elec_county$Year == i)
          scenario$NO2[intersect(index_electricity, index_county)] <- baseline$NO2[intersect(index_electricity, index_county)] + (fleet_emissions_elec_county$NOx[index_fips]-fleet_emissions_elec_county$NOx[which(fleet_emissions_elec_county$FIPS == counties$FIPS[k] & fleet_emissions_elec_county$Year == i-1)])/length(scenario$NO2[intersect(index_electricity, index_county)])
          scenario$SO2[intersect(index_electricity, index_county)] <- baseline$SO2[intersect(index_electricity, index_county)] + (fleet_emissions_elec_county$SO2[index_fips]-fleet_emissions_elec_county$SO2[which(fleet_emissions_elec_county$FIPS == counties$FIPS[k] & fleet_emissions_elec_county$Year == i-1)])/length(scenario$SO2[intersect(index_electricity, index_county)])
          scenario$NH3[intersect(index_electricity, index_county)] <- baseline$NH3[intersect(index_electricity, index_county)] + (fleet_emissions_elec_county$NH3[index_fips]-fleet_emissions_elec_county$NH3[which(fleet_emissions_elec_county$FIPS == counties$FIPS[k] & fleet_emissions_elec_county$Year == i-1)])/length(scenario$NH3[intersect(index_electricity, index_county)])
          scenario$PM25[intersect(index_electricity, index_county)] <- baseline$PM25[intersect(index_electricity, index_county)] + (fleet_emissions_elec_county$PM25[index_fips]-fleet_emissions_elec_county$PM25[which(fleet_emissions_elec_county$FIPS == counties$FIPS[k] & fleet_emissions_elec_county$Year == i-1)])/length(scenario$PM25[intersect(index_electricity, index_county)])
          scenario$VOC[intersect(index_electricity, index_county)] <- baseline$VOC[intersect(index_electricity, index_county)] + (fleet_emissions_elec_county$VOC[index_fips]-fleet_emissions_elec_county$VOC[which(fleet_emissions_elec_county$FIPS == counties$FIPS[k] & fleet_emissions_elec_county$Year == i-1)])/length(scenario$VOC[intersect(index_electricity, index_county)])
        }
      }
      write.csv(baseline, paste0(getwd(), "/inputs/air_quality/cobra_inputs/COBRA_emissions_baseline_", i, ".csv"), row.names = FALSE)
      write.csv(scenario, paste0(getwd(), "/inputs/air_quality/cobra_inputs/COBRA_emissions_scenario_", i, ".csv"), row.names = FALSE)
      # Write the files in the output folder for debugging purposes (can be removed if no longer needed)
      write.csv(baseline, paste0(results_path, "/COBRA_baseline_files/COBRA_emissions_baseline_", i, ".csv"), row.names = FALSE)
      write.csv(scenario, paste0(results_path, "/COBRA_scenario_files/COBRA_emissions_scenario_", i, ".csv"), row.names = FALSE)
      baseline <- scenario
    }
    write.csv(emissions_oil_related_activities, paste0(results_path, "/emissions_oil_related_activities.csv"), row.names = FALSE)
  }
  baseline <- template_baseline
  scenario <- template_scenario
  if (calculation_mode == "aggregated" | calculation_mode == "all") {
    m <- 1
    for (j in 1:length(states_correspondence$COBRA_code)) {
      index_state <- which(scenario$stid == states_correspondence$COBRA_code[j])
      padd <- unique(GIS_matching_matrix$PADD[which(GIS_matching_matrix$ST_FIPS == states_correspondence$COBRA_code[j])])
      rel_demand <- prod(fleet_fuel_usage_US$Rel_Demand[which(fleet_fuel_usage_US$PADD == padd)])
      rel_refining <- prod(fleet_fuel_usage_US$Rel_Refining[which(fleet_fuel_usage_US$PADD == padd)])
      rel_crude <- prod(fleet_fuel_usage_US$Rel_Crude_oil[which(fleet_fuel_usage_US$PADD == padd)])
      rel_ethanol <- prod(fleet_fuel_usage_US$Rel_Ethanol[which(fleet_fuel_usage_US$PADD == padd)])
      rel_ng <- prod(fleet_fuel_usage_US$Rel_Natural_Gas[which(fleet_fuel_usage_US$PADD == padd)])
      # Petroleum extraction and storage are linked to the crude oil factor
      scenario$NO2[intersect(index_petroleum, index_state)] <- baseline$NO2[intersect(index_petroleum, index_state)]*rel_crude
      scenario$SO2[intersect(index_petroleum, index_state)] <- baseline$SO2[intersect(index_petroleum, index_state)]*rel_crude
      scenario$NH3[intersect(index_petroleum, index_state)] <- baseline$NH3[intersect(index_petroleum, index_state)]*rel_crude
      scenario$PM25[intersect(index_petroleum, index_state)] <- baseline$PM25[intersect(index_petroleum, index_state)]*rel_crude
      scenario$VOC[intersect(index_petroleum, index_state)] <- baseline$VOC[intersect(index_petroleum, index_state)]*rel_crude
      # Refining and storage of final products is linked to the refining factor
      scenario$NO2[intersect(index_refining, index_state)] <- baseline$NO2[intersect(index_refining, index_state)]*rel_refining
      scenario$SO2[intersect(index_refining, index_state)] <- baseline$SO2[intersect(index_refining, index_state)]*rel_refining
      scenario$NH3[intersect(index_refining, index_state)] <- baseline$NH3[intersect(index_refining, index_state)]*rel_refining
      scenario$PM25[intersect(index_refining, index_state)] <- baseline$PM25[intersect(index_refining, index_state)]*rel_refining
      scenario$VOC[intersect(index_refining, index_state)] <- baseline$VOC[intersect(index_refining, index_state)]*rel_refining
      # Distribution is linked to the demand factor
      scenario$NO2[intersect(index_refined_distribution, index_state)] <- baseline$NO2[intersect(index_refined_distribution, index_state)]*rel_demand
      scenario$SO2[intersect(index_refined_distribution, index_state)] <- baseline$SO2[intersect(index_refined_distribution, index_state)]*rel_demand
      scenario$NH3[intersect(index_refined_distribution, index_state)] <- baseline$NH3[intersect(index_refined_distribution, index_state)]*rel_demand
      scenario$PM25[intersect(index_refined_distribution, index_state)] <- baseline$PM25[intersect(index_refined_distribution, index_state)]*rel_demand
      scenario$VOC[intersect(index_refined_distribution, index_state)] <- baseline$VOC[intersect(index_refined_distribution, index_state)]*rel_demand
      # Ethanol production
      scenario$NO2[intersect(index_ethanol, index_state)] <- baseline$NO2[intersect(index_ethanol, index_state)]*rel_ethanol
      scenario$SO2[intersect(index_ethanol, index_state)] <- baseline$SO2[intersect(index_ethanol, index_state)]*rel_ethanol
      scenario$NH3[intersect(index_ethanol, index_state)] <- baseline$NH3[intersect(index_ethanol, index_state)]*rel_ethanol
      scenario$PM25[intersect(index_ethanol, index_state)] <- baseline$PM25[intersect(index_ethanol, index_state)]*rel_ethanol
      scenario$VOC[intersect(index_ethanol, index_state)] <- baseline$VOC[intersect(index_ethanol, index_state)]*rel_ethanol
      # Natural gas emissions
      scenario$NO2[intersect(index_ng, index_state)] <- baseline$NO2[intersect(index_ng, index_state)]*rel_ng
      scenario$SO2[intersect(index_ng, index_state)] <- baseline$SO2[intersect(index_ng, index_state)]*rel_ng
      scenario$NH3[intersect(index_ng, index_state)] <- baseline$NH3[intersect(index_ng, index_state)]*rel_ng
      scenario$PM25[intersect(index_ng, index_state)] <- baseline$PM25[intersect(index_ng, index_state)]*rel_ng
      scenario$VOC[intersect(index_ng, index_state)] <- baseline$VOC[intersect(index_ng, index_state)]*rel_ng
      # Generate the output file for oil-related emissions
      emissions_oil_related_activities[nrow(emissions_oil_related_activities)+1,] <- c(states_correspondence$COBRA_code[j], i, "tons", sum(scenario$NO2[intersect(index_total_oil, index_state)]), sum(scenario$SO2[intersect(index_total_oil, index_state)]), sum(scenario$PM25[intersect(index_total_oil, index_state)]), sum(scenario$NH3[intersect(index_total_oil, index_state)]), sum(scenario$VOC[intersect(index_total_oil, index_state)])) 
      # Emissions from the electricity
      counties <- data.frame(GIS_matching_matrix$COBRA_SOURCEINDX[which(GIS_matching_matrix$ST_FIPS == states_correspondence$COBRA_code[j])], GIS_matching_matrix$FIPS[which(GIS_matching_matrix$ST_FIPS == states_correspondence$COBRA_code[j])])
      colnames(counties) <- c("sourceindx", "FIPS")
      fleet_emissions_state <- fleet_direct_emissions_state_breakdown[which(fleet_direct_emissions_state_breakdown$State == states_correspondence$COBRA_code[j]),]
      for (k in 1:dim(counties)[1]) {
        index_county <- which(scenario$sourceindx == counties$sourceindx[k])
        county_FIPS <- GIS_matching_matrix$FIPS[which(GIS_matching_matrix$COBRA_SOURCEINDX == counties$sourceindx[k])]
        county_pop_factor <- county_allocation_factor$county_allocation_factor[which(county_allocation_factor$FIPS == county_FIPS)]
        baseline$NO2[intersect(index_fleet, index_county)] <- fleet_emissions_state$NOx[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
        baseline$SO2[intersect(index_fleet, index_county)] <- fleet_emissions_state$SO2[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
        baseline$PM25[intersect(index_fleet, index_county)] <- (fleet_emissions_state$Total_PM25[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]+fleet_emissions_state$Brake_PM25[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]+fleet_emissions_state$Tire_PM25[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[j])])*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
        baseline$NH3[intersect(index_fleet, index_county)] <- fleet_emissions_state$NH3[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
        baseline$VOC[intersect(index_fleet, index_county)] <- fleet_emissions_state$VOC[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
        # Calculation of the emissions from the fleet - county-based with data taken directly from MOVES (overwrites original data from COBRA)
        scenario$NO2[intersect(index_fleet, index_county)] <- fleet_emissions_state$NOx[which(fleet_emissions_state$Year == last_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
        scenario$SO2[intersect(index_fleet, index_county)] <- fleet_emissions_state$SO2[which(fleet_emissions_state$Year == last_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
        scenario$PM25[intersect(index_fleet, index_county)] <- (fleet_emissions_state$Total_PM25[which(fleet_emissions_state$Year == last_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]+fleet_emissions_state$Brake_PM25[which(fleet_emissions_state$Year == last_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]+fleet_emissions_state$Tire_PM25[which(fleet_emissions_state$Year == last_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[j])])*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
        scenario$NH3[intersect(index_fleet, index_county)] <- fleet_emissions_state$NH3[which(fleet_emissions_state$Year == last_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
        scenario$VOC[intersect(index_fleet, index_county)] <- fleet_emissions_state$VOC[which(fleet_emissions_state$Year == last_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[j])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
        # Calculation of the emissions from electricity
        index_fips <- which(fleet_emissions_elec_county$FIPS == counties$FIPS[k] & fleet_emissions_elec_county$Year == last_yr)
        scenario$NO2[intersect(index_electricity, index_county)] <- baseline$NO2[intersect(index_electricity, index_county)] + (fleet_emissions_elec_county$NOx[index_fips]-fleet_emissions_elec_county$NOx[which(fleet_emissions_elec_county$FIPS == counties$FIPS[k] & fleet_emissions_elec_county$Year == first_proj_yr+1)])/length(scenario$NO2[intersect(index_electricity, index_county)])
        scenario$SO2[intersect(index_electricity, index_county)] <- baseline$SO2[intersect(index_electricity, index_county)] + (fleet_emissions_elec_county$SO2[index_fips]-fleet_emissions_elec_county$SO2[which(fleet_emissions_elec_county$FIPS == counties$FIPS[k] & fleet_emissions_elec_county$Year == first_proj_yr+1)])/length(scenario$SO2[intersect(index_electricity, index_county)])
        scenario$NH3[intersect(index_electricity, index_county)] <- baseline$NH3[intersect(index_electricity, index_county)] + (fleet_emissions_elec_county$NH3[index_fips]-fleet_emissions_elec_county$NH3[which(fleet_emissions_elec_county$FIPS == counties$FIPS[k] & fleet_emissions_elec_county$Year == first_proj_yr+1)])/length(scenario$NH3[intersect(index_electricity, index_county)])
        scenario$PM25[intersect(index_electricity, index_county)] <- baseline$PM25[intersect(index_electricity, index_county)] + (fleet_emissions_elec_county$PM25[index_fips]-fleet_emissions_elec_county$PM25[which(fleet_emissions_elec_county$FIPS == counties$FIPS[k] & fleet_emissions_elec_county$Year == first_proj_yr+1)])/length(scenario$PM25[intersect(index_electricity, index_county)])
        scenario$VOC[intersect(index_electricity, index_county)] <- baseline$VOC[intersect(index_electricity, index_county)] + (fleet_emissions_elec_county$VOC[index_fips]-fleet_emissions_elec_county$VOC[which(fleet_emissions_elec_county$FIPS == counties$FIPS[k] & fleet_emissions_elec_county$Year == first_proj_yr+1)])/length(scenario$VOC[intersect(index_electricity, index_county)])
      }
    }
    # Write the files in the input folder to be used by COBRA
    write.csv(baseline, paste0(getwd(), "/inputs/air_quality/cobra_inputs/COBRA_emissions_baseline_aggregated.csv"), row.names = FALSE)
    write.csv(scenario, paste0(getwd(), "/inputs/air_quality/cobra_inputs/COBRA_emissions_scenario_aggregated.csv"), row.names = FALSE)
    # Write the files in the output folder for debugging purposes (can be removed if no longer needed)
    write.csv(baseline, paste0(results_path, "/COBRA_baseline_files/COBRA_emissions_baseline_aggregated.csv"), row.names = FALSE)
    write.csv(scenario, paste0(results_path, "/COBRA_scenario_files/COBRA_emissions_scenario_aggregated.csv"), row.names = FALSE)
    write.csv(emissions_oil_related_activities, paste0(results_path, "/emissions_oil_related_activities.csv"), row.names = FALSE)
  }
  baseline <- template_baseline
  scenario <- template_scenario
  if (calculation_mode == "pollutants_breakdown" | calculation_mode == "all") {
    pollutants <- c("NOx", "SO2", "PM25Combustion", "PM25Tire", "PM25Brake", "NH3", "VOC")
    sectors <- c("Fleet", "Electricity", "OilIndustry")
    m <- 0
    for (i in pollutants) {
      for (j in sectors) {
        m <- m+1
        for (k in 1:length(states_correspondence$COBRA_code)) {
          if (j == "OilIndustry") {
            index_state <- which(scenario$stid == states_correspondence$COBRA_code[j])
            padd <- unique(GIS_matching_matrix$PADD[which(GIS_matching_matrix$ST_FIPS == states_correspondence$COBRA_code[k])])
            rel_demand <- prod(fleet_fuel_usage_US$Rel_Demand[which(fleet_fuel_usage_US$PADD == padd)])
            rel_refining <- prod(fleet_fuel_usage_US$Rel_Refining[which(fleet_fuel_usage_US$PADD == padd)])
            rel_crude <- prod(fleet_fuel_usage_US$Rel_Crude_oil[which(fleet_fuel_usage_US$PADD == padd)])
            rel_ethanol <- prod(fleet_fuel_usage_US$Rel_Ethanol[which(fleet_fuel_usage_US$PADD == padd)])
            rel_ng <- prod(fleet_fuel_usage_US$Rel_Natural_Gas[which(fleet_fuel_usage_US$PADD == padd)])
            if (i == "NOx") {
              scenario$NO2[intersect(index_petroleum, index_state)] <- baseline$NO2[intersect(index_petroleum, index_state)]*rel_crude
              scenario$NO2[intersect(index_refining, index_state)] <- baseline$NO2[intersect(index_refining, index_state)]*rel_refining
              scenario$NO2[intersect(index_refined_distribution, index_state)] <- baseline$NO2[intersect(index_refined_distribution, index_state)]*rel_demand
              scenario$NO2[intersect(index_ethanol, index_state)] <- baseline$NO2[intersect(index_ethanol, index_state)]*rel_ethanol
              scenario$NO2[intersect(index_ng, index_state)] <- baseline$NO2[intersect(index_ng, index_state)]*rel_ng
            } else if (i == "SO2") {
              scenario$SO2[intersect(index_petroleum, index_state)] <- baseline$SO2[intersect(index_petroleum, index_state)]*rel_crude
              scenario$SO2[intersect(index_refining, index_state)] <- baseline$SO2[intersect(index_refining, index_state)]*rel_refining
              scenario$SO2[intersect(index_refined_distribution, index_state)] <- baseline$SO2[intersect(index_refined_distribution, index_state)]*rel_demand
              scenario$SO2[intersect(index_ethanol, index_state)] <- baseline$SO2[intersect(index_ethanol, index_state)]*rel_ethanol
              scenario$SO2[intersect(index_ng, index_state)] <- baseline$SO2[intersect(index_ng, index_state)]*rel_ng
            } else if (i == "PM25Combustion") {
              scenario$PM25[intersect(index_petroleum, index_state)] <- baseline$PM25[intersect(index_petroleum, index_state)]*rel_crude
              scenario$PM25[intersect(index_refining, index_state)] <- baseline$PM25[intersect(index_refining, index_state)]*rel_refining
              scenario$PM25[intersect(index_refined_distribution, index_state)] <- baseline$PM25[intersect(index_refined_distribution, index_state)]*rel_demand
              scenario$PM25[intersect(index_ethanol, index_state)] <- baseline$PM25[intersect(index_ethanol, index_state)]*rel_ethanol
              scenario$PM25[intersect(index_ng, index_state)] <- baseline$PM25[intersect(index_ng, index_state)]*rel_ng
            } else if (i == "NH3") {
              scenario$NH3[intersect(index_petroleum, index_state)] <- baseline$NH3[intersect(index_petroleum, index_state)]*rel_crude
              scenario$NH3[intersect(index_refining, index_state)] <- baseline$NH3[intersect(index_refining, index_state)]*rel_refining
              scenario$NH3[intersect(index_refined_distribution, index_state)] <- baseline$NH3[intersect(index_refined_distribution, index_state)]*rel_demand
              scenario$NH3[intersect(index_ethanol, index_state)] <- baseline$NH3[intersect(index_ethanol, index_state)]*rel_ethanol
              scenario$NH3[intersect(index_ng, index_state)] <- baseline$NH3[intersect(index_ng, index_state)]*rel_ng
            } else if (i == "VOC") {
              scenario$VOC[intersect(index_petroleum, index_state)] <- baseline$VOC[intersect(index_petroleum, index_state)]*rel_crude
              scenario$VOC[intersect(index_refining, index_state)] <- baseline$VOC[intersect(index_refining, index_state)]*rel_refining
              scenario$VOC[intersect(index_refined_distribution, index_state)] <- baseline$VOC[intersect(index_refined_distribution, index_state)]*rel_demand
              scenario$VOC[intersect(index_ethanol, index_state)] <- baseline$VOC[intersect(index_ethanol, index_state)]*rel_ethanol
              scenario$VOC[intersect(index_ng, index_state)] <- baseline$VOC[intersect(index_ng, index_state)]*rel_ng
            }
          } else if (j == "Electricity") {
            counties <- data.frame(GIS_matching_matrix$COBRA_SOURCEINDX[which(GIS_matching_matrix$ST_FIPS == states_correspondence$COBRA_code[k])], GIS_matching_matrix$FIPS[which(GIS_matching_matrix$ST_FIPS == states_correspondence$COBRA_code[k])])
            colnames(counties) <- c("sourceindx", "FIPS")
            for (l in 1:dim(counties)[1]) {
              index_county <- which(scenario$sourceindx == counties$sourceindx[l])
              index_fips <- which(fleet_emissions_elec_county$FIPS == counties$FIPS[l] & fleet_emissions_elec_county$Year == last_yr)
              if (i == "NOx") {
                scenario$NO2[intersect(index_electricity, index_county)] <- baseline$NO2[intersect(index_electricity, index_county)] + (fleet_emissions_elec_county$NOx[index_fips]-fleet_emissions_elec_county$NOx[which(fleet_emissions_elec_county$FIPS == counties$FIPS[l] & fleet_emissions_elec_county$Year == first_proj_yr+1)])/length(scenario$NO2[intersect(index_electricity, index_county)])
              } else if (i == "SO2") {
                scenario$SO2[intersect(index_electricity, index_county)] <- baseline$SO2[intersect(index_electricity, index_county)] + (fleet_emissions_elec_county$SO2[index_fips]-fleet_emissions_elec_county$SO2[which(fleet_emissions_elec_county$FIPS == counties$FIPS[l] & fleet_emissions_elec_county$Year == first_proj_yr+1)])/length(scenario$SO2[intersect(index_electricity, index_county)])
              } else if (i == "PM25Combustion") {
                scenario$PM25[intersect(index_electricity, index_county)] <- baseline$PM25[intersect(index_electricity, index_county)] + (fleet_emissions_elec_county$PM25[index_fips]-fleet_emissions_elec_county$PM25[which(fleet_emissions_elec_county$FIPS == counties$FIPS[l] & fleet_emissions_elec_county$Year == first_proj_yr+1)])/length(scenario$PM25[intersect(index_electricity, index_county)])
              } else if (i == "NH3") {
                scenario$NH3[intersect(index_electricity, index_county)] <- baseline$NH3[intersect(index_electricity, index_county)] + (fleet_emissions_elec_county$NH3[index_fips]-fleet_emissions_elec_county$NH3[which(fleet_emissions_elec_county$FIPS == counties$FIPS[l] & fleet_emissions_elec_county$Year == first_proj_yr+1)])/length(scenario$NH3[intersect(index_electricity, index_county)])
              } else if (i == "VOC") {
                scenario$VOC[intersect(index_electricity, index_county)] <- baseline$VOC[intersect(index_electricity, index_county)] + (fleet_emissions_elec_county$VOC[index_fips]-fleet_emissions_elec_county$NOx[which(fleet_emissions_elec_county$FIPS == counties$FIPS[l] & fleet_emissions_elec_county$Year == first_proj_yr+1)])/length(scenario$VOC[intersect(index_electricity, index_county)])
              }
            }
          }  else if (j == "Fleet") {
            # Emissions from the fleet
            fleet_emissions_state <- fleet_direct_emissions_state_breakdown[which(fleet_direct_emissions_state_breakdown$State == states_correspondence$COBRA_code[k]),]
            counties <- data.frame(GIS_matching_matrix$COBRA_SOURCEINDX[which(GIS_matching_matrix$ST_FIPS == states_correspondence$COBRA_code[k])], GIS_matching_matrix$FIPS[which(GIS_matching_matrix$ST_FIPS == states_correspondence$COBRA_code[k])])
            colnames(counties) <- c("sourceindx", "FIPS")
            for (l in 1:dim(counties)[1]) {
              county_FIPS <- GIS_matching_matrix$FIPS[which(GIS_matching_matrix$COBRA_SOURCEINDX == counties$sourceindx[l])]
              county_pop_factor <- county_allocation_factor$county_allocation_factor[which(county_allocation_factor$FIPS == county_FIPS)]
              if (i == "NOx") {
                baseline$NO2[intersect(index_fleet, index_county)] <- fleet_emissions_state$NOx[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
                scenario$NO2[intersect(index_fleet, index_county)] <- fleet_emissions_state$NOx[which(fleet_emissions_state$Year == last_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
              } else if (i == "SO2") {
                baseline$SO2[intersect(index_fleet, index_county)] <- fleet_emissions_state$SO2[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
                scenario$SO2[intersect(index_fleet, index_county)] <- fleet_emissions_state$SO2[which(fleet_emissions_state$Year == last_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
              } else if (i == "PM25Combustion") {
                baseline$PM25[intersect(index_fleet, index_county)] <- (fleet_emissions_state$Total_PM25[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]+fleet_emissions_state$Brake_PM25[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]+fleet_emissions_state$Tire_PM25[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])])*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
                scenario$PM25[intersect(index_fleet, index_county)] <- (fleet_emissions_state$Total_PM25[which(fleet_emissions_state$Year == last_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]+fleet_emissions_state$Brake_PM25[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]+fleet_emissions_state$Tire_PM25[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])])*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
              } else if (i == "PM25Tire") {
                baseline$PM25[intersect(index_fleet, index_county)] <- (fleet_emissions_state$Total_PM25[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]+fleet_emissions_state$Brake_PM25[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]+fleet_emissions_state$Tire_PM25[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])])*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
                scenario$PM25[intersect(index_fleet, index_county)] <- (fleet_emissions_state$Total_PM25[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]+fleet_emissions_state$Brake_PM25[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]+fleet_emissions_state$Tire_PM25[which(fleet_emissions_state$Year == last_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])])*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
              } else if (i == "PM25Brake") {
                baseline$PM25[intersect(index_fleet, index_county)] <- (fleet_emissions_state$Total_PM25[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]+fleet_emissions_state$Brake_PM25[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]+fleet_emissions_state$Tire_PM25[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])])*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
                scenario$PM25[intersect(index_fleet, index_county)] <- (fleet_emissions_state$Total_PM25[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]+fleet_emissions_state$Brake_PM25[which(fleet_emissions_state$Year == last_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]+fleet_emissions_state$Tire_PM25[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k ])])*county_pop_factor/length(scenario$NO2[intersect(index_fleet, index_county)])
              } else if (i == "NH3") {
                baseline$NH3[intersect(index_fleet, index_county)] <- fleet_emissions_state$NOx[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]*county_pop_factor/length(scenario$NH3[intersect(index_fleet, index_county)])
                scenario$NH3[intersect(index_fleet, index_county)] <- fleet_emissions_state$NOx[which(fleet_emissions_state$Year == last_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]*county_pop_factor/length(scenario$NH3[intersect(index_fleet, index_county)])
              } else if (i == "VOC") {
                baseline$VOC[intersect(index_fleet, index_county)] <- fleet_emissions_state$VOC[which(fleet_emissions_state$Year == first_proj_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]*county_pop_factor/length(scenario$VOC[intersect(index_fleet, index_county)])
                scenario$VOC[intersect(index_fleet, index_county)] <- fleet_emissions_state$VOC[which(fleet_emissions_state$Year == last_yr & fleet_emissions_state$State == states_correspondence$COBRA_code[k])]*county_pop_factor/length(scenario$VOC[intersect(index_fleet, index_county)])
              }
            }
          }
        }
        # Write the files in the input folder to be used by COBRA
        write.csv(baseline, paste0(getwd(), "/inputs/air_quality/cobra_inputs/COBRA_emissions_baseline_breakdown_", i, "_", j, ".csv"), row.names = FALSE)
        write.csv(scenario, paste0(getwd(), "/inputs/air_quality/cobra_inputs/COBRA_emissions_scenario_breakdown_", i, "_", j, ".csv"), row.names = FALSE)
        # Write the files in the output folder for debugging purposes (can be removed if no longer needed)
        write.csv(baseline, paste0(results_path, "/COBRA_baseline_files/COBRA_emissions_baseline_breakdown_", i, "_", j, ".csv"), row.names = FALSE)
        write.csv(scenario, paste0(results_path, "/COBRA_scenario_files/COBRA_emissions_scenario_breakdown_", i, "_", j, ".csv"), row.names = FALSE)
        baseline <- template_baseline
        scenario <- template_scenario
      }
    }
  }
  #write.csv(emissions_oil_related_activities, paste0(results_path, "/emissions_oil_related_activities.csv"), row.names = FALSE)

  # Call the population data function to generate input files for COBRA
  print("Generating the population datafiles for COBRA")
  #do.call(COBRA_population_data_generation_f, list())
  #Script to run COBRA using windows cmd
  # Generate .bat files //Add the correct year to the files and generate baseline and scenario files for each year // Add corresponding population data
  dir.create(paste0(results_path, "/COBRA_results"))
  if (calculation_mode == "default" | calculation_mode == "all") {
    print("Generating batch files for COBRA - Default mode")
    for (i in (first_proj_yr+1):last_yr) {
      executable_path <- '"C:/Program Files/COBRA/cobra_console.exe"'
      db_path <- '"C:/Program Files/COBRA/data/cobra.db"'
      A <- paste0(getwd(), "/inputs/air_quality/cobra_inputs/COBRA_emissions_baseline_", i, ".csv")
      #A <- paste0("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_Updated_FLAME/inputs/air_quality/cobra_inputs/COBRA_emissions_baseline_", i, ".csv")
      B <- paste0(getwd(), "/inputs/air_quality/cobra_inputs/COBRA_emissions_scenario_", i, ".csv")
      #B <- paste0("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_Updated_FLAME/inputs/air_quality/cobra_inputs/COBRA_emissions_scenario_", i, ".csv")
      C <- paste0(getwd(), "/inputs/air_quality/cobra_population/COBRA_population_data_", i, ".csv")
      #C <- paste0("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_Updated_FLAME/inputs/air_quality/cobra_population/COBRA_population_data_", i, ".csv")
      D <- paste0(getwd(), "/inputs/air_quality/COBRA_default_incidence_data.csv")
      #D <- '"D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_Updated_FLAME/inputs/air_quality/cobra_inputs/COBRA_default_incidence_data.csv"'
      E <- paste0(results_path, "/COBRA_results/COBRA_results_", i, ".csv")
      #E <- paste0("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_Updated_FLAME/outputs/air_quality/COBRA_results_", i, ".csv")
      F <- "YES"
      G <- 2023
      if (i == (first_proj_yr+1)) {
        command <- paste0(executable_path, ' -d ', db_path, ' -b', A, ' -c ', B, ' ', C, ' ', D, ' -o ', E, ' --pct3 ', F, ' -y ', G, "\n")
      } else {
        command <- paste0(command, executable_path, ' -d ', db_path, ' -b', A, ' -c ', B, ' ', C, ' ', D, ' -o ', E, ' --pct3 ', F, ' -y ', G, "\n")
      }
      #command <- paste0(executable_path, ' -d ', db_path, ' -b', A, ' -c ', B, ' ', C, ' ', D, ' -o ', E, ' --pct3 ', F, ' -y ', G, "\n")
      path <- paste0(getwd(), "/inputs/air_quality/COBRA_batch.bat")
      writeLines(command, path, sep = "\n")
      #if (i == (first_proj_yr+1)) {
      #  write(command, file = path)
      #} else {
      #  write(command, file = path, append = TRUE)
      #}
    }
    path <- paste0(getwd(), "/inputs/air_quality/COBRA_batch.bat")
    print("Starting COBRA - default mode calculation")
    shell(path, wait = TRUE)
    # Import all generated csv files from COBRA and calculate the cumulative benefits by state and in total
    results_files <- list.files(path = paste0(results_path, "/COBRA_results/"), pattern = "COBRA_results_")
    cobra_output_file <- data.frame()
    for (i in 1:length(results_files)) {
      path <- paste0(results_path, "/COBRA_results/", results_files[i])
      temp <- read.csv(path)
      temp <- temp %>%
        add_column(state_id = NA) %>%
        add_column(year = strsplit(strsplit(results_files[i], "_")[[1]][3], ".csv")[[1]][1])
      for (j in 1:length(states_correspondence$COBRA_code)) {
        temp$state_id[which(temp$Destination%in%list_states_COBRA$SOURCEINDX[which(list_states_COBRA$STFIPS == states_correspondence$COBRA_code[j])])] <- states_correspondence$COBRA_code[j]
      }
      if (i == 1) {
        cobra_output_file <- temp
      } else {
        cobra_output_file <- rbind(cobra_output_file, temp)
      } 
    }
  }
  if (calculation_mode == "aggregated" | calculation_mode == "all") {
    print("Generating batch files for COBRA - Aggregated mode")
    executable_path <- '"C:/Program Files/COBRA/cobra_console.exe"'
    db_path <- '"C:/Program Files/COBRA/data/cobra.db"'
    A <- paste0(getwd(), "/inputs/air_quality/cobra_inputs/COBRA_emissions_baseline_aggregated.csv")
    B <- paste0(getwd(), "/inputs/air_quality/cobra_inputs/COBRA_emissions_scenario_aggregated.csv")
    C <- paste0(getwd(), "/inputs/air_quality/cobra_population/COBRA_population_data_", last_yr, ".csv")
    D <- paste0(getwd(), "/inputs/air_quality/COBRA_default_incidence_data.csv")
    E <- paste0(results_path, "/COBRA_results/COBRA_aggregated.csv")
    F <- "YES"
    G <- 2023
    command <- paste0(executable_path, ' -d ', db_path, ' -b', A, ' -c ', B, ' ', C, ' ', D, ' -o ', E, ' --pct3 ', F, ' -y ', G)
    path <- paste0(getwd(), "/inputs/air_quality/COBRA_batch_aggregated.bat")
    write(command, file = path)
    path <- paste0(getwd(), "/inputs/air_quality/COBRA_batch_aggregated.bat")
    print("Starting COBRA - aggregated mode calculation")
    shell(path, wait = TRUE)
    # Import all generated csv files from COBRA and calculate the cumulative benefits by state and in total
    results_files <- list.files(path = paste0(results_path, "/COBRA_results/"), pattern = "COBRA_aggregated")
    cobra_output_file <- data.frame()
    for (i in 1:length(results_files)) {
      path <- paste0(results_path, "/COBRA_results/", results_files[i])
      temp <- read.csv(path)
      temp <- temp %>%
        add_column(state_id = NA)
      for (j in 1:length(states_correspondence$COBRA_code)) {
        temp$state_id[which(temp$Destination%in%list_states_COBRA$SOURCEINDX[which(list_states_COBRA$STFIPS == states_correspondence$COBRA_code[j])])] <- states_correspondence$COBRA_code[j]
      }
      if (i == 1) {
        cobra_output_file <- temp
      } else {
        cobra_output_file <- rbind(cobra_output_file, temp)
      } 
    }
  }
  if (calculation_mode == "pollutants_breakdown" | calculation_mode == "all") {
    print("Generating batch files for COBRA - Breakdown mode")
    m <- 0
    for (i in pollutants) {
      for (j in sectors) {
        m <- m+1
        executable_path <- '"C:/Program Files/COBRA/cobra_console.exe"'
        db_path <- '"C:/Program Files/COBRA/data/cobra.db"'
        A <- paste0(getwd(), "/inputs/air_quality/cobra_inputs/COBRA_emissions_baseline_breakdown_", i, "_", j, ".csv")
        B <- paste0(getwd(), "/inputs/air_quality/cobra_inputs/COBRA_emissions_scenario_breakdown_", i, "_", j, ".csv")
        C <- paste0(getwd(), "/inputs/air_quality/cobra_population/COBRA_population_data_", last_yr, ".csv")
        D <- paste0(getwd(), "/inputs/air_quality/COBRA_default_incidence_data.csv")
        E <- paste0(results_path, "/COBRA_results/COBRA_breakdown_", i, "_", j, ".csv")
        F <- "YES"
        G <- 2023
        command <- paste0(executable_path, ' -d ', db_path, ' -b', A, ' -c ', B, ' ', C, ' ', D, ' -o ', E, ' --pct3 ', F, ' -y ', G)
        path <- paste0(getwd(), "/inputs/air_quality/COBRA_batch_pollutants_breakdown.bat")
        if (m == 1) {
          write(command, file = path)
        } else {
          write(command, file = path, append = TRUE)
        }
      }
    }
    path <- paste0(getwd(), "/inputs/air_quality/COBRA_batch_pollutants_breakdown.bat")
    print("Starting COBRA - breakdown mode calculation")
    shell(path, wait = TRUE)
    # Import all generated csv files from COBRA and calculate the cumulative benefits by state and in total
    results_files <- list.files(path = paste0(results_path, "/COBRA_results/"), pattern = "COBRA_breakdown_")
    cobra_output_file <- data.frame()
    for (i in 1:length(results_files)) {
      pollutant <- str_split(results_files[i], "_")[[1]][3]
      sector <- str_split(str_split(results_files[i], "_")[[1]][4], ".csv")[[1]][1]
      path <- paste0(results_path, "/COBRA_results/", results_files[i])
      temp <- read.csv(path)
      temp <- temp %>%
        add_column(state_id = NA) %>%
        add_column("Pollutant" = pollutant) %>%
        add_column("Sector" = sector)
      for (j in 1:length(states_correspondence$COBRA_code)) {
        temp$state_id[which(temp$Destination%in%list_states_COBRA$SOURCEINDX[which(list_states_COBRA$STFIPS == states_correspondence$COBRA_code[j])])] <- states_correspondence$COBRA_code[j]
      }
      if (i == 1) {
        cobra_output_file <- temp
      } else {
        cobra_output_file <- rbind(cobra_output_file, temp)
      } 
    }
  }
  # Calculate health benefits
  # Calculate the non-monetized health benefits
  
  # by year and by county + by year and by state + overall health benefits
  cobra_output_file <- add_column(cobra_output_file, 'X..Acute.Myocardial.Infarction..Nonfatal')
  cobra_output_file <- add_column(cobra_output_file, 'X..Mortality..All.Cause')
  cobra_output_file <- add_column(cobra_output_file, 'Acute.Myocardial.Infarction..Nonfatal')
  cobra_output_file <- add_column(cobra_output_file, 'Mortality..All.Cause')
  #cobra_output_file$`"X..Acute.Myocardial.Infarction..Nonfatal"` <- (cobra_output_file$X..Acute.Myocardial.Infarction..Nonfatal..high.+cobra_output_file$X..Acute.Myocardial.Infarction..Nonfatal..low.)
  #cobra_output_file$`"X..Mortality..All.Cause"` <- (cobra_output_file$X..Mortality..All.Cause..low.+cobra_output_file$X..Mortality..All.Cause..high.)
  #cobra_output_file$`"X..Acute.Myocardial.Infarction..Nonfatal"` <- (cobra_output_file$X..Acute.Myocardial.Infarction..Nonfatal..high.+cobra_output_file$X..Acute.Myocardial.Infarction..Nonfatal..low.)/2
  #cobra_output_file$`"X..Mortality..All.Cause"` <- (cobra_output_file$X..Mortality..All.Cause..low.+cobra_output_file$X..Mortality..All.Cause..high.)/2
  cobra_output_file$`"X..Acute.Myocardial.Infarction..Nonfatal"` <- cobra_output_file$X..Acute.Myocardial.Infarction..Nonfatal..high.
  cobra_output_file$`"X..Mortality..All.Cause"` <- cobra_output_file$X..Mortality..All.Cause..high.
  cobra_output_file$`"Acute.Myocardial.Infarction..Nonfatal"` <- cobra_output_file$Acute.Myocardial.Infarction..Nonfatal..high.
  cobra_output_file$`"Mortality..All.Cause"` <- cobra_output_file$Mortality..All.Cause..high.
  # Calculation of the real benefits, i.e. not monetized
  non_monetized_benefits <- cobra_output_file[which(!is.na(cobra_output_file$Destination)),which(!grepl(pattern = "X..", colnames(cobra_output_file)) & !grepl(pattern = "low", colnames(cobra_output_file)) & !grepl(pattern = "high", colnames(cobra_output_file)))]
  non_monetized_benefits <- non_monetized_benefits[,6:length(colnames(non_monetized_benefits))]
  non_monetized_benefits <- select(non_monetized_benefits, -c(state_id))
  non_monetized_benefits <- aggregate(.~year, data = non_monetized_benefits, FUN = sum)
  colnames(non_monetized_benefits) <- c("Year",
                                        "Acute_Bronchitis", 
                                        "Asthma_Exacerbation_Cough",
                                        "Asthma_Exacerbation_Shortness_of_Breath",
                                        "Asthma_Exacerbation_Wheeze",
                                        "Emergency_Room_Visits_Asthma",
                                        "HA_All_Cardiovascular_less_Myocardial_Infarctions",
                                        "HA_All_Respiratory",
                                        "HA_Asthma",
                                        "HA_Chronic_Lung_Disease",
                                        "Lower_Respiratory_Symptoms",
                                        "Minor_Restricted_Activity_Days",
                                        "Infant_Mortality",
                                        "Upper_Respiratory_Symptoms",
                                        "Work_Loss_Days",
                                        "Acute_Myocardial_Infarction_Nonfatal",
                                        "Mortality_All_Cause")
  # Breakdown the monetized benefit by source
  benefits_breakdown <- cobra_output_file[!is.na(cobra_output_file$Destination),]
  benefits_breakdown <- select(benefits_breakdown, c(colnames(cobra_output_file)[which(grepl(pattern = "X..", colnames(cobra_output_file)) & !grepl(pattern = "low", colnames(cobra_output_file)) & !grepl(pattern = "high", colnames(cobra_output_file)))], year))
  benefits_breakdown <- aggregate(.~year, data = benefits_breakdown, FUN = sum)
  colnames(benefits_breakdown) <- c("Year",
                                    "Acute_Bronchitis", 
                                    "Asthma_Exacerbation",
                                    "Emergency_Room_Visits_Asthma",
                                    "CVD_Hosp_Adm",
                                    "Resp_Hosp_Adm",
                                    "Lower_Respiratory_Symptoms",
                                    "Minor_Restricted_Activity_Days",
                                    "Infant_Mortality",
                                    "Upper_Respiratory_Symptoms",
                                    "Work_Loss_Days",
                                    "Acute_Myocardial_Infarction_Nonfatal",
                                    "Mortality_All_Cause")
  if (calculation_mode == "default" | calculation_mode == "all") {
    print("Calculating the health benefits by county")
    FIPS_counties <- get_input_f(input_name = 'COBRA_SOURCEINDX to FIPS crosswalk')
    health_benefits_by_county <- select(cobra_output_file, c(Destination, colnames(cobra_output_file)[which(grepl(pattern = "X..", colnames(cobra_output_file)) & !grepl(pattern = "low", colnames(cobra_output_file)) & !grepl(pattern = "high", colnames(cobra_output_file)))], year)) %>%
      add_column("State" = NA) %>%
      add_column("County" = NA) %>%
      add_column("FIPS" = NA) %>%
      add_column("Total_health_benefits" = NA)
    health_benefits_by_county$Total_health_benefits <- rowSums(cobra_output_file[which(grepl(pattern = "X..", colnames(cobra_output_file)) & !grepl(pattern = "low", colnames(cobra_output_file)) & !grepl(pattern = "high", colnames(cobra_output_file)))])
    health_benefits_by_county <- select(health_benefits_by_county, c("Destination", "year", "State", "County", "FIPS", "Total_health_benefits"))
    for (i in 1:dim(FIPS_counties)[1]) {
      health_benefits_by_county$State[which(health_benefits_by_county$Destination == FIPS_counties$SOURCEINDX[i])] <- FIPS_counties$STNAME[i]
      health_benefits_by_county$County[which(health_benefits_by_county$Destination == FIPS_counties$SOURCEINDX[i])] <- FIPS_counties$CYNAME[i]
      health_benefits_by_county$FIPS[which(health_benefits_by_county$Destination == FIPS_counties$SOURCEINDX[i])] <- FIPS_counties$FIPS[i]
    }
    health_benefits_by_county <- select(health_benefits_by_county, -c("Destination"))
    colnames(health_benefits_by_county)[1] <- c("Year")
    health_benefits_by_state <- data.frame() %>%
      add_column("Year" = NA) %>%
      add_column("State" = NA) %>%
      add_column("Total_health_benefits" = NA)
    health_benefits <- data.frame() %>%
      add_column("Year" = NA) %>%
      add_column("Total_health_benefits" = NA)
    for (i in unique(health_benefits_by_county$FIPS)) {
      temp <- which(health_benefits_by_county$FIPS == i)
      health_benefits_by_county <- add_row(health_benefits_by_county, Year = "0", 
                                           State = unique(health_benefits_by_county$State[temp]),
                                           County = unique(health_benefits_by_county$County[temp]),
                                           FIPS = i,
                                           Total_health_benefits = sum(health_benefits_by_county$Total_health_benefits[temp]))
    }
    health_benefits_by_county <- filter(health_benefits_by_county, !is.na(health_benefits_by_county$FIPS))
    health_benefits_by_county$FIPS[which(health_benefits_by_county$FIPS < 10000)] <- paste0("0", health_benefits_by_county$FIPS[which(health_benefits_by_county$FIPS < 10000)])
    print("Calculating the health benefits by state and nationwide")
    n <- 0
    for (i in (first_proj_yr+1):last_yr) {
      health_benefits <- add_row(health_benefits, Year = i, Total_health_benefits = sum(cobra_output_file[which(is.na(cobra_output_file$Destination) & cobra_output_file$year == i), which(grepl(pattern = "X..", colnames(cobra_output_file)) & !grepl(pattern = "low", colnames(cobra_output_file)) & !grepl(pattern = "high", colnames(cobra_output_file)))]))
      for (j in 1:length(states_correspondence$COBRA_code)) {
        n <- n+1
        health_benefits_by_state <- add_row(health_benefits_by_state, Year = i, State = states_correspondence$COBRA_code[j], Total_health_benefits = sum(cobra_output_file[which(cobra_output_file$state_id == states_correspondence$COBRA_code[j] & cobra_output_file$year == i), which(grepl(pattern = "X..", colnames(cobra_output_file)) & !grepl(pattern = "low", colnames(cobra_output_file)) & !grepl(pattern = "high", colnames(cobra_output_file)))]))
      }
    }
    for (i in 1:length(states_correspondence$COBRA_code)) {
      temp <- c(0, states_correspondence$COBRA_code[i], sum(health_benefits_by_state$Total_health_benefits[which(health_benefits_by_state$State == states_correspondence$COBRA_code[i])]))
      health_benefits_by_state <- rbind(health_benefits_by_state, temp)
      health_benefits_by_state$State[which(health_benefits_by_state$State == states_correspondence$COBRA_code[i])] <- states_correspondence$name_state[i]
    }
    # Add the discounted benefits
    health_benefits <- add_column(health_benefits, "Cumulative_before_discount_benefits" = NA)
    health_benefits <- add_column(health_benefits, "Cumulative_benefits" = NA)
    health_benefits <- add_column(health_benefits, "Total_benefits" = NA)
    health_benefits_by_state <- add_column(health_benefits_by_state, "Cumulative_before_discount_benefits" = NA)
    health_benefits_by_state <- add_column(health_benefits_by_state, "Cumulative_benefits" = NA)
    health_benefits_by_state <- add_column(health_benefits_by_state, "Total_benefits" = NA)
    health_benefits_by_county <- add_column(health_benefits_by_county, "Cumulative_before_discount_benefits" = NA)
    health_benefits_by_county <- add_column(health_benefits_by_county, "Cumulative_benefits" = NA)
    health_benefits_by_county <- add_column(health_benefits_by_county, "Total_benefits" = NA)
    years <- unique(health_benefits$Year)
    states <- unique(health_benefits_by_state$State)
    counties <- unique(health_benefits_by_county$FIPS)
    for (i in 1:length(years)) {
      if (i > 1) {
        health_benefits$Cumulative_before_discount_benefits[i] <- health_benefits$Cumulative_before_discount_benefits[i-1]+health_benefits$Total_health_benefits[i]
        health_benefits$Cumulative_benefits[i] <- health_benefits$Cumulative_before_discount_benefits[i]*1/((1+discount_rate/100)^(i))
        health_benefits$Total_benefits[i] <- health_benefits$Total_benefits[i-1]+health_benefits$Cumulative_benefits[i]
      } else {
        health_benefits$Cumulative_before_discount_benefits[i] <- health_benefits$Total_health_benefits[i]
        health_benefits$Cumulative_benefits[i] <- health_benefits$Cumulative_before_discount_benefits[i]*1/((1+discount_rate/100)^(i))
        health_benefits$Total_benefits[i] <- health_benefits$Cumulative_benefits[i]
      }
      for (j in 1:length(states)) {
        index_state <- which(health_benefits_by_state$State == states[j] & health_benefits_by_state$Year == years[i])
        index_state_previous <- which(health_benefits_by_state$State == states[j] & health_benefits_by_state$Year == years[i-1])
        if (i > 1) {
          health_benefits_by_state$Cumulative_before_discount_benefits[index_state] <- health_benefits_by_state$Cumulative_before_discount_benefits[index_state_previous]+health_benefits_by_state$Total_health_benefits[index_state]
          health_benefits_by_state$Cumulative_benefits[index_state] <- health_benefits_by_state$Cumulative_before_discount_benefits[index_state]*1/((1+discount_rate/100)^(i))
          health_benefits_by_state$Total_benefits[index_state] <- health_benefits_by_state$Total_benefits[index_state_previous] + health_benefits_by_state$Cumulative_benefits[index_state]
        } else {
          health_benefits_by_state$Cumulative_before_discount_benefits[index_state] <- health_benefits_by_state$Total_health_benefits[index_state]
          health_benefits_by_state$Cumulative_benefits[index_state] <- health_benefits_by_state$Cumulative_before_discount_benefits[index_state]*1/((1+discount_rate/100)^(i))
          health_benefits_by_state$Total_benefits[index_state] <- health_benefits_by_state$Cumulative_benefits[index_state]
        }
      }
      for (j in 1:length(counties)) {
        index_county <- which(health_benefits_by_county$FIPS == counties[j] & health_benefits_by_county$Year == years[i])
        index_county_previous <- which(health_benefits_by_county$FIPS == counties[j] & health_benefits_by_county$Year == years[i-1])
        if (i > 1) {
          health_benefits_by_county$Cumulative_before_discount_benefits[index_county] <- health_benefits_by_county$Cumulative_before_discount_benefits[index_county_previous]+health_benefits_by_county$Total_health_benefits[index_county]
          health_benefits_by_county$Cumulative_benefits[index_county] <- health_benefits_by_county$Cumulative_before_discount_benefits[index_county]*1/((1+discount_rate/100)^(i))
          health_benefits_by_county$Total_benefits[index_county] <- health_benefits_by_county$Total_benefits[index_county_previous] + health_benefits_by_county$Cumulative_benefits[index_county]
        } else {
          health_benefits_by_county$Cumulative_before_discount_benefits[index_county] <- health_benefits_by_county$Total_health_benefits[index_county]
          health_benefits_by_county$Cumulative_benefits[index_county] <- health_benefits_by_county$Cumulative_before_discount_benefits[index_county]*1/((1+discount_rate/100)^(i))
          health_benefits_by_county$Total_benefits[index_county] <- health_benefits_by_county$Cumulative_benefits[index_county]
        }
      }
    }
    health_benefits <- add_row(health_benefits, Year = 0, Total_health_benefits = sum(health_benefits$Total_health_benefits), Cumulative_before_discount_benefits = sum(health_benefits$Cumulative_before_discount_benefits), Cumulative_benefits = sum(health_benefits$Cumulative_benefits), Total_benefits = sum(health_benefits$Total_benefits))
    # Export the health benefits files
    print("Exporting the health benefits files")
    write.csv(health_benefits, paste0(results_path, "/health_benefits.csv"))
    write.csv(health_benefits_by_state, paste0(results_path, "/health_benefits_by_state.csv"))
    write.csv(health_benefits_by_county, paste0(results_path, "/health_benefits_by_county.csv"))
    write.csv(non_monetized_benefits, paste0(results_path, "/non_monetized_health_benefits.csv"))
    write.csv(benefits_breakdown, paste0(results_path, "/monetized_benefits_breakdown.csv"))
    print("Health impact files generated")
  } 
  if (calculation_mode == "aggregated" | calculation_mode == "all") {
    print("Calculating the health benefits by county")
    FIPS_counties <- get_input_f(input_name = 'COBRA_SOURCEINDX to FIPS crosswalk')
    health_benefits_by_county <- select(cobra_output_file, c(Destination, colnames(cobra_output_file)[which(grepl(pattern = "X..", colnames(cobra_output_file)) & !grepl(pattern = "low", colnames(cobra_output_file)) & !grepl(pattern = "high", colnames(cobra_output_file)))])) %>%
      add_column("State" = NA) %>%
      add_column("County" = NA) %>%
      add_column("FIPS" = NA) %>%
      add_column("Total_health_benefits" = NA)
    health_benefits_by_county$Total_health_benefits <- rowSums(cobra_output_file[which(grepl(pattern = "X..", colnames(cobra_output_file)) & !grepl(pattern = "low", colnames(cobra_output_file)) & !grepl(pattern = "high", colnames(cobra_output_file)))])
    health_benefits_by_county <- select(health_benefits_by_county, c("Destination", "State", "County", "FIPS", "Total_health_benefits"))
    for (i in 1:dim(FIPS_counties)[1]) {
      health_benefits_by_county$State[which(health_benefits_by_county$Destination == FIPS_counties$SOURCEINDX[i])] <- FIPS_counties$STNAME[i]
      health_benefits_by_county$County[which(health_benefits_by_county$Destination == FIPS_counties$SOURCEINDX[i])] <- FIPS_counties$CYNAME[i]
      health_benefits_by_county$FIPS[which(health_benefits_by_county$Destination == FIPS_counties$SOURCEINDX[i])] <- FIPS_counties$FIPS[i]
    }
    health_benefits_by_county <- select(health_benefits_by_county, -c("Destination"))
    colnames(health_benefits_by_county)[1] <- c("State")
    #for (i in unique(health_benefits_by_county$FIPS)) {
    #  temp <- which(health_benefits_by_county$FIPS == i)
    #  health_benefits_by_county <- add_row(health_benefits_by_county, Year = "0", 
    #                                       State = unique(health_benefits_by_county$State[temp]),
    #                                       County = unique(health_benefits_by_county$County[temp]),
    #                                       FIPS = i,
    #                                       Total_health_benefits = sum(health_benefits_by_county$Total_health_benefits[temp]))
    #}
    health_benefits_by_county <- filter(health_benefits_by_county, !is.na(health_benefits_by_county$FIPS))
    health_benefits_by_county$FIPS[which(health_benefits_by_county$FIPS < 10000)] <- paste0("0", health_benefits_by_county$FIPS[which(health_benefits_by_county$FIPS < 10000)])
    print("Calculating the health benefits by state and nationwide")
    health_benefits_by_state <- aggregate(Total_health_benefits ~ State, data = select(health_benefits_by_county, -c("County", "FIPS")), FUN = sum) %>%
      add_column("Output_Type" = "Aggregated")
    health_benefits <- aggregate(Total_health_benefits ~ Output_Type , data = select(health_benefits_by_state, -c("State")), FUN = sum)
    write.csv(health_benefits, paste0(results_path, "/health_benefits_aggregated.csv"))
    write.csv(health_benefits_by_state, paste0(results_path, "/health_benefits_by_state_aggregated.csv"))
    write.csv(health_benefits_by_county, paste0(results_path, "/health_benefits_by_county_aggregated.csv"))
  }
  if (calculation_mode == "pollutants_breakdown" | calculation_mode == "all") {
    print("Calculating the health benefits by county")
    FIPS_counties <- get_input_f(input_name = 'COBRA_SOURCEINDX to FIPS crosswalk')
    health_benefits_by_county <- select(cobra_output_file, c(Destination, colnames(cobra_output_file)[which(grepl(pattern = "X..", colnames(cobra_output_file)) & !grepl(pattern = "low", colnames(cobra_output_file)) & !grepl(pattern = "high", colnames(cobra_output_file)))], Pollutant, Sector)) %>%
      add_column("State" = NA) %>%
      add_column("County" = NA) %>%
      add_column("FIPS" = NA) %>%
      add_column("Total_health_benefits" = NA)
    health_benefits_by_county$Total_health_benefits <- rowSums(cobra_output_file[which(grepl(pattern = "X..", colnames(cobra_output_file)) & !grepl(pattern = "low", colnames(cobra_output_file)) & !grepl(pattern = "high", colnames(cobra_output_file)))])
    health_benefits_by_county <- select(health_benefits_by_county, c("Destination", "State", "County", "FIPS", "Pollutant", "Sector", "Total_health_benefits"))
    for (i in 1:dim(FIPS_counties)[1]) {
      health_benefits_by_county$State[which(health_benefits_by_county$Destination == FIPS_counties$SOURCEINDX[i])] <- FIPS_counties$STNAME[i]
      health_benefits_by_county$County[which(health_benefits_by_county$Destination == FIPS_counties$SOURCEINDX[i])] <- FIPS_counties$CYNAME[i]
      health_benefits_by_county$FIPS[which(health_benefits_by_county$Destination == FIPS_counties$SOURCEINDX[i])] <- FIPS_counties$FIPS[i]
    }
    health_benefits_by_county <- select(health_benefits_by_county, -c("Destination"))
    colnames(health_benefits_by_county)[1] <- c("State")
    #for (i in unique(health_benefits_by_county$FIPS)) {
    #  temp <- which(health_benefits_by_county$FIPS == i)
    #  health_benefits_by_county <- add_row(health_benefits_by_county, Year = "0", 
    #                                       State = unique(health_benefits_by_county$State[temp]),
    #                                       County = unique(health_benefits_by_county$County[temp]),
    #                                       FIPS = i,
    #                                       Total_health_benefits = sum(health_benefits_by_county$Total_health_benefits[temp]))
    #}
    health_benefits_by_county <- filter(health_benefits_by_county, !is.na(health_benefits_by_county$FIPS))
    health_benefits_by_county$FIPS[which(health_benefits_by_county$FIPS < 10000)] <- paste0("0", health_benefits_by_county$FIPS[which(health_benefits_by_county$FIPS < 10000)])
    print("Calculating the health benefits by state and nationwide")
    health_benefits_by_state <- aggregate(Total_health_benefits ~ State + Pollutant + Sector, data = select(health_benefits_by_county, -c("County", "FIPS")), FUN = sum) %>%
      add_column("Output_Type" = "Polutants_Breakdown")
    health_benefits <- aggregate(Total_health_benefits ~ Output_Type + Pollutant + Sector , data = select(health_benefits_by_state, -c("State")), FUN = sum)
    write.csv(health_benefits, paste0(results_path, "/health_benefits_pollutants_breakdown.csv"))
    write.csv(health_benefits_by_state, paste0(results_path, "/health_benefits_by_state_pollutants_breakdown.csv"))
    write.csv(health_benefits_by_county, paste0(results_path, "/health_benefits_by_county_pollutants_breakdown.csv"))
  }
  health_impacts <- list(health_benefits = health_benefits, 
                         health_benefits_by_state = health_benefits_by_state,
                         health_benefits_by_county = health_benefits_by_county)
  return(health_impacts)
}
