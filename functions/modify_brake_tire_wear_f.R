modify_brake_tire_wear_f <- function(normalized_total_emissions, normalized_emissions_by_state, brake_tire_emissions_modification = NA) {
  attribute_f("modify_brake_tire_wear_f")
  BTW_scenario <- get_input_f(input_name = paste0("BTW_", brake_tire_emissions_modification))
  normalized_total_emissions_temp <- normalized_total_emissions
  normalized_emissions_by_state_temp <- normalized_emissions_by_state
  for (i in 1:dim(BTW_scenario)[1]) {
    temp <- which(normalized_total_emissions_temp$Source == BTW_scenario$Size[i] & normalized_total_emissions_temp$Fuel == BTW_scenario$Fuel[i])
    temp_state <- which(normalized_emissions_by_state_temp$Source == BTW_scenario$Size[i] & normalized_emissions_by_state_temp$Fuel == BTW_scenario$Fuel[i])
    normalized_total_emissions_temp[temp, which(colnames(normalized_total_emissions_temp) == BTW_scenario$Pollutant[i])] <- normalized_total_emissions[temp, which(colnames(normalized_total_emissions_temp) == BTW_scenario$Pollutant[i])]*BTW_scenario$Factor[i]
    normalized_emissions_by_state_temp[temp, which(colnames(normalized_emissions_by_state_temp) == BTW_scenario$Pollutant[i])] <- normalized_emissions_by_state[temp, which(colnames(normalized_emissions_by_state_temp) == BTW_scenario$Pollutant[i])]*BTW_scenario$Factor[i]
  }
  normalized_total_emissions <- normalized_total_emissions_temp
  normalized_emissions_by_state <- normalized_emissions_by_state_temp
  return(list(normalized_total_emissions = normalized_total_emissions, normalized_emissions_by_state = normalized_emissions_by_state))
}
