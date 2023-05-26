FLAME_AQ_f <- function(scenario_id = NA) {
  attribute_f("FLAME_AQ_f")
  print("Initialization of the FLAME Air Quality module")
  print("Creation of the geographical correspondance file")
  do.call(GIS_matching_f, list())
  print("Calculation of the electricity production by county")
  do.call(county_level_elec_prod_f, list())
  fleet_dataset <- do.call(fun_res_f,list(fun_name="fleet_fuel_u_f"))
  fleet_composition <- list(fleet_vint_stock = fleet_dataset[["fleet_composition"]][["fleet_vint_stock"]], 
                            fleet_vint_scrap = fleet_dataset[["fleet_composition"]][["fleet_vint_scrap"]])
  fleet_composition_state_breakdown <- list(fleet_vint_stock_scenario_state_breakdown = fleet_dataset[["fleet_composition"]][["fleet_vint_stock_scenario_state_breakdown"]], 
                                            fleet_vint_scrap_scenario_state_breakdown = fleet_dataset[["fleet_composition"]][["fleet_vint_scrap_scenario_state_breakdown"]])
  fleet_elec_use_tot_state <- fleet_dataset[["fleet_elec_use_tot_state"]]
  fleet_elec_use_tot_county <- fleet_dataset[["fleet_elec_use_tot_county"]]
  fleet_fuel_use_tot <- fleet_dataset[["fleet_fuel_use_tot"]]
  fleet_fuel_use_tot_state <- fleet_dataset[["fleet_fuel_use_tot_state"]]
  health_impacts <- do.call(air_quality_module_f, list(fleet_composition_state_breakdown = fleet_composition_state_breakdown, 
                                                       fleet_composition = fleet_composition, 
                                                       fleet_elec_use_tot_state = fleet_elec_use_tot_state, 
                                                       fleet_elec_use_tot_county = fleet_elec_use_tot_county,
                                                       fleet_fuel_use_tot = fleet_fuel_use_tot,
                                                       fleet_fuel_use_tot_state=fleet_fuel_use_tot_state))
  return(health_impacts)
}
