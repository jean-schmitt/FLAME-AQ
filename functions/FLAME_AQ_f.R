FLAME_AQ_f <- function(scenario_id = NA) {
  attribute_f("FLAME_AQ_f")
  print("Initialization of the FLAME Air Quality module")
  print("Creation of the geographical correspondance file")
  do.call(GIS_matching_f, list())
  print("Calculation of the electricity production by county")
  do.call(county_level_elec_prod_f, list())
  fleet_dataset <- do.call(fun_res_f,list(fun_name="fleet_fuel_u_f"))
  do.call(air_quality_module_f, list(fleet_dataset = fleet_dataset))
}
