fleet_read_scenario_f <- function(fleet_id = NA) {
  attribute_f("fleet_read_scenario_f")
  scenario <- read.delim(paste0(getwd(), "/inputs/fleet_scenarios/", fleet_id, ".txt"), header = TRUE, sep = ";", dec = ".")
  return(scenario)
}
