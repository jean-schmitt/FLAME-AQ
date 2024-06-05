fleet_read_scenario_f <- function(fleet_id = NA) {
  attribute_f("fleet_read_scenario_f")
  if (fleet_id == "no_ldvs") {
    scenario <- read.delim(paste0(getwd(), "/inputs/fleet_scenarios/", "no_EVs", ".txt"), header = TRUE, sep = ";", dec = ".")
  } else {
    scenario <- read.delim(paste0(getwd(), "/inputs/fleet_scenarios/", fleet_id, ".txt"), header = TRUE, sep = ";", dec = ".")
  }
  return(scenario)
}
