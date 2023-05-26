electric_read_scenario_f <- function(electric_grid_scenario = NA) {
  attribute_f("electric_read_scenario_f")
  scenario <- read.delim(paste0(getwd(), "/inputs/electricity_scenarios/", electric_grid_scenario, ".txt"), header = TRUE, sep = ";", dec = ".")
  return(scenario)
}
