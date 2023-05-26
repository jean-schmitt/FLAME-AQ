# Code to verify the input files for COBRA
verification_cobra_datasets_f <- function(state_resolved_fleet_direct_emissions, fleet_emissions_elec_state, fleet_fuel_usage_US) {
  path_log <- paste0(results_path, "/logfile.txt")
  write(paste0(Sys.time(), " - Validation of the COBRA input files"), file = path_log, append = TRUE)
  scenario_files <- list.files(path = paste0(results_path, "/COBRA_scenario_files/"))
  baseline_files <- list.files(path = paste0(results_path, "/COBRA_baseline_files/"))
  if (grepl("aggregated", scenario_files)[1]) {
    scenario <- read.csv(paste0(results_path, "/COBRA_scenario_files/", scenario_files))
    #baseline <- read.csv(paste0(results_path, "/COBRA_baseline_files/", baseline_files))
    error_NOx_fleet <- FALSE
    error_SO2_fleet <- FALSE
    error_PM25_fleet <- FALSE
    error_NOx_elec <- FALSE
    error_SO2_elec <- FALSE
    error_PM25_elec <- FALSE
    NOx_fleet <- abs(1-sum(state_resolved_fleet_direct_emissions$NOx[which(state_resolved_fleet_direct_emissions$Year == 2050)])/sum(scenario$NO2[which(scenario$TIER1 == 11 & scenario$TIER3 == 2)]))
    SO2_fleet <- abs(1-sum(state_resolved_fleet_direct_emissions$SO2[which(state_resolved_fleet_direct_emissions$Year == 2050)])/sum(scenario$SO2[which(scenario$TIER1 == 11 & scenario$TIER3 == 2)]))
    PM25_fleet <- abs(1-sum(state_resolved_fleet_direct_emissions$Total_PM25[which(state_resolved_fleet_direct_emissions$Year == 2050)]+state_resolved_fleet_direct_emissions$Brake_PM25[which(state_resolved_fleet_direct_emissions$Year == 2050)]+state_resolved_fleet_direct_emissions$Tire_PM25[which(state_resolved_fleet_direct_emissions$Year == 2050)])/sum(scenario$PM25[which(scenario$TIER1 == 11 & scenario$TIER3 == 2)]))
    NOx_elec <- abs(sum(fleet_emissions_elec_state$NOx[which(fleet_emissions_elec_state$Year == 2050)])/sum(scenario$NO2[which(scenario$TIER1 == 1)]))
    SO2_elec <- abs(sum(fleet_emissions_elec_state$SO2[which(fleet_emissions_elec_state$Year == 2050)])/sum(scenario$SO2[which(scenario$TIER1 == 1)]))
    PM25_elec <- abs(sum(fleet_emissions_elec_state$PM25[which(fleet_emissions_elec_state$Year == 2050)])/sum(scenario$PM25[which(scenario$TIER1 == 1)]))
  } else {
    years <- unique(state_resolved_fleet_direct_emissions$Year[which(state_resolved_fleet_direct_emissions$Year > 2022)])
    error_NOx_fleet <- FALSE
    error_SO2_fleet <- FALSE
    error_PM25_fleet <- FALSE
    error_NOx_elec <- FALSE
    error_SO2_elec <- FALSE
    error_PM25_elec <- FALSE
    for (i in years) {
      scenario <- read.csv(paste0(results_path, "/COBRA_scenario_files/", scenario_files[grep(i, scenario_files)]))
      #baseline <- read.csv(paste0(results_path, "/COBRA_baseline_files/", baseline_files[grep(i, baseline_files)]))
      NOx_fleet <- abs(1-sum(state_resolved_fleet_direct_emissions$NOx[which(state_resolved_fleet_direct_emissions$Year == i)])/sum(scenario$NO2[which(scenario$TIER1 == 11 & scenario$TIER3 == 2)]))
      SO2_fleet <- abs(1-sum(state_resolved_fleet_direct_emissions$SO2[which(state_resolved_fleet_direct_emissions$Year == i)])/sum(scenario$SO2[which(scenario$TIER1 == 11 & scenario$TIER3 == 2)]))
      PM25_fleet <- abs(1-sum(state_resolved_fleet_direct_emissions$Total_PM25[which(state_resolved_fleet_direct_emissions$Year == i)]+state_resolved_fleet_direct_emissions$Brake_PM25[which(state_resolved_fleet_direct_emissions$Year == i)]+state_resolved_fleet_direct_emissions$Tire_PM25[which(state_resolved_fleet_direct_emissions$Year == i)])/sum(scenario$PM25[which(scenario$TIER1 == 11 & scenario$TIER3 == 2)]))
      NOx_elec <- abs(sum(fleet_emissions_elec_state$NOx[which(fleet_emissions_elec_state$Year == i)])/sum(scenario$NO2[which(scenario$TIER1 == 1)]))
      SO2_elec <- abs(sum(fleet_emissions_elec_state$SO2[which(fleet_emissions_elec_state$Year == i)])/sum(scenario$SO2[which(scenario$TIER1 == 1)]))
      PM25_elec <- abs(sum(fleet_emissions_elec_state$PM25[which(fleet_emissions_elec_state$Year == i)])/sum(scenario$PM25[which(scenario$TIER1 == 1)]))
      if (NOx_fleet > 0.02) {
        error_NOx_fleet <- TRUE
      }
      if (SO2_fleet > 0.02) {
        error_SO2_fleet <- TRUE
      }
      if (PM25_fleet > 0.02) {
        error_PM25_fleet <- TRUE
      }
      if (NOx_elec > 0.02) {
        error_NOx_elec <- TRUE
      }
      if (SO2_elec > 0.02) {
        error_SO2_elec <- TRUE
      }
      if (PM25_elec > 0.02) {
        error_PM25_elec <- TRUE
      }
    }
    if (isTRUE(error_NOx_fleet)) {
      write("Fleet emissions of NOx - OK", file = path_log, append = TRUE)
    } else {
      write("Fleet emissions of NOx - ERROR", file = path_log, append = TRUE)
    }
    if (isTRUE(error_SO2_fleet)) {
      write("Fleet emissions of SO2 - OK", file = path_log, append = TRUE)
    } else {
      write("Fleet emissions of SO2 - ERROR", file = path_log, append = TRUE)
    }
    if (isTRUE(error_PM25_fleet)) {
      write("Fleet emissions of PM25 - OK", file = path_log, append = TRUE)
    } else {
      write("Fleet emissions of PM25 - ERROR", file = path_log, append = TRUE)
    }
    if (isTRUE(error_NOx_elec)) {
      write("Electricity emissions of NOx - OK", file = path_log, append = TRUE)
    } else {
      write("Electricity emissions of NOx - ERROR", file = path_log, append = TRUE)
    }
    if (isTRUE(error_SO2_elec)) {
      write("Electricity emissions of SO2 - OK", file = path_log, append = TRUE)
    } else {
      write("Electricity emissions of SO2 - ERROR", file = path_log, append = TRUE)
    }
    if (isTRUE(error_PM25_elec)) {
      write("Electricity emissions of PM25 - OK", file = path_log, append = TRUE)
    } else {
      write("Electricity emissions of PM25 - ERROR", file = path_log, append = TRUE)
    }
  }
}
