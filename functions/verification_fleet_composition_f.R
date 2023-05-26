# This function verifies the fleet composition dataset and write in the log file
verification_fleet_composition_f <- function(fleet, fleet_composition) {
  path_log <- paste0(results_path, "/logfile.txt")
  write(paste0(Sys.time(), " - Validation of the fleet dataset"), file = path_log, append = TRUE)
  scenario_entire_us <- fleet_read_scenario_f(fleet_id = NA)
  aeo_dataset <- fleet$get_list_dataframe()[["fleet_vint_stock"]]
  flameaq_dataset <- fleet_composition[["fleet_vint_stock"]]
  flameaq_dataset_by_state <- fleet_composition_state_breakdown[["fleet_vint_stock_scenario_state_breakdown"]]
  # Growth rate 
  if (dim(filter(scenario_entire_us, scenario_entire_us$Event == "growth_rate"))[1] == 0) {
    # Conformity of the yearly fleet size
    years <- unique(aeo_dataset$Year)
    for (i in years) {
      bool <- TRUE
      bool_st <- TRUE
      fleet_comparison <- sum(aeo_dataset$Value[which(aeo_dataset$Year == i)])/sum(flameaq_dataset$Value[which(flameaq_dataset$Year == i)])
      fleet_comparison_by_state <- sum(aeo_dataset$Value[which(aeo_dataset$Year == i)])/sum(flameaq_dataset_by_state$Value[which(flameaq_dataset_by_state$Year == i)])
      if (abs(1-fleet_comparison) > 0.001) {
        bool <- FALSE
      }
      if (abs(1-fleet_comparison_by_state) > 0.001) {
        bool_st <- FALSE
      }
    }
    if (isTRUE(bool)) {
      write("Yearly size of the fleet - OK", file = path_log, append = TRUE)
    } else {
      write(paste0("Yearly size of the fleet - ERROR - Year ", i), file = path_log, append = TRUE)
    }
    if (isTRUE(bool_st)) {
      write("Yearly size of the fleet by state - OK", file = path_log, append = TRUE)
    } else {
      write(paste0("Yearly size of the fleet by state - ERROR - Year ", i), file = path_log, append = TRUE)
    }
  } else {
    write(paste0("WARNING: Automatic verification not implemented for a custom growth rate"), file = path_log, append = TRUE)
  }
  # Road ban
  if (dim(filter(scenario_entire_us, scenario_entire_us$Event == "road_ban"))[1] != 0) {
    write(paste0("WARNING: Automatic verification not implemented for a road bans"), file = path_log, append = TRUE)
  }
  # Sales share
  if (dim(filter(scenario_entire_us, scenario_entire_us$Event == "sales_share"))[1] == 0) {
    
  } else {
    
  }
  
  # Sales ban
  if (dim(filter(scenario_entire_us, scenario_entire_us$Event == "sales_ban"))[1] == 0) {
    
  } else {
    
  }
  
  # Tech share
  if (dim(filter(scenario_entire_us, scenario_entire_us$Event == "tech_share"))[1] == 0) {
    
  } else {
    
  }
  

  
  
  
  
  # Conformity of the technology market share
  

  # Conformity of the size market share
  
  # Conformity of the geographical distribution
  
}
