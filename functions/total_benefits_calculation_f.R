total_benefits_calculation_f <- function(cost_of_carbon = NA, scenario_id = NA, calculation_mode = NA) {
  attribute_f("total_benefits_calculation_f")
  results_path <- paste0(getwd(), "/outputs/air_quality/", Sys.Date(), "_", scenario_id, "_results")
  print("Creation of the results folder")
  dir.create(paste0(getwd(), "/outputs/air_quality/", Sys.Date(), "_", scenario_id, "_results"))
  print("Creation of the log file")
  path_log <- paste0(results_path, "/logfile.txt")
  write(paste0(Sys.time(), " - Calculation started"), file = path_log)
  print("Calculation of the GHG emissions")
  fleet_gwp <- do.call(fleet_gwp_f, list())[["fleet_gwp"]]
  write.csv(fleet_gwp, paste0(results_path, "/fleet_GHG_emissions.csv"))
  write(paste0(Sys.time(), " - Emissions of greenhouse gases done"), file = path_log, append = TRUE)
  print("Calculation of the health impacts from changes in pollutants emissions")
  health_impacts <- do.call(FLAME_AQ_f, list())[["health_benefits"]]
  print("Generation of the final results files")
  yearly_fleet_gwp <- data.frame(unique(fleet_gwp$Year)) %>%
    add_column("Value" = 0) %>%
    add_column("Unit" = "tons CO2 eq.") %>%
    add_column("Cost" = 0) %>%
    add_column("Emissions_Changes" = 0) %>%
    add_column("Benefits" = 0)
  colnames(yearly_fleet_gwp)[1] <- "Year"
  for (i in unique(fleet_gwp$Year)) {
    temp <- which(fleet_gwp$Year == i)
    temp2 <- which(yearly_fleet_gwp == i)
    yearly_fleet_gwp$Value[temp2] <- sum(fleet_gwp$Value[temp])/1000
    yearly_fleet_gwp$Cost[temp2] <- yearly_fleet_gwp$Value[temp2]*cost_of_carbon
    if (i > min(unique(fleet_gwp$Year))) {
      yearly_fleet_gwp$Emissions_Changes[temp2] <- yearly_fleet_gwp$Value[temp2]-yearly_fleet_gwp$Value[which(yearly_fleet_gwp$Year == i-1)]
      yearly_fleet_gwp$Benefits[temp2] <- -(yearly_fleet_gwp$Cost[temp2]-yearly_fleet_gwp$Cost[which(yearly_fleet_gwp$Year == i-1)])
    }
  }
  if (calculation_mode == "default" | calculation_mode == "all") {
    total_benefits <- c(health_impacts$Year, health_impacts$Total_health_benefits) %>%
      add_column("Total_climate_benefits" = 0) %>%
      add_column("Total_benefits" = 0) %>% 
      filter(Year != 0)
    for (i in unique(total_benefits$Year)) {
      total_benefits$Total_climate_benefits[which(total_benefits$Year == i)] <- yearly_fleet_gwp$Benefits[which(yearly_fleet_gwp$Year == i)]
    }
    total_benefits$Total_benefits <- total_benefits$Total_health_benefits + total_benefits$Total_climate_benefits
    total_benefits[nrow(total_benefits)+1,] <- c(0, sum(total_benefits$Total_health_benefits), sum(total_benefits$Total_climate_benefits), sum(total_benefits$Total_benefits))
    print("Exportation of the final results")
    write.csv(total_benefits, paste0(results_path, "/total_benefits.csv"))
    print("Done!")
    return(list(total_benefits = total_benefits))
  }
}

