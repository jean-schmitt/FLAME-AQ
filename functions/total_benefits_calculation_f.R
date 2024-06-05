total_benefits_calculation_f <- function(cost_of_carbon = NA, scenario_id = NA, calculation_mode = NA, discount_rate = NA) {
  attribute_f("total_benefits_calculation_f")
  results_path <- paste0(getwd(), "/outputs/air_quality/", Sys.Date(), "_", scenario_id, "_results")
  print("Creation of the results folder")
  dir.create(paste0(getwd(), "/outputs/air_quality/", Sys.Date(), "_", scenario_id, "_results"))
  print("Creation of the log file")
  path_log <- paste0(results_path, "/logfile.txt")
  write(paste0(Sys.time(), " - Calculation started"), file = path_log)
  print("Calculation of the GHG emissions")
  fleet_gwp <- do.call(fleet_gwp_f, list())[["fleet_gwp"]]
  write.csv(fleet_gwp, paste0(results_path, "/fleet_GHG_emissions_details.csv"))
  fleet_gwp_aggregated <- aggregate(.~Sector, data = select(fleet_gwp, c("Sector", "Value")), FUN = sum)
  write.csv(fleet_gwp_aggregated, paste0(results_path, "/fleet_GHG_emissions.csv"))
  write(paste0(Sys.time(), " - Emissions of greenhouse gases done"), file = path_log, append = TRUE)
  print("Calculation of the health impacts from changes in pollutants emissions")
  do.call(FLAME_AQ_f, list())[["health_benefits"]]
  #emissions_reference <- sum(fleet_gwp$Value[which(fleet_gwp$Year == 2022)])/1000
  #fleet_gwp <- fleet_gwp[which(fleet_gwp$Year >= 2023),] %>%
  #  select(c(Year, Value))
  #fleet_gwp <- aggregate(Value/1000 ~ Year, data = fleet_gwp, FUN = sum) %>%
  #  add_column("Reference_emissions" = emissions_reference) %>%
  #  add_column("Emissions_difference" = 0) %>%
  #  add_column("Emissions_Unit" = "tons CO2 eq.") %>%
  #  add_column("Emissions_valuation" = 0) %>%
  #  add_column("Discounted_emissions" = 0) %>%
  #  add_column("Valuation_Unit" = "USD") 
  #fleet_gwp$Emissions_difference <- fleet_gwp$Reference_emissions-fleet_gwp$Value
  #for (i in 1:length(fleet_gwp$Year)) {
  #  fleet_gwp$Emissions_valuation[i] <- fleet_gwp$Emissions_difference[i]*(cost_of_carbon)
  #  fleet_gwp$Discounted_emissions[i] <- fleet_gwp$Emissions_valuation[i]/((1+discount_rate/100)^(unique(fleet_gwp$Year)[i]-unique(fleet_gwp$Year)[1]))
  #}
  ##fleet_gwp[nrow(fleet_gwp)+1,] <- c(0, sum(fleet_gwp$Value), sum(fleet_gwp$Reference_emissions), sum(fleet_gwp$Emissions_difference), sum(fleet_gwp$Emissions_valuation), sum(fleet_gwp$Discounted_emissions))
  #print("Exportation of the final results")
  #write.csv(fleet_gwp, paste0(results_path, "/ghg_benefits.csv"))
  print("Done!")
}

