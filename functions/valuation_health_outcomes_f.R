valuation_health_outcomes_f <- function(emissions_battery_production, scenario_id = NA, discount_rate = NA, value_statistical_life = NA, income_growth_rate = NA, vsl_elasticity = NA, include_battery_manufacturing = NA, battery_calculation_method = NA) {
  attribute_f("valuation_health_outcomes_f")
  results_path <- paste0(getwd(), "/outputs/air_quality/", Sys.Date(), "_", scenario_id, "_results")
  GIS_matching_matrix <- read.csv(paste0(getwd(), "/inputs/air_quality/GIS_matching_matrix.csv"))
  # Import all generated csv files from COBRA and calculate the cumulative benefits by state and in total
  results_files <- list.files(path = paste0(results_path, "/COBRA_results/"), pattern = "COBRA_results_")
  cobra_output_file <- data.frame()
  for (i in 1:length(results_files)) {
    path <- paste0(results_path, "/COBRA_results/", results_files[i])
    temp <- read.csv(path)
    temp <- temp %>%
      add_column(year = strsplit(strsplit(results_files[i], "_")[[1]][3], ".csv")[[1]][1])
    if (i == 1) {
      cobra_output_file <- temp
    } else {
      cobra_output_file <- rbind(cobra_output_file, temp)
    } 
  }
  ## Generate the final results files
  ## Non monetized benefits
  print("Generate non-monetized benefits at county, state, and federal level.")
  non_monetized_benefits_county <- select(cobra_output_file, c("year", "Destination", "Mortality..All.Cause..low.", "Mortality..All.Cause..high.", "Infant.Mortality"))
  colnames(non_monetized_benefits_county) <- c("Year", "COBRA_SOURCEINDX", "Mortality_Low", "Mortality_High", "Infant_Mortality")
  non_monetized_benefits_county$Year <- as.numeric(non_monetized_benefits_county$Year)
  non_monetized_benefits_county <- left_join(non_monetized_benefits_county, select(GIS_matching_matrix, c("FIPS", "COBRA_SOURCEINDX", "ST_FIPS")), by = "COBRA_SOURCEINDX") %>%
    filter(!is.na(COBRA_SOURCEINDX)) %>%
    select(-c("COBRA_SOURCEINDX"))
  years <- unique(non_monetized_benefits_county$Year)
  total_benefits_calculation <- function(i) {
    values <- aggregate(.~ FIPS + ST_FIPS, data = select(non_monetized_benefits_county[which(non_monetized_benefits_county$Year <= i),], -c("Year")), FUN = sum) %>%
      add_column("Year" = i)
    colnames(values) <- c("FIPS", "ST_FIPS", "Total_Mortality_Low", "Total_Mortality_High", "Total_Infant_Mortality", "Year")
    return(values)
  }
  non_monetized_benefits_county <- left_join(non_monetized_benefits_county, do.call(rbind, lapply(years, total_benefits_calculation)), by = c("Year", "FIPS", "ST_FIPS"))
  cumulative_benefits_calculation <- function(i) {
    values <- aggregate(.~ FIPS + ST_FIPS, data = non_monetized_benefits_county[which(non_monetized_benefits_county$Year <= i),5:9], FUN = sum) %>%
      add_column("Year" = i)
    colnames(values) <- c("FIPS", "ST_FIPS", "Cumulative_Mortality_Low", "Cumulative_Mortality_High", "Cumulative_Infant_Mortality", "Year")
    return(values)
  }
  non_monetized_benefits_county <- left_join(non_monetized_benefits_county, do.call(rbind, lapply(years, cumulative_benefits_calculation)), by = c("Year", "FIPS", "ST_FIPS")) %>%
    relocate(Year, .before = Mortality_Low) %>%
    relocate(FIPS, .after = Year) %>%
    relocate(ST_FIPS, .after = FIPS)
  non_monetized_benefits_state <- aggregate(.~Year + ST_FIPS, data = select(non_monetized_benefits_county, -c("FIPS")), FUN = sum)
  non_monetized_benefits <- aggregate(.~ Year, data = select(non_monetized_benefits_state, -c("ST_FIPS")), FUN = sum)
  
  ## Monetized benefits
  print("Aggregate monetized benefits from COBRA at the county, state, and federal level.")
  monetized_benefits_county <- data.frame(Year = as.numeric(cobra_output_file$year),
                                          COBRA_SOURCEINDX = cobra_output_file$Destination,
                                          Yearly_Benefits_Low = rowSums(cobra_output_file[,setdiff(grep("X..", colnames(cobra_output_file)),grep("high", colnames(cobra_output_file)))]),
                                          Yearly_Benefits_High = rowSums(cobra_output_file[,setdiff(grep("X..", colnames(cobra_output_file)),grep("low", colnames(cobra_output_file)))]))
  monetized_benefits_county <- left_join(monetized_benefits_county, select(GIS_matching_matrix, c("FIPS", "COBRA_SOURCEINDX", "ST_FIPS")), by = "COBRA_SOURCEINDX") %>%
    filter(!is.na(COBRA_SOURCEINDX)) %>%
    select(-c("COBRA_SOURCEINDX"))
  years <- as.numeric(unique(monetized_benefits_county$Year))
  monetized_total_benefits_calculation <- function(i) {
    values <- aggregate(.~ FIPS + ST_FIPS, data = select(monetized_benefits_county[which(monetized_benefits_county$Year <= i),], -c("Year")), FUN = sum) %>%
      add_column("Year" = i)
    colnames(values) <- c("FIPS", "ST_FIPS", "Total_Monetized_Low", "Total_Monetized_High", "Year")
    values <- add_column(values, "Discounted_Total_Monetized_Low" = values$Total_Monetized_Low*1/(1+discount_rate/100)^(as.numeric(i)-as.numeric(years[1])))
    values <- add_column(values, "Discounted_Total_Monetized_High" = values$Total_Monetized_High*1/(1+discount_rate/100)^(as.numeric(i)-as.numeric(years[1])))
    values$Year <- as.numeric(values$Year)
    return(values)
  }
  monetized_benefits_county <- left_join(monetized_benefits_county, do.call(rbind, lapply(years, monetized_total_benefits_calculation)), by = c("Year", "FIPS", "ST_FIPS"))
  monetized_cumulative_benefits_calculation <- function(i) {
    values <- aggregate(.~ FIPS + ST_FIPS, data = monetized_benefits_county[which(monetized_benefits_county$Year <= i),c(4,5,8,9)], FUN = sum) %>%
      add_column("Year" = i)
    colnames(values) <- c("FIPS", "ST_FIPS", "Cumulative_Discounted_Monetized_Low", "Cumulative_Discounted_Monetized_High", "Year")
    return(values)
  }
  monetized_benefits_county <- left_join(monetized_benefits_county, do.call(rbind, lapply(years, monetized_cumulative_benefits_calculation)), by = c("Year", "FIPS", "ST_FIPS")) %>%
    relocate(FIPS, .after = Year) %>%
    relocate(ST_FIPS, .after = FIPS)
  monetized_benefits_state <- aggregate(.~Year + ST_FIPS, data = select(monetized_benefits_county, -c("FIPS")), FUN = sum)
  monetized_benefits <- aggregate(.~Year, data = select(monetized_benefits_state, -c("ST_FIPS")), FUN = sum)
  
  ### Calculation of the monetized mortality
  print("Monetized benefits from mortality using default growth rate and discount rate")
  COBRA_future_years_benefits <- get_input_f(input_name = 'COBRA_future_years_benefits')
  print("Calculate VSL projections for future years")
  vsl_projections <- data.frame(Year = as.numeric(years),
                                VSL = value_statistical_life)
  vsl_projections$VSL <- vsl_projections$VSL[1]*(1+income_growth_rate/100)^(vsl_elasticity*(vsl_projections$Year-vsl_projections$Year[1]))
  fun <- function(i) {
    discount <- 1/((1+discount_rate/100)^(i-min(years)))
    benefits_distribution <- COBRA_future_years_benefits
    vsl <- vsl_projections$VSL[which(vsl_projections$Year == i)]
    monetized_mortality <- expand_grid(non_monetized_benefits_county[which(non_monetized_benefits_county$Year == i),2:12], benefits_distribution) %>%
      add_column("Discount" = NA)
    colnames(monetized_mortality)[3:8] <- c("Yearly_Benefits_Low", "Yearly_Benefits_High", "Yearly_Benefits_Infant", "Total_Benefits_Low", "Total_Benefits_High", "Total_Benefits_Infant")
    monetized_mortality$Discount <- 1/(1+discount_rate/100)^(monetized_mortality$Year-min(monetized_mortality$Year))
    monetized_mortality[,c(3,4,6,7)] <- monetized_mortality[,c(3,4,6,7)]*monetized_mortality$Share*monetized_mortality$Discount*vsl*discount
    monetized_mortality[,c(5,8)] <- monetized_mortality[,c(5,8)]*monetized_mortality$Infant*monetized_mortality$Discount*vsl*discount
    monetized_mortality$Year <- i
    monetized_mortality <- relocate(monetized_mortality, Year, .before = "FIPS")
    monetized_mortality <- monetized_mortality[,1:9]
    monetized_mortality <- aggregate(.~Year+FIPS+ST_FIPS, data = monetized_mortality, FUN = sum)
    return(monetized_mortality)
  }
  monetized_mortality_county <- do.call(rbind, lapply(years, fun))
  # Combine general mortality and infant mortality
  monetized_mortality_county$Yearly_Benefits_Low <- monetized_mortality_county$Yearly_Benefits_Low+monetized_mortality_county$Yearly_Benefits_Infant
  monetized_mortality_county$Yearly_Benefits_High <- monetized_mortality_county$Yearly_Benefits_High+monetized_mortality_county$Yearly_Benefits_Infant
  monetized_mortality_county$Total_Benefits_Low <- monetized_mortality_county$Total_Benefits_Low+monetized_mortality_county$Total_Benefits_Infant
  monetized_mortality_county$Total_Benefits_High <- monetized_mortality_county$Total_Benefits_High+monetized_mortality_county$Total_Benefits_Infant
  monetized_mortality_county <- select(monetized_mortality_county, -c("Yearly_Benefits_Infant", "Total_Benefits_Infant"))
  ## Add damages from battery production
  if (include_battery_manufacturing == "Y" & battery_calculation_method == "Tessum2014") {
    batteries_damages_monetized_yearly_county <- emissions_battery_production[["batteries_damages_monetized_yearly_county"]]
    monetized_mortality_county <- left_join(monetized_mortality_county, batteries_damages_monetized_yearly_county, by = c("Year", "FIPS", "ST_FIPS"))
    monetized_mortality_county$Total_Benefits_Low <- monetized_mortality_county$Total_Benefits_Low-monetized_mortality_county$Discounted_damages
    monetized_mortality_county$Total_Benefits_High <- monetized_mortality_county$Total_Benefits_High-monetized_mortality_county$Discounted_damages
  }
  cumulative_benefits_calculation <- function(i) {
    values <- aggregate(.~ FIPS + ST_FIPS, data = monetized_mortality_county[which(monetized_mortality_county$Year <= i),c(2,3,6,7)], FUN = sum) %>%
      add_column("Year" = i)
    colnames(values) <- c("FIPS", "ST_FIPS", "Cumulative_Benefits_Low", "Cumulative_Benefits_High", "Year")
    return(values)
  }
  monetized_mortality_county <- left_join(monetized_mortality_county, do.call(rbind, lapply(years, cumulative_benefits_calculation)), by = c("Year", "FIPS", "ST_FIPS")) %>%
    relocate(Year, .before = Yearly_Benefits_Low) %>%
    relocate(FIPS, .after = Year) %>%
    relocate(ST_FIPS, .after = FIPS)
  monetized_mortality_state <- aggregate(.~Year+ST_FIPS, data = select(monetized_mortality_county, -c("FIPS")), FUN = sum)
  monetized_mortality <- aggregate(.~Year, data = select(monetized_mortality_state, -c("ST_FIPS")), FUN = sum)
  
  ## Sensitivity analysis on VSL, growth rate and discount rate at the federal level
  print("Sensitivity analysis on VSL, growth rate and discount rate at the federal level")
  discount_rate_sens <- c(2,3,7)
  growth_rate_sens <- c(-3,-2,-1,0,1,1.3,2,3)
  vsl_sens <- c(6.13e6, 12.1e6, 13.19e6, 20.12e6)
  sensitivity <- expand_grid(discount_rate_sens, growth_rate_sens, vsl_sens)
  fun <- function(i) {
    benefits_distribution <- COBRA_future_years_benefits
    colnames(benefits_distribution)[1] <- "Year_in"
    sensitivity_analysis <- expand_grid(non_monetized_benefits[which(non_monetized_benefits$Year == i),1:5], expand_grid(sensitivity, benefits_distribution)) %>%
      add_column("Discount_in" = NA) %>%
      add_column("Discount" = NA) %>%
      add_column("Projected_VSL" = NA)
    sensitivity_analysis$Discount_in <- 1/(1+sensitivity_analysis$discount_rate_sens/100)^(sensitivity_analysis$Year_in-min(sensitivity_analysis$Year_in))
    sensitivity_analysis$Discount <- 1/(1+sensitivity_analysis$discount_rate_sens/100)^(sensitivity_analysis$Year-min(years))
    sensitivity_analysis$Projected_VSL <- sensitivity_analysis$vsl_sens*(1+sensitivity_analysis$growth_rate_sens/100)^(vsl_elasticity*(sensitivity_analysis$Year-min(years)))
    sensitivity_analysis[,2:5] <- sensitivity_analysis[,2:5]*sensitivity_analysis$Share*sensitivity_analysis$Discount_in*sensitivity_analysis$Projected_VSL*sensitivity_analysis$Discount
    sensitivity_analysis <- aggregate(.~Year+discount_rate_sens+growth_rate_sens+vsl_sens, data = sensitivity_analysis[,1:8], FUN = sum)
    return(sensitivity_analysis)
  }
  monetized_mortality_sensitivity <- do.call(rbind, lapply(years, fun))
  monetized_cumulative_mortality_calculation <- function(i) {
    values <- aggregate(.~ discount_rate_sens + growth_rate_sens + vsl_sens, data = monetized_mortality_sensitivity[which(monetized_mortality_sensitivity$Year <= i),c(2:4,7,8)], FUN = sum) %>%
      add_column("Year" = i)
    colnames(values)[4:5] <- c("Cumulative_Discounted_Monetized_Low", "Cumulative_Discounted_Monetized_High")
    return(values)
  }
  monetized_mortality_sensitivity <- left_join(monetized_mortality_sensitivity, do.call(rbind, lapply(years, monetized_cumulative_mortality_calculation)), by = c("Year", "discount_rate_sens", "growth_rate_sens", "vsl_sens")) %>%
    relocate(Year, .before = discount_rate_sens)
  
  ## Combine mortality and infant mortality in the non-monetized files
  non_monetized_benefits_county$Mortality_Low <- non_monetized_benefits_county$Mortality_Low+non_monetized_benefits_county$Infant_Mortality
  non_monetized_benefits_county$Mortality_High <- non_monetized_benefits_county$Total_Mortality_High+non_monetized_benefits_county$Infant_Mortality
  non_monetized_benefits_county$Total_Mortality_Low <- non_monetized_benefits_county$Total_Mortality_Low+non_monetized_benefits_county$Total_Infant_Mortality
  non_monetized_benefits_county$Total_Mortality_High <- non_monetized_benefits_county$Total_Mortality_High+non_monetized_benefits_county$Total_Infant_Mortality
  non_monetized_benefits_county$Cumulative_Mortality_Low <- non_monetized_benefits_county$Cumulative_Mortality_Low+non_monetized_benefits_county$Cumulative_Infant_Mortality
  non_monetized_benefits_county$Cumulative_Mortality_High <- non_monetized_benefits_county$Cumulative_Mortality_High+non_monetized_benefits_county$Cumulative_Infant_Mortality
  #
  non_monetized_benefits_state$Mortality_Low <- non_monetized_benefits_state$Mortality_Low+non_monetized_benefits_state$Infant_Mortality
  non_monetized_benefits_state$Mortality_High <- non_monetized_benefits_state$Total_Mortality_High+non_monetized_benefits_state$Infant_Mortality
  non_monetized_benefits_state$Total_Mortality_Low <- non_monetized_benefits_state$Total_Mortality_Low+non_monetized_benefits_state$Total_Infant_Mortality
  non_monetized_benefits_state$Total_Mortality_High <- non_monetized_benefits_state$Total_Mortality_High+non_monetized_benefits_state$Total_Infant_Mortality
  non_monetized_benefits_state$Cumulative_Mortality_Low <- non_monetized_benefits_state$Cumulative_Mortality_Low+non_monetized_benefits_state$Cumulative_Infant_Mortality
  non_monetized_benefits_state$Cumulative_Mortality_High <- non_monetized_benefits_state$Cumulative_Mortality_High+non_monetized_benefits_state$Cumulative_Infant_Mortality
  #
  non_monetized_benefits$Mortality_Low <- non_monetized_benefits$Mortality_Low+non_monetized_benefits$Infant_Mortality
  non_monetized_benefits$Mortality_High <- non_monetized_benefits$Total_Mortality_High+non_monetized_benefits$Infant_Mortality
  non_monetized_benefits$Total_Mortality_Low <- non_monetized_benefits$Total_Mortality_Low+non_monetized_benefits$Total_Infant_Mortality
  non_monetized_benefits$Total_Mortality_High <- non_monetized_benefits$Total_Mortality_High+non_monetized_benefits$Total_Infant_Mortality
  non_monetized_benefits$Cumulative_Mortality_Low <- non_monetized_benefits$Cumulative_Mortality_Low+non_monetized_benefits$Cumulative_Infant_Mortality
  non_monetized_benefits$Cumulative_Mortality_High <- non_monetized_benefits$Cumulative_Mortality_High+non_monetized_benefits$Cumulative_Infant_Mortality
  #
  non_monetized_benefits_county <- select(non_monetized_benefits_county, -c("Infant_Mortality", "Total_Infant_Mortality", "Cumulative_Infant_Mortality"))
  non_monetized_benefits_state <- select(non_monetized_benefits_state, -c("Infant_Mortality", "Total_Infant_Mortality", "Cumulative_Infant_Mortality"))
  non_monetized_benefits <- select(non_monetized_benefits, -c("Infant_Mortality", "Total_Infant_Mortality", "Cumulative_Infant_Mortality"))
  ## Export the generated files
  print("Generation of the final results files")
  write.csv(non_monetized_benefits_county, paste0(results_path, "/non_monetized_benefits_county.csv"), row.names = FALSE)
  write.csv(non_monetized_benefits_state, paste0(results_path, "/non_monetized_benefits_state.csv"), row.names = FALSE)
  write.csv(non_monetized_benefits, paste0(results_path, "/non_monetized_benefits.csv"))
  write.csv(monetized_benefits_county, paste0(results_path, "/monetized_benefits_county.csv"), row.names = FALSE)
  write.csv(monetized_benefits_state, paste0(results_path, "/monetized_benefits_state.csv"), row.names = FALSE)
  write.csv(monetized_benefits, paste0(results_path, "/monetized_benefits.csv"), row.names = FALSE)
  write.csv(monetized_mortality_county, paste0(results_path, "/monetized_mortality_county.csv"), row.names = FALSE)
  write.csv(monetized_mortality_state, paste0(results_path, "/monetized_mortality_state.csv"), row.names = FALSE)
  write.csv(monetized_mortality, paste0(results_path, "/monetized_mortality.csv"), row.names = FALSE)
  write.csv(monetized_mortality_sensitivity, paste0(results_path, "/monetized_mortality_sensitivity.csv"), row.names = FALSE)
}
