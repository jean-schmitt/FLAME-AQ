#Calculation of the benefits of 1 EV by model year
# Calculation of the average yearly benefit of an EV: dividing the total benefits in the scenario by the number of EVs
# The function is to be integrated to FLAME-AQ after the calculation of the benefits

path <- "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_FLAME-AQ/"
scenario_worst <- "outputs/air_quality/2023-05-20_ZEV_All_current_mix_results"
scenario_low <- "outputs/air_quality/2023-05-20_ZEV_All_NREL_low_case_results"
scenario_intermediate <- "outputs/air_quality/2023-05-19_ZEV_All_NREL_intermediate_results"
scenario_best <- "outputs/air_quality/2023-05-20_ZEV_All_NREL_high_decarb_results"
reference <- "outputs/air_quality/2023-05-19_noEVs_NREL_intermediate_results"
file <- "/health_benefits.csv"
reference_scenario <- read.csv(paste0(path, reference, file))
health_benefits_worst <- read.csv(paste0(path, scenario_worst, file))
health_benefits_low <- read.csv(paste0(path, scenario_low, file))
health_benefits_intermediate <- read.csv(paste0(path, scenario_intermediate, file))
health_benefits_best <- read.csv(paste0(path, scenario_best, file))
fleet_composition <- read.csv(paste0(path, inputs, "/fleet_composition.csv"))
vkt_data <- fleet_vint_vkt
vehicle_lifetime <- 20

# Average benefits by EV - Nationwide
benefits_per_EV_nationwide <- data.frame(Years = unique(health_benefits_intermediate$Year[which(health_benefits$Year >0)])) %>%
  add_column("Benefits_per_EV_worst" = 0) %>%
  add_column("EV_Model_Year_worst" = 0) %>%
  add_column("Benefits_per_EV_low" = 0) %>%
  add_column("EV_Model_Year_low" = 0) %>%
  add_column("Benefits_per_EV_intermediate" = 0) %>%
  add_column("EV_Model_Year_intermediate" = 0) %>%
  add_column("Benefits_per_EV_best" = 0) %>%
  add_column("EV_Model_Year_best" = 0)
years <- unique(benefits_per_EV_nationwide$Years[which(benefits_per_EV_nationwide$Years > 0)])
for (i in 1:dim(benefits_per_EV_nationwide)[1]) {
  number_EVs <- sum(fleet_composition$Value[which(fleet_composition$Year == benefits_per_EV_nationwide$Years[i] & grepl("BEV", fleet_composition$Technology))])
  benefits_per_EV_nationwide$Benefits_per_EV_worst[i] <- (health_benefits_worst$Cumulative_benefits[which(health_benefits_worst$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])/number_EVs
  benefits_per_EV_nationwide$Benefits_per_EV_low[i] <- (health_benefits_low$Cumulative_benefits[which(health_benefits_low$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])/number_EVs
  benefits_per_EV_nationwide$Benefits_per_EV_intermediate[i] <- (health_benefits_intermediate$Cumulative_benefits[which(health_benefits_intermediate$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])/number_EVs
  benefits_per_EV_nationwide$Benefits_per_EV_best[i] <- (health_benefits_best$Cumulative_benefits[which(health_benefits_best$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])/number_EVs
  total_EV_vkt <- sum(vkt_data$Value[vkt_data$Year == benefits_per_EV_nationwide$Years[i] & grepl("BEV", vkt_data$Technology)])
  for (j in 1:length(years)) {
    if (years[j] <= benefits_per_EV_nationwide$Years[i] & (benefits_per_EV_nationwide$Years[i]-years[j]) <= vehicle_lifetime) {
      modelYr_vkt <- sum(vkt_data$Value[which(vkt_data$Year == benefits_per_EV_nationwide$Years[i] & vkt_data$Age == (benefits_per_EV_nationwide$Years[i]-years[j]) & grepl("BEV", vkt_data$Technology))])
      vehicles_in_year <- sum(fleet_composition$Value[which(fleet_composition$Model_Year == years[j] & fleet_composition$Year == benefits_per_EV_nationwide$Years[i] & grepl("BEV", fleet_composition$Technology))])
      benefits_per_EV_nationwide$EV_Model_Year_worst[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_worst[which(benefits_per_EV_nationwide$Years == years[j])] + (health_benefits_worst$Cumulative_benefits[which(health_benefits_worst$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])*(modelYr_vkt/total_EV_vkt)/vehicles_in_year
      benefits_per_EV_nationwide$EV_Model_Year_low[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_low[which(benefits_per_EV_nationwide$Years == years[j])] + (health_benefits_low$Cumulative_benefits[which(health_benefits_low$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])*(modelYr_vkt/total_EV_vkt)/vehicles_in_year
      benefits_per_EV_nationwide$EV_Model_Year_intermediate[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_intermediate[which(benefits_per_EV_nationwide$Years == years[j])] + (health_benefits_intermediate$Cumulative_benefits[which(health_benefits_intermediate$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])*(modelYr_vkt/total_EV_vkt)/vehicles_in_year
      benefits_per_EV_nationwide$EV_Model_Year_best[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_best[which(benefits_per_EV_nationwide$Years == years[j])] + (health_benefits_best$Cumulative_benefits[which(health_benefits_best$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])*(modelYr_vkt/total_EV_vkt)/vehicles_in_year
    }
    if (benefits_per_EV_nationwide$Years[i] == max(benefits_per_EV_nationwide$Years) & (benefits_per_EV_nationwide$Years[i]-years[j]) < vehicle_lifetime) {
      benefits_per_EV_nationwide$EV_Model_Year_worst[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_worst[which(benefits_per_EV_nationwide$Years == years[j])] + benefits_per_EV_nationwide$Benefits_per_EV_worst[which(benefits_per_EV_nationwide$Years == benefits_per_EV_nationwide$Years[i])]*(vehicle_lifetime-(max(benefits_per_EV_nationwide$Years)-years[j]))
      benefits_per_EV_nationwide$EV_Model_Year_low[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_low[which(benefits_per_EV_nationwide$Years == years[j])] + benefits_per_EV_nationwide$Benefits_per_EV_low[which(benefits_per_EV_nationwide$Years == benefits_per_EV_nationwide$Years[i])]*(vehicle_lifetime-(max(benefits_per_EV_nationwide$Years)-years[j]))
      benefits_per_EV_nationwide$EV_Model_Year_intermediate[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_intermediate[which(benefits_per_EV_nationwide$Years == years[j])] + benefits_per_EV_nationwide$Benefits_per_EV_intermediate[which(benefits_per_EV_nationwide$Years == benefits_per_EV_nationwide$Years[i])]*(vehicle_lifetime-(max(benefits_per_EV_nationwide$Years)-years[j]))
      benefits_per_EV_nationwide$EV_Model_Year_best[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_best[which(benefits_per_EV_nationwide$Years == years[j])] + benefits_per_EV_nationwide$Benefits_per_EV_best[which(benefits_per_EV_nationwide$Years == benefits_per_EV_nationwide$Years[i])]*(vehicle_lifetime-(max(benefits_per_EV_nationwide$Years)-years[j]))
    }
  }
}

# Select a state
state <- 6
GIS_matching_matrix <- read.csv("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_FLAME-AQ/inputs/air_quality/GIS_matching_matrix.csv")
state_name <- unique(GIS_matching_matrix$ST_NAME[which(GIS_matching_matrix$ST_FIPS == state)])
file_state <- "/health_benefits_by_state.csv"
reference_scenario_state <- read.csv(paste0(path, reference, file_state))
health_benefits_worst_state <- read.csv(paste0(path, scenario_worst, file_state))
health_benefits_low_state <- read.csv(paste0(path, scenario_low, file_state))
health_benefits_intermediate_state <- read.csv(paste0(path, scenario_intermediate, file_state))
health_benefits_best_state <- read.csv(paste0(path, scenario_best, file_state))
fleet_composition_state <- read.csv(paste0(path, inputs, "/fleet_composition_by_state.csv"))
benefits_per_EV_nationwide <- data.frame(Years = unique(health_benefits_intermediate$Year[which(health_benefits$Year >0)])) %>%
  add_column("Benefits_per_EV_worst" = 0) %>%
  add_column("EV_Model_Year_worst" = 0) %>%
  add_column("Benefits_per_EV_low" = 0) %>%
  add_column("EV_Model_Year_low" = 0) %>%
  add_column("Benefits_per_EV_intermediate" = 0) %>%
  add_column("EV_Model_Year_intermediate" = 0) %>%
  add_column("Benefits_per_EV_best" = 0) %>%
  add_column("EV_Model_Year_best" = 0)
years <- unique(benefits_per_EV_nationwide$Years[which(benefits_per_EV_nationwide$Years > 0)])
reference_scenario <- reference_scenario_state[which(reference_scenario_state$State == state_name),]
health_benefits_worst <- health_benefits_worst_state[which(health_benefits_worst_state$State == state_name),]
health_benefits_low <- health_benefits_low_state[which(health_benefits_low_state$State == state_name),]
health_benefits_intermediate <- health_benefits_intermediate_state[which(health_benefits_intermediate_state$State == state_name),]
health_benefits_best <- health_benefits_best_state[which(health_benefits_best_state$State == state_name),]
fleet_composition <- fleet_composition_state[which(fleet_composition_state$State == state),]
for (i in 1:dim(benefits_per_EV_nationwide)[1]) {
  number_EVs <- sum(fleet_composition$Value[which(fleet_composition$Year == benefits_per_EV_nationwide$Years[i] & grepl("BEV", fleet_composition$Technology))])
  benefits_per_EV_nationwide$Benefits_per_EV_worst[i] <- (health_benefits_worst$Cumulative_benefits[which(health_benefits_worst$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])/number_EVs
  benefits_per_EV_nationwide$Benefits_per_EV_low[i] <- (health_benefits_low$Cumulative_benefits[which(health_benefits_low$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])/number_EVs
  benefits_per_EV_nationwide$Benefits_per_EV_intermediate[i] <- (health_benefits_intermediate$Cumulative_benefits[which(health_benefits_intermediate$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])/number_EVs
  benefits_per_EV_nationwide$Benefits_per_EV_best[i] <- (health_benefits_best$Cumulative_benefits[which(health_benefits_best$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])/number_EVs
  total_EV_vkt <- sum(vkt_data$Value[vkt_data$Year == benefits_per_EV_nationwide$Years[i] & grepl("BEV", vkt_data$Technology)])
  for (j in 1:length(years)) {
    if (years[j] <= benefits_per_EV_nationwide$Years[i] & (benefits_per_EV_nationwide$Years[i]-years[j]) <= vehicle_lifetime) {
      modelYr_vkt <- sum(vkt_data$Value[which(vkt_data$Year == benefits_per_EV_nationwide$Years[i] & vkt_data$Age == (benefits_per_EV_nationwide$Years[i]-years[j]) & grepl("BEV", vkt_data$Technology))])
      vehicles_in_year <- sum(fleet_composition$Value[which(fleet_composition$Model_Year == years[j] & fleet_composition$Year == benefits_per_EV_nationwide$Years[i] & grepl("BEV", fleet_composition$Technology))])
      benefits_per_EV_nationwide$EV_Model_Year_worst[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_worst[which(benefits_per_EV_nationwide$Years == years[j])] + (health_benefits_worst$Cumulative_benefits[which(health_benefits_worst$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])*(modelYr_vkt/total_EV_vkt)/vehicles_in_year
      benefits_per_EV_nationwide$EV_Model_Year_low[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_low[which(benefits_per_EV_nationwide$Years == years[j])] + (health_benefits_low$Cumulative_benefits[which(health_benefits_low$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])*(modelYr_vkt/total_EV_vkt)/vehicles_in_year
      benefits_per_EV_nationwide$EV_Model_Year_intermediate[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_intermediate[which(benefits_per_EV_nationwide$Years == years[j])] + (health_benefits_intermediate$Cumulative_benefits[which(health_benefits_intermediate$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])*(modelYr_vkt/total_EV_vkt)/vehicles_in_year
      benefits_per_EV_nationwide$EV_Model_Year_best[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_best[which(benefits_per_EV_nationwide$Years == years[j])] + (health_benefits_best$Cumulative_benefits[which(health_benefits_best$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])*(modelYr_vkt/total_EV_vkt)/vehicles_in_year
    }
    if (benefits_per_EV_nationwide$Years[i] == max(benefits_per_EV_nationwide$Years) & (benefits_per_EV_nationwide$Years[i]-years[j]) < vehicle_lifetime) {
      benefits_per_EV_nationwide$EV_Model_Year_worst[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_worst[which(benefits_per_EV_nationwide$Years == years[j])] + benefits_per_EV_nationwide$Benefits_per_EV_worst[which(benefits_per_EV_nationwide$Years == benefits_per_EV_nationwide$Years[i])]*(vehicle_lifetime-(max(benefits_per_EV_nationwide$Years)-years[j]))
      benefits_per_EV_nationwide$EV_Model_Year_low[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_low[which(benefits_per_EV_nationwide$Years == years[j])] + benefits_per_EV_nationwide$Benefits_per_EV_low[which(benefits_per_EV_nationwide$Years == benefits_per_EV_nationwide$Years[i])]*(vehicle_lifetime-(max(benefits_per_EV_nationwide$Years)-years[j]))
      benefits_per_EV_nationwide$EV_Model_Year_intermediate[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_intermediate[which(benefits_per_EV_nationwide$Years == years[j])] + benefits_per_EV_nationwide$Benefits_per_EV_intermediate[which(benefits_per_EV_nationwide$Years == benefits_per_EV_nationwide$Years[i])]*(vehicle_lifetime-(max(benefits_per_EV_nationwide$Years)-years[j]))
      benefits_per_EV_nationwide$EV_Model_Year_best[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_best[which(benefits_per_EV_nationwide$Years == years[j])] + benefits_per_EV_nationwide$Benefits_per_EV_best[which(benefits_per_EV_nationwide$Years == benefits_per_EV_nationwide$Years[i])]*(vehicle_lifetime-(max(benefits_per_EV_nationwide$Years)-years[j]))
    }
  }
}
# Select a county
county_FIPS <- 6037
county_allocation_factors <- read.csv("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_FLAME-AQ/inputs/air_quality/normalized_vehicles_distribution_by_county.csv")
GIS_matching_matrix <- read.csv("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_FLAME-AQ/inputs/air_quality/GIS_matching_matrix.csv")
state_name <- unique(GIS_matching_matrix$ST_NAME[which(GIS_matching_matrix$FIPS == county_FIPS)])
state <- unique(GIS_matching_matrix$ST_FIPS[which(GIS_matching_matrix$FIPS == county_FIPS)])
county_allocation_factor <- county_allocation_factors$county_allocation_factor[which(county_allocation_factors$FIPS == county_FIPS)]
file_county <- "/health_benefits_by_county.csv"
reference_scenario_county <- read.csv(paste0(path, reference, file_county))
health_benefits_worst_county <- read.csv(paste0(path, scenario_worst, file_county))
health_benefits_low_county <- read.csv(paste0(path, scenario_low, file_county))
health_benefits_intermediate_county <- read.csv(paste0(path, scenario_intermediate, file_county))
health_benefits_best_county <- read.csv(paste0(path, scenario_best, file_county))
fleet_composition_county <- read.csv(paste0(path, inputs, "/fleet_composition_by_state.csv"))
benefits_per_EV_nationwide <- data.frame(Years = unique(health_benefits_intermediate$Year[which(health_benefits$Year >0)])) %>%
  add_column("Benefits_per_EV_worst" = 0) %>%
  add_column("EV_Model_Year_worst" = 0) %>%
  add_column("Benefits_per_EV_low" = 0) %>%
  add_column("EV_Model_Year_low" = 0) %>%
  add_column("Benefits_per_EV_intermediate" = 0) %>%
  add_column("EV_Model_Year_intermediate" = 0) %>%
  add_column("Benefits_per_EV_best" = 0) %>%
  add_column("EV_Model_Year_best" = 0)
years <- unique(benefits_per_EV_nationwide$Years[which(benefits_per_EV_nationwide$Years > 0)])
reference_scenario <- reference_scenario_county[which(reference_scenario_county$FIPS == county_FIPS),]
health_benefits_worst <- health_benefits_worst_county[which(health_benefits_worst_county$FIPS == county_FIPS),]
health_benefits_low <- health_benefits_low_county[which(health_benefits_low_county$FIPS == county_FIPS),]
health_benefits_intermediate <- health_benefits_intermediate_county[which(health_benefits_intermediate_county$FIPS == county_FIPS),]
health_benefits_best <- health_benefits_best_county[which(health_benefits_best_county$FIPS == county_FIPS),]
# Complete the missing values in the results files by county
for (i in 1:length(years)) {
  if (i == 1) {
    health_benefits_worst$Cumulative_benefits[i] <- health_benefits_worst$Discounted_benefits[i]
    health_benefits_low$Cumulative_benefits[i] <- health_benefits_low$Discounted_benefits[i]
    health_benefits_intermediate$Cumulative_benefits[i] <- health_benefits_intermediate$Discounted_benefits[i]
    health_benefits_best$Cumulative_benefits[i] <- health_benefits_best$Discounted_benefits[i]
    reference_scenario$Cumulative_benefits[i] <- reference_scenario$Discounted_benefits[i]
    health_benefits_worst$Total_benefits[i] <- health_benefits_worst$Cumulative_benefits[i]
    health_benefits_low$Total_benefits[i] <- health_benefits_low$Cumulative_benefits[i]
    health_benefits_intermediate$Total_benefits[i] <- health_benefits_intermediate$Cumulative_benefits[i]
    health_benefits_best$Total_benefits[i] <- health_benefits_best$Cumulative_benefits[i]
    reference_scenario$Total_benefits[i] <- reference_scenario$Cumulative_benefits[i]
  } else {
    health_benefits_worst$Cumulative_benefits[i] <- health_benefits_worst$Cumulative_benefits[i-1]+health_benefits_worst$Discounted_benefits[i]
    health_benefits_low$Cumulative_benefits[i] <- health_benefits_low$Cumulative_benefits[i-1]+health_benefits_low$Discounted_benefits[i]
    health_benefits_intermediate$Cumulative_benefits[i] <- health_benefits_intermediate$Cumulative_benefits[i-1]+health_benefits_intermediate$Discounted_benefits[i]
    health_benefits_best$Cumulative_benefits[i] <- health_benefits_best$Cumulative_benefits[i-1]+health_benefits_best$Discounted_benefits[i]
    reference_scenario$Cumulative_benefits[i] <- reference_scenario$Cumulative_benefits[i-1]+reference_scenario$Discounted_benefits[i]
    health_benefits_worst$Total_benefits[i] <- health_benefits_worst$Total_benefits[i-1]+health_benefits_worst$Cumulative_benefits[i]
    health_benefits_low$Total_benefits[i] <- health_benefits_low$Total_benefits[i-1]+health_benefits_low$Cumulative_benefits[i]
    health_benefits_intermediate$Total_benefits[i] <- health_benefits_intermediate$Total_benefits[i-1]+health_benefits_intermediate$Cumulative_benefits[i]
    health_benefits_best$Total_benefits[i] <- health_benefits_best$Total_benefits[i-1]+health_benefits_best$Cumulative_benefits[i]
    reference_scenario$Total_benefits[i] <- reference_scenario$Total_benefits[i-1]+reference_scenario$Cumulative_benefits[i]
  }
}
fleet_composition <- fleet_composition_state[which(fleet_composition_state$State == state),]
for (i in 1:dim(benefits_per_EV_nationwide)[1]) {
  number_EVs <- sum(fleet_composition$Value[which(fleet_composition$Year == benefits_per_EV_nationwide$Years[i] & grepl("BEV", fleet_composition$Technology))])*county_allocation_factor
  benefits_per_EV_nationwide$Benefits_per_EV_worst[i] <- (health_benefits_worst$Cumulative_benefits[which(health_benefits_worst$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])/number_EVs
  benefits_per_EV_nationwide$Benefits_per_EV_low[i] <- (health_benefits_low$Cumulative_benefits[which(health_benefits_low$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])/number_EVs
  benefits_per_EV_nationwide$Benefits_per_EV_intermediate[i] <- (health_benefits_intermediate$Cumulative_benefits[which(health_benefits_intermediate$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])/number_EVs
  benefits_per_EV_nationwide$Benefits_per_EV_best[i] <- (health_benefits_best$Cumulative_benefits[which(health_benefits_best$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])/number_EVs
  total_EV_vkt <- sum(vkt_data$Value[vkt_data$Year == benefits_per_EV_nationwide$Years[i] & grepl("BEV", vkt_data$Technology)])
  for (j in 1:length(years)) {
    if (years[j] <= benefits_per_EV_nationwide$Years[i] & (benefits_per_EV_nationwide$Years[i]-years[j]) <= vehicle_lifetime) {
      modelYr_vkt <- sum(vkt_data$Value[which(vkt_data$Year == benefits_per_EV_nationwide$Years[i] & vkt_data$Age == (benefits_per_EV_nationwide$Years[i]-years[j]) & grepl("BEV", vkt_data$Technology))])
      vehicles_in_year <- sum(fleet_composition$Value[which(fleet_composition$Model_Year == years[j] & fleet_composition$Year == benefits_per_EV_nationwide$Years[i] & grepl("BEV", fleet_composition$Technology))])*county_allocation_factor
      benefits_per_EV_nationwide$EV_Model_Year_worst[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_worst[which(benefits_per_EV_nationwide$Years == years[j])] + (health_benefits_worst$Cumulative_benefits[which(health_benefits_worst$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])*(modelYr_vkt/total_EV_vkt)/vehicles_in_year
      benefits_per_EV_nationwide$EV_Model_Year_low[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_low[which(benefits_per_EV_nationwide$Years == years[j])] + (health_benefits_low$Cumulative_benefits[which(health_benefits_low$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])*(modelYr_vkt/total_EV_vkt)/vehicles_in_year
      benefits_per_EV_nationwide$EV_Model_Year_intermediate[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_intermediate[which(benefits_per_EV_nationwide$Years == years[j])] + (health_benefits_intermediate$Cumulative_benefits[which(health_benefits_intermediate$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])*(modelYr_vkt/total_EV_vkt)/vehicles_in_year
      benefits_per_EV_nationwide$EV_Model_Year_best[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_best[which(benefits_per_EV_nationwide$Years == years[j])] + (health_benefits_best$Cumulative_benefits[which(health_benefits_best$Year == benefits_per_EV_nationwide$Years[i])]-reference_scenario$Cumulative_benefits[which(reference_scenario$Year == benefits_per_EV_nationwide$Years[i])])*(modelYr_vkt/total_EV_vkt)/vehicles_in_year
    }
    if (benefits_per_EV_nationwide$Years[i] == max(benefits_per_EV_nationwide$Years) & (benefits_per_EV_nationwide$Years[i]-years[j]) < vehicle_lifetime) {
      benefits_per_EV_nationwide$EV_Model_Year_worst[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_worst[which(benefits_per_EV_nationwide$Years == years[j])] + benefits_per_EV_nationwide$Benefits_per_EV_worst[which(benefits_per_EV_nationwide$Years == benefits_per_EV_nationwide$Years[i])]*(vehicle_lifetime-(max(benefits_per_EV_nationwide$Years)-years[j]))
      benefits_per_EV_nationwide$EV_Model_Year_low[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_low[which(benefits_per_EV_nationwide$Years == years[j])] + benefits_per_EV_nationwide$Benefits_per_EV_low[which(benefits_per_EV_nationwide$Years == benefits_per_EV_nationwide$Years[i])]*(vehicle_lifetime-(max(benefits_per_EV_nationwide$Years)-years[j]))
      benefits_per_EV_nationwide$EV_Model_Year_intermediate[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_intermediate[which(benefits_per_EV_nationwide$Years == years[j])] + benefits_per_EV_nationwide$Benefits_per_EV_intermediate[which(benefits_per_EV_nationwide$Years == benefits_per_EV_nationwide$Years[i])]*(vehicle_lifetime-(max(benefits_per_EV_nationwide$Years)-years[j]))
      benefits_per_EV_nationwide$EV_Model_Year_best[which(benefits_per_EV_nationwide$Years == years[j])] <- benefits_per_EV_nationwide$EV_Model_Year_best[which(benefits_per_EV_nationwide$Years == years[j])] + benefits_per_EV_nationwide$Benefits_per_EV_best[which(benefits_per_EV_nationwide$Years == benefits_per_EV_nationwide$Years[i])]*(vehicle_lifetime-(max(benefits_per_EV_nationwide$Years)-years[j]))
    }
  }
}