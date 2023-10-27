health_damages_batteries_f <- function(fleet_composition, fleet_composition_state_breakdown, battery_capacity_car = NA, battery_capacity_truck = NA, damage_per_kwh = NA, discount_rate = NA, first_proj_yr = NA, battery_calculation_method = NA, battery_energy_density  = NA, fleet_id = NA) {
  attribute_f("health_damages_batteries_f")
  fleet_vint_stock <- fleet_composition[["fleet_vint_stock"]]
  first_proj_yr <- 2022
  technologies <- c("BEV100", "BEV300")
  fleet_vint_stock <- filter(fleet_vint_stock, fleet_vint_stock$Age == 0 & fleet_vint_stock$Technology%in%technologies) %>%
    select(-c("Age", "Technology"))
  if (battery_calculation_method == "Tessum2014") {
    # Calculation of the monetized damages from batteries manufacturing
    batteries_damages_monetized <- aggregate(Value ~ Year+Size, data = fleet_vint_stock, FUN = sum) %>%
      add_column("Manufactured_capacity" = NA) %>%
      add_column("Monetized_damages" = NA)
    for (i in 1:dim(batteries_damages_monetized)[1]) {
      if (batteries_damages_monetized$Size[i] == "Car") {
        battery_capacity <- battery_capacity_car
      } else if (batteries_damages_monetized$Size[i] == "Light truck") {
        battery_capacity <- battery_capacity_truck
      }
      batteries_damages_monetized$Manufactured_capacity[i] <- batteries_damages_monetized$Value[i]*battery_capacity
      batteries_damages_monetized$Monetized_damages[i] <- batteries_damages_monetized$Manufactured_capacity[i]*damage_per_kwh
    }
    batteries_damages_monetized_yearly <- aggregate(Monetized_damages ~ Year, data = select(batteries_damages_monetized, -c("Size", "Value", "Manufactured_capacity")), FUN = sum) %>%
      add_column("Discounted_damages" = NA)
    batteries_damages_monetized_yearly <- filter(batteries_damages_monetized_yearly, batteries_damages_monetized_yearly$Year >= first_proj_yr)
    for (i in 1:dim(batteries_damages_monetized_yearly)[1]) {
      batteries_damages_monetized_yearly$Discounted_damages[i] <- batteries_damages_monetized_yearly$Monetized_damages[i]/((1+discount_rate/100)^i)
    }
    # Calculation of the allocation by county
    fleet_stock_state <- fleet_composition_state_breakdown[["fleet_vint_stock_scenario_state_breakdown"]] %>%
      filter(Age == 0 & 
             (Technology == "BEV100" | Technology == "BEV300") &
               Year >= first_proj_yr) %>%
      select(-c("Age", "Size", "Technology", "Model_Year"))
    fleet_stock_state <- aggregate(Value ~ Year+State, data = fleet_stock_state, FUN = sum)
    state_allocation_factors <- fleet_stock_state
    for (i in 1:dim(state_allocation_factors)[1]) {
      if (fleet_id != "no_EVs") {
        state_allocation_factors$Value[i] <- fleet_stock_state$Value[i]/sum(fleet_stock_state$Value[which(fleet_stock_state$Year == state_allocation_factors$Year[i])])
      } else {
        state_allocation_factors$Value[i] <- 1
      }
    }
    GIS_matching_matrix <- read.csv(paste0(getwd(), "/inputs/air_quality/GIS_matching_matrix.csv"))
    county_allocation_factor <- get_input_f(input_name = 'normalized_vehicles_distribution_by_county')[,2:4]
    county_allocation_factor <- county_allocation_factor %>%
      add_column("State_allocation_factor" = 0) %>%
      add_column("Year" = 0) %>%
      add_column("Damages"= 0) %>%
      add_column("Discounted_damages" = 0)
    years <- unique(state_allocation_factors$Year)
    states <- unique(state_allocation_factors$State)
    m <- 0
    for (i in years) {
      batteries_damages_per_county_temp <- county_allocation_factor
      batteries_damages_per_county_temp$Year <- i
      for (j in states) {
        batteries_damages_per_county_temp$State_allocation_factor[which(batteries_damages_per_county_temp$State_ID == j)] <- state_allocation_factors$Value[which(state_allocation_factors$State == j & state_allocation_factors$Year == i)]
      }
      batteries_damages_per_county_temp$Damages <- batteries_damages_monetized_yearly$Monetized_damages[which(batteries_damages_monetized_yearly$Year == i)]*batteries_damages_per_county_temp$State_allocation_factor*batteries_damages_per_county_temp$county_allocation_factor
      batteries_damages_per_county_temp$Discounted_damages <- batteries_damages_monetized_yearly$Discounted_damages[which(batteries_damages_monetized_yearly$Year == i)]*batteries_damages_per_county_temp$State_allocation_factor*batteries_damages_per_county_temp$county_allocation_factor
      if (m == 0) {
        batteries_damages_per_county <- batteries_damages_per_county_temp
        m <- 1
      } else {
        batteries_damages_per_county <- rbind(batteries_damages_per_county, batteries_damages_per_county_temp)
      }
    }
    batteries_damages_per_county <- select(batteries_damages_per_county, -c("county_allocation_factor", "State_allocation_factor"))
    batteries_damages_per_state <- aggregate(.~State_ID + Year, data = select(batteries_damages_per_county, -c("FIPS")), FUN = sum)
    return(list(batteries_damages_monetized_yearly = batteries_damages_monetized_yearly,
                batteries_damages_per_state = batteries_damages_per_state,
                batteries_damages_per_county = batteries_damages_per_county))
  } else if (grepl("GREET", battery_calculation_method)) {
    GREET_emissions_battery <- read.csv(paste0(getwd(), "/inputs/air_quality/battery_emissions_GREET.csv"))
    battery_energy_density <- 0.25 # kWh/kg
    battery_manufacturing_emissions <- aggregate(Value ~ Year+Size, data = fleet_vint_stock, FUN = sum) %>%
      add_column("Batteries_weight" = NA) %>%
      add_column("Batteries_capacity" = NA) %>%
      add_column("NOx" = NA) %>%
      add_column("SO2" = NA) %>%
      add_column("PM25" = NA) %>%
      add_column("VOC" = NA) %>%
      add_column("NH3" = NA)
    for (i in 1:dim(battery_manufacturing_emissions)[1]) {
      if (battery_manufacturing_emissions$Size[i] == "Car") {
        battery_capacity <- battery_capacity_car
      } else if (battery_manufacturing_emissions$Size[i] == "Light truck") {
        battery_capacity <- battery_capacity_truck
      }
      if (grepl("full", battery_calculation_method)) {
        materials <- 1
        assembly <- 1
      } else if (grepl("assembly", battery_calculation_method)) {
        materials <- 0
        assembly <- 1
      }
      battery_manufacturing_emissions$Batteries_capacity[i] <- battery_manufacturing_emissions$Value[i]*battery_capacity
      battery_manufacturing_emissions$Batteries_weight[i] <- battery_manufacturing_emissions$Batteries_capacity[i]/battery_energy_density
      battery_manufacturing_emissions$NOx[i] <- (materials*battery_manufacturing_emissions$Batteries_weight[i]*GREET_emissions_battery$NOx[which(GREET_emissions_battery$Size == battery_manufacturing_emissions$Size[i] & GREET_emissions_battery$Process == "Materials")] +
                                                 assembly*battery_manufacturing_emissions$Batteries_capacity[i]*GREET_emissions_battery$NOx[which(GREET_emissions_battery$Size == battery_manufacturing_emissions$Size[i] & GREET_emissions_battery$Process == "Assembly")])/1e6
      battery_manufacturing_emissions$SO2[i] <- (materials*battery_manufacturing_emissions$Batteries_weight[i]*GREET_emissions_battery$SO2[which(GREET_emissions_battery$Size == battery_manufacturing_emissions$Size[i] & GREET_emissions_battery$Process == "Materials")] +
                                                   assembly*battery_manufacturing_emissions$Batteries_capacity[i]*GREET_emissions_battery$SO2[which(GREET_emissions_battery$Size == battery_manufacturing_emissions$Size[i] & GREET_emissions_battery$Process == "Assembly")])/1e6
      battery_manufacturing_emissions$PM25[i] <- (materials*battery_manufacturing_emissions$Batteries_weight[i]*GREET_emissions_battery$PM25[which(GREET_emissions_battery$Size == battery_manufacturing_emissions$Size[i] & GREET_emissions_battery$Process == "Materials")] +
                                                   assembly*battery_manufacturing_emissions$Batteries_capacity[i]*GREET_emissions_battery$PM25[which(GREET_emissions_battery$Size == battery_manufacturing_emissions$Size[i] & GREET_emissions_battery$Process == "Assembly")])/1e6
      battery_manufacturing_emissions$VOC[i] <- (materials*battery_manufacturing_emissions$Batteries_weight[i]*GREET_emissions_battery$VOC[which(GREET_emissions_battery$Size == battery_manufacturing_emissions$Size[i] & GREET_emissions_battery$Process == "Materials")] +
                                                   assembly*battery_manufacturing_emissions$Batteries_capacity[i]*GREET_emissions_battery$VOC[which(GREET_emissions_battery$Size == battery_manufacturing_emissions$Size[i] & GREET_emissions_battery$Process == "Assembly")])/1e6
      battery_manufacturing_emissions$NH3[i] <- (materials*battery_manufacturing_emissions$Batteries_weight[i]*GREET_emissions_battery$NH3[which(GREET_emissions_battery$Size == battery_manufacturing_emissions$Size[i] & GREET_emissions_battery$Process == "Materials")] +
                                                   assembly*battery_manufacturing_emissions$Batteries_capacity[i]*GREET_emissions_battery$NH3[which(GREET_emissions_battery$Size == battery_manufacturing_emissions$Size[i] & GREET_emissions_battery$Process == "Assembly")])/1e6
    }
    battery_manufacturing_emissions <- select(battery_manufacturing_emissions, -c("Size", "Value", "Batteries_weight", "Batteries_capacity"))
    battery_manufacturing_emissions <- aggregate(. ~ Year, data = battery_manufacturing_emissions, FUN = sum)
    battery_manufacturing_emissions <-  filter(battery_manufacturing_emissions, battery_manufacturing_emissions$Year >= first_proj_yr)
    return(list(battery_manufacturing_emissions = battery_manufacturing_emissions))
  }
}
