health_damages_batteries_f <- function(fleet_composition, battery_capacity_car = NA, battery_capacity_truck = NA, damage_per_kwh = NA, discount_rate = NA, first_proj_yr = NA, battery_calculation_method = NA, battery_energy_density  = NA, fleet_id = NA) {
  attribute_f("health_damages_batteries_f")
  fleet_vint_stock_county <- fleet_composition$fleet_vint_stock_county
  fleet_vint_stock <- fleet_composition$fleet_vint_stock
  first_proj_yr <- 2022
  technologies <- c("BEV100", "BEV300")
  fleet_vint_stock_county <- filter(fleet_vint_stock_county, fleet_vint_stock_county$Age == 0 & fleet_vint_stock_county$Technology%in%technologies) %>%
    select(-c("Age", "Technology"))
  fleet_vint_stock <- filter(fleet_vint_stock, fleet_vint_stock$Age == 0 & fleet_vint_stock$Technology%in%technologies) %>%
    select(-c("Age", "Technology"))
  if (battery_calculation_method == "Tessum2014") {
    # Calculation of the monetized damages from batteries manufacturing
    batteries_damages_monetized_County <- aggregate(Value ~ Year+Size+FIPS+ST_FIPS, data = fleet_vint_stock_county, FUN = sum) %>%
      add_column("Battery_capacity" = NA) %>%
      add_column("Manufactured_capacity" = NA) %>%
      add_column("Monetized_damages" = NA) 
    batteries_damages_monetized_County$Battery_capacity[which(batteries_damages_monetized_County$Size == "Car")] <- battery_capacity_car
    batteries_damages_monetized_County$Battery_capacity[which(batteries_damages_monetized_County$Size == "Light truck")] <- battery_capacity_truck
    batteries_damages_monetized_County$Manufactured_capacity <- batteries_damages_monetized_County$Value*batteries_damages_monetized_County$Battery_capacity
    batteries_damages_monetized_County$Monetized_damages <- batteries_damages_monetized_County$Manufactured_capacity*damage_per_kwh
    batteries_damages_monetized_yearly_county <- aggregate(Monetized_damages ~ Year+FIPS+ST_FIPS, data = select(batteries_damages_monetized_County, -c("Size", "Value", "Battery_capacity", "Manufactured_capacity")), FUN = sum) %>%
      add_column("Discounted_damages" = NA)
    batteries_damages_monetized_yearly_county$Discounted_damages <- batteries_damages_monetized_yearly_county$Monetized_damages*1/(1+discount_rate/100)^(batteries_damages_monetized_yearly_county$Year-first_proj_yr+1)
    batteries_damages_monetized_yearly_county <- filter(batteries_damages_monetized_yearly_county, batteries_damages_monetized_yearly_county$Year >= first_proj_yr) %>%
      select(-c("Monetized_damages"))
    batteries_damages_monetized_yearly_state <- aggregate(.~Year + ST_FIPS, data = select(batteries_damages_monetized_yearly_county, -c("FIPS")), FUN = sum)
    batteries_damages_monetized_yearly <- aggregate(.~Year, data = select(batteries_damages_monetized_yearly_state, -c("ST_FIPS")), FUN = sum)
    return(list(batteries_damages_monetized_yearly = batteries_damages_monetized_yearly,
                batteries_damages_monetized_yearly_state = batteries_damages_monetized_yearly_state,
                batteries_damages_monetized_yearly_county = batteries_damages_monetized_yearly_county))
  } else if (grepl("GREET", battery_calculation_method)) {
    GREET_emissions_battery <- read.csv(paste0(getwd(), "/inputs/air_quality/battery_emissions_GREET.csv"))
    battery_energy_density <- 0.25 # kWh/kg
    battery_manufacturing_emissions <- aggregate(Value ~ Year+Size, data = fleet_vint_stock, FUN = sum) %>%
      add_column("Batteries_weight" = NA) %>%
      add_column("Batteries_capacity" = NA)
    battery_manufacturing_emissions$Batteries_capacity[which(battery_manufacturing_emissions$Size == "Car")] <- battery_capacity_car
    battery_manufacturing_emissions$Batteries_capacity[which(battery_manufacturing_emissions$Size == "Light truck")] <- battery_capacity_truck
    if (grepl("full", battery_calculation_method)) {
      materials <- 1
      assembly <- 1
    } else if (grepl("assembly", battery_calculation_method)) {
      materials <- 0
      assembly <- 1
    }
    battery_manufacturing_emissions$Batteries_capacity <- battery_manufacturing_emissions$Batteries_capacity*battery_manufacturing_emissions$Value
    battery_manufacturing_emissions$Batteries_weight <- battery_manufacturing_emissions$Batteries_capacity/battery_energy_density
    ## Calculate the pollutants emissions
    battery_manufacturing_emissions <- left_join(battery_manufacturing_emissions, GREET_emissions_battery, by = "Size", relationship = "many-to-many")
    battery_manufacturing_emissions[which(battery_manufacturing_emissions$Process == "Materials"),7:11] <- materials*battery_manufacturing_emissions[which(battery_manufacturing_emissions$Process == "Materials"),7:11]*battery_manufacturing_emissions$Batteries_weight[which(battery_manufacturing_emissions$Process == "Materials")]/1e6
    battery_manufacturing_emissions[which(battery_manufacturing_emissions$Process == "Assembly"),7:11] <- assembly*battery_manufacturing_emissions[which(battery_manufacturing_emissions$Process == "Assembly"),7:11]*battery_manufacturing_emissions$Batteries_capacity[which(battery_manufacturing_emissions$Process == "Assembly")]/1e6
    ## Finalize the dataset
    battery_manufacturing_emissions <- aggregate(.~Year, data = select(battery_manufacturing_emissions, -c("Size", "Value", "Batteries_weight", "Batteries_capacity", "Process", "Unit")), FUN = sum)
    battery_manufacturing_emissions <-  filter(battery_manufacturing_emissions, battery_manufacturing_emissions$Year >= first_proj_yr)
    return(list(battery_manufacturing_emissions = battery_manufacturing_emissions))
  }
}
