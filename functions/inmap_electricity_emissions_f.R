inmap_electricity_emissions_f <- function(fleet_elec_use_tot_county, first_proj_yr = NA) {
  attribute_f("inmap_electricity_emissions_f")
  # Load the input data
  GIS_matrix <- read.csv(paste0(getwd(), "/inputs/air_quality/GIS_matching_matrix.csv"))
  power_plants <- get_input_f(input_name = 'EIA_Power_Plants') %>%
    filter(State != "Alaska" & State != "Hawaii" & State != "Puerto Rico")
  sources <- c("coal", "oil", "ng", "renewables", "nuclear")
  colnames(power_plants)[which(colnames(power_plants) == "County")] <- "CY_NAME"
  colnames(power_plants)[which(colnames(power_plants) == "State")] <- "ST_NAME"
  power_plants <- pivot_longer(power_plants, cols = 19:30, names_to = "Fuel_type", values_to = "Capacity") %>%
    filter(!is.na(Capacity))
  power_plants$Fuel_type[which(power_plants$Fuel_type == "Coal_MW")] <- "Coal"
  power_plants$Fuel_type[which(power_plants$Fuel_type == "NG_MW")] <- "Natural Gas"
  power_plants$Fuel_type[which(power_plants$Fuel_type == "Crude_MW")] <- "Oil"
  power_plants$Fuel_type[which(power_plants$Fuel_type == "Nuclear_MW")] <- "Nuclear"
  power_plants$Fuel_type[grep("_MW",power_plants$Fuel_type)] <- "Renewables"
  power_plants <- aggregate(power_plants$Capacity, list(power_plants$Longitude, power_plants$Latitude, power_plants$CY_NAME, power_plants$ST_NAME, power_plants$Fuel_type), sum)
  colnames(power_plants) <- c("Longitude", "Latitude", "CY_NAME", "ST_NAME", "Fuel_type", "Capacity_MW")
  
  # Combine with the geographical information dataset
  power_plants <- left_join(power_plants, select(GIS_matrix, c("FIPS", "Cambium.GEA", "CY_NAME", "ST_NAME")), by = c("CY_NAME", "ST_NAME")) %>%
    filter(!is.na('FIPS')) %>%
    add_column("Rowname" = 0)
  power_plants$Rowname <- as.numeric(row.names(power_plants))
  
  # Import into the inmap dataset
  counties <- tigris::counties() %>%
    select(c("STATEFP", "COUNTYFP", "GEOID", "geometry"))
  inmap_input <- read_sf(paste0(getwd(), "/inputs/air_quality/", "inmap_template.shp")) %>%
    select(c("geometry")) %>%
    add_column("Rowname_inmap" = NA)
  inmap_input$Rowname_inmap <- as.numeric(rownames(inmap_input))
  power_plants$geometry <- mapply(function(x,y) st_point(c(x,y)),power_plants$Longitude,power_plants$Latitude,SIMPLIFY = FALSE)
  power_plants$geometry <- st_sfc(power_plants$geometry)
  st_crs(power_plants$geometry) <- 4326
  power_plants$geometry <- st_transform(power_plants$geometry, st_crs(inmap_input$geometry))
  power_plants_intersection <- data.frame(st_intersects(power_plants$geometry, inmap_input$geometry))
  colnames(power_plants_intersection) <- c("Rowname", "Rowname_inmap")
  power_plants_inmap <- left_join(power_plants, left_join(power_plants_intersection, inmap_input, by = "Rowname_inmap"), by = "Rowname")
  power_plants <- select(power_plants_inmap, -c("geometry.x", "geometry.y"))
  
  # Normalize the electricity production at the GEA level
  GEA_capacity <- aggregate(power_plants$Capacity_MW, list(power_plants$Cambium.GEA, power_plants$Fuel_type), sum) 
  colnames(GEA_capacity) <- c("Cambium.GEA", "Fuel_type", "GEA_Capacity_MW")
  power_plants <- left_join(power_plants, GEA_capacity, by = c("Cambium.GEA", "Fuel_type"))
  power_plants$Normalized_Capacity <- power_plants$Capacity_MW/power_plants$GEA_Capacity_MW
  
  # Combine with the electricity mix scenario
  reg_matrix_mix <- do.call(cambium_el_grid_scenario_f, list())[["grid_mix_reg"]]
  colnames(reg_matrix_mix) <- c("Year", "Cambium.GEA", "Fuel_type", "Value")
  #reg_matrix_mix$Fuel_type[which(reg_matrix_mix$Fuel_type == "Coal")] <- "coal"
  #reg_matrix_mix$Fuel_type[which(reg_matrix_mix$Fuel_type == "Natural Gas")] <- "ng"
  #reg_matrix_mix$Fuel_type[which(reg_matrix_mix$Fuel_type == "Nuclear")] <- "nuclear"
  reg_matrix_mix$Fuel_type[which(reg_matrix_mix$Fuel_type == "Other")] <- "Oil"
  reg_matrix_mix$Fuel_type[which(reg_matrix_mix$Fuel_type == "Renewable")] <- "Renewables"
  #reg_matrix_mix$Fuel_type[which(reg_matrix_mix$Fuel_type == "Biomass")] <- "renewables"
  reg_matrix_mix <- aggregate(reg_matrix_mix$Value, list(reg_matrix_mix$Year, reg_matrix_mix$Cambium.GEA, reg_matrix_mix$Fuel_type), sum)
  colnames(reg_matrix_mix) <- c("Year", "Cambium.GEA", "Fuel_type", "Value")
  electric_mix <- pivot_wider(reg_matrix_mix[which(reg_matrix_mix$Year >= first_proj_yr),], names_from = Year, values_from = Value)
  power_plants <- left_join(power_plants, electric_mix, by = c("Cambium.GEA", "Fuel_type"))
  power_plants[,13:41] <- power_plants[,13:41]*power_plants$Normalized_Capacity
  
  # Add data on the fleet electricity consumption by county (from FLAME-AQ) and aggregate at the GEA level
  fleet_elec_use_tot_county <- pivot_wider(fleet_elec_use_tot_county, names_from = Year, values_from = Value)
  colnames(fleet_elec_use_tot_county)[which(colnames(fleet_elec_use_tot_county) == "ST_FIPS")] <- "State_ID"
  fleet_elec_use_tot_county <- left_join(fleet_elec_use_tot_county, select(GIS_matrix, c("FIPS", "Cambium.GEA")), by = c("FIPS"))
  elec_use_gea <- aggregate(.~ Cambium.GEA, data = select(fleet_elec_use_tot_county, -c("FIPS", "State_ID", "Fuel", "Unit")), FUN = sum)
  elec_consumption <- left_join(select(power_plants, c("Longitude", "Latitude", "Fuel_type", "Cambium.GEA")), elec_use_gea, by = c("Cambium.GEA"))
  power_plants[,13:41] <- power_plants[,13:41]*elec_consumption[,6:34]
  power_plants <- select(power_plants, -c("GEA_Capacity_MW", "Normalized_Capacity", "Capacity_MW"))
  
  # Add the emission factors
  ef_egrid <- get_input_f(input_name = 'ef_pollutants_egrid')
  colnames(ef_egrid) <- c("Cambium.GEA", "Fuel_type", "Pollutant", "Emission_factor", "Unit")
  ef_egrid$Cambium.GEA <- paste0(ef_egrid$Cambium.GEA, "c")
  ef_egrid$Emission_factor <- ef_egrid$Emission_factor*0.454*1e-6
  ef_egrid$Unit <- "tons/kWh"
  ef_egrid <- pivot_wider(ef_egrid, names_from = Pollutant, values_from = Emission_factor)
  power_plants <- pivot_longer(power_plants, cols = 10:38, names_to = "Year", values_to = "Production")
  power_plants <- left_join(power_plants, ef_egrid, by = c("Cambium.GEA", "Fuel_type"))
  power_plants[,13:17] <- power_plants[,13:17]*power_plants$Production
  power_plants$Unit <- "tons"
  
  # Generate the final dataset
  power_plant_emissions <- select(power_plants, -c("Production")) %>%
    filter(Fuel_type != "renewables")
  
  #Format specifically for Inmap (unit moves to tons)
  power_plant_emissions <- select(power_plant_emissions, c("Rowname_inmap", "Year", "NOx", "SO2", "PM25", "NH3", "VOC")) %>%
    relocate("Rowname_inmap", .after = "VOC")
  power_plant_emissions <- aggregate(.~Year+Rowname_inmap, data = power_plant_emissions, FUN = sum)
  emissions_electricity <- left_join(power_plant_emissions, inmap_input, by = "Rowname_inmap") %>%
    select(-c("Rowname_inmap"))
  colnames(emissions_electricity) <- c("Year", "NOx", "SOx", "PM2_5", "NH3", "VOC", "geometry")
  return(emissions_electricity)
}
