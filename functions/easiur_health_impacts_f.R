easiur_health_impacts_f <- function(fleet_elec_use_tot_county, vehicles_emissions_county, fleet_fuel_usage_tot_county, oil_gas_activity, electricity_emissions_county, scenario_id = NA) {
  # Generate the total of emissions in the same file as the inmap input; the online tool of easiur will do the conversion
  attribute_f("easiur_health_impacts_f")
  GIS_matching_matrix <- read.csv(paste0(getwd(), "/inputs/air_quality/GIS_matching_matrix.csv"))
  results_path <- paste0(getwd(), "/outputs/air_quality/", Sys.Date(), "_", scenario_id, "_results")
  ## Generate electricity production emissions
  print("Generate the electricity production emissions for easiur")
  emissions_electricity <- do.call(inmap_electricity_emissions_f, list(fleet_elec_use_tot_county = fleet_elec_use_tot_county))
  ## Generate vehicles emissions
  print("Generate the vehicles emissions for easiur")
  emissions_vehicles <- do.call(inmap_vehicles_emissions_f, list(vehicles_emissions_county = vehicles_emissions_county))
  ## Generate refining emissions
  print("Generate the refining emissions for easiur")
  emissions_oil_processing <- do.call(inmap_refining_emissions_f, list(fleet_fuel_usage_tot_county = fleet_fuel_usage_tot_county, oil_gas_activity = oil_gas_activity))
  
  ### Calculate the impacts at the county level
  ## Refining emissions at the county level
  oil_refining_emissions <- get_input_f(input_name = 'COBRA_emissions_baseline_template')
  ## Define the indexes
  index_petroleum <- which(oil_refining_emissions$TIER1 == 6 & oil_refining_emissions$TIER2 == 1 & oil_refining_emissions$TIER3 == 99)
  index_petroleum_storage <- which(oil_refining_emissions$TIER1 == 9 & oil_refining_emissions$TIER2 == 2 & (oil_refining_emissions$TIER3 == 2 | oil_refining_emissions$TIER3 == 4 | oil_refining_emissions$TIER3 == 6 | oil_refining_emissions$TIER3 == 8))
  index_refining <- which(oil_refining_emissions$TIER1 == 6 & oil_refining_emissions$TIER2 == 2)
  index_refined_storage <- which(oil_refining_emissions$TIER1 == 9 & oil_refining_emissions$TIER2 == 2 &(oil_refining_emissions$TIER3 == 1 | oil_refining_emissions$TIER3 == 3 | oil_refining_emissions$TIER3 == 5 | oil_refining_emissions$TIER3 == 7 | oil_refining_emissions$TIER3 == 9 | oil_refining_emissions$TIER3 == 10))
  index_refined_storage_bis <- which(oil_refining_emissions$TIER1 == 9 & oil_refining_emissions$TIER2 == 3 & oil_refining_emissions$TIER3 != 5)
  index_refined_distribution <- which(oil_refining_emissions$TIER1 == 9 & (oil_refining_emissions$TIER2 == 4 | oil_refining_emissions$TIER2 == 5 | oil_refining_emissions$TIER2 == 6))
  index_ethanol <- which(oil_refining_emissions$TIER1 == 7 & oil_refining_emissions$TIER2 == 99 & oil_refining_emissions$TIER3 == 1)
  index_ng <- which(oil_refining_emissions$TIER1 == 6 & oil_refining_emissions$TIER2 == 1 & oil_refining_emissions$TIER3 == 1)
  ###   Sub-classification of the indexes
  index_demand <- c(index_refined_distribution, index_refined_storage_bis, index_refined_storage)
  index_refining <- c(index_refining)
  index_crude <- c(index_petroleum_storage, index_petroleum)
  index_total_oil <- c(index_demand, index_refining, index_crude, index_ethanol, index_ng)
  ###   Emissions from changes in demand
  years <- unique(oil_gas_activity$Year)
  for (i in years) {
    if (i > min(years)) {
      oil_gas_activity[which(oil_gas_activity$Year == i), 3:9] <- oil_gas_activity[which(oil_gas_activity$Year == i), 3:9]*oil_gas_activity[which(oil_gas_activity$Year == i-1), 3:9]
    }
  }
  fun <- function(i) {
    oil_dataset <- oil_refining_emissions
    oil_gas_changes <- left_join(oil_gas_activity[which(oil_gas_activity$Year == i),], select(GIS_matching_matrix, c("PADD", "COBRA_SOURCEINDX")), by = "PADD", relationship = "many-to-many")
    colnames(oil_gas_changes)[10] <- "sourceindx"
    oil_dataset <- left_join(oil_dataset, select(oil_gas_changes, -c("PADD", "Year")), by = "sourceindx")
    oil_dataset[index_demand,9:14] <- oil_dataset[index_demand,9:14]*oil_dataset$Rel_Demand[index_demand]
    oil_dataset[index_refining,9:14] <- oil_dataset[index_refining,9:14]*oil_dataset$Rel_Refining[index_refining]
    oil_dataset[index_crude,9:14] <- oil_dataset[index_crude,9:14]*oil_dataset$Rel_Crude_oil[index_crude]
    oil_dataset[index_ethanol,9:14] <- oil_dataset[index_ethanol,9:14]*oil_dataset$Rel_Ethanol[index_ethanol]
    oil_dataset[index_ng,9:14] <- oil_dataset[index_ng,9:14]*oil_dataset$Rel_Natural_Gas_fleet[index_ng]
    oil_dataset <- filter(oil_dataset, !is.na(oil_dataset$Rel_Demand))
    oil_dataset <- oil_dataset[index_total_oil,1:14]
    ## Extract the emissions from oil&gas refining
    oil_gas_emissions <- select(oil_dataset, -c("ID", "typeindx", "stid", "cyid", "TIER1", "TIER2", "TIER3")) %>%
      add_column("Year" = i)
    colnames(oil_gas_emissions)[1] <- "COBRA_SOURCEINDX"
    oil_gas_emissions <- aggregate(.~Year+COBRA_SOURCEINDX, data = oil_gas_emissions, FUN = sum)
    return(oil_gas_emissions)
  }
  oil_gas_emissions <- do.call(rbind, lapply(years, fun))
  oil_gas_emissions_county <- left_join(oil_gas_emissions, select(GIS_matching_matrix, c("COBRA_SOURCEINDX", "FIPS", "ST_FIPS")), by = "COBRA_SOURCEINDX") %>%
    select(-c("COBRA_SOURCEINDX"))
  oil_gas_emissions_state <- aggregate(.~Year+ST_FIPS, data = select(oil_gas_emissions_county, -c("FIPS")), FUN = sum)
  oil_gas_emissions <- aggregate(.~Year, data = select(oil_gas_emissions_state, -c("ST_FIPS")), FUN = sum)
  ## Allocate emissions to counties
  EASIUR_county <- read.csv(paste0(getwd(), "/inputs/air_quality/EASIUR_Marginal-Social-Cost.csv"))
  vehicles_emissions_county$PM25 <- vehicles_emissions_county$Total_PM25+vehicles_emissions_county$Brake_PM25+vehicles_emissions_county$Tire_PM25
  electricity_emissions <- aggregate(.~FIPS+Year, data = select(electricity_emissions_county[["fleet_elec_emissions"]], -c("Fuel_type", "Reg_entity", "Unit")), FUN = sum)
  electricity_emissions$Year <- as.numeric(electricity_emissions$Year)
  total_emissions_county <- left_join(select(vehicles_emissions_county, c("Year", "FIPS", "NOx", "SO2", "PM25", "NH3")), left_join(select(oil_gas_emissions_county, -c("ST_FIPS", "SOA", "VOC")), select(electricity_emissions, -c("VOC")), by = c("Year", "FIPS")), by = c("Year", "FIPS"))
  total_emissions_county$NOx.y[is.na(total_emissions_county$NOx.y)] <- 0
  total_emissions_county$SO2.y[is.na(total_emissions_county$SO2.y)] <- 0
  total_emissions_county$PM25.y[is.na(total_emissions_county$PM25.y)] <- 0
  total_emissions_county$NH3.y[is.na(total_emissions_county$NH3.y)] <- 0
  emissions_dataset_county <- data.frame(Year = total_emissions_county$Year,
                                  FIPS = total_emissions_county$FIPS,
                                  NOx = total_emissions_county$NOx.x+total_emissions_county$NO2+total_emissions_county$NOx.y,
                                  SO2 = total_emissions_county$SO2+total_emissions_county$SO2.x+total_emissions_county$SO2.y,
                                  PM25 = total_emissions_county$PM25+total_emissions_county$PM25.x+total_emissions_county$PM25.y,
                                  NH3 = total_emissions_county$NH3+total_emissions_county$NH3.x+total_emissions_county$NH3.y)
  emissions_dataset_county <- left_join(emissions_dataset_county, EASIUR_county, by = "FIPS")
  emissions_damages_county <- data.frame(Year = emissions_dataset_county$Year,
                                         FIPS = emissions_dataset_county$FIPS,
                                         Damages = emissions_dataset_county$NOx.x*emissions_dataset_county$NOx.y+
                                                   emissions_dataset_county$SO2.x*emissions_dataset_county$SO2.y+
                                                   emissions_dataset_county$PM25.x*emissions_dataset_county$PM25.y+
                                                   emissions_dataset_county$NH3.x*emissions_dataset_county$NH3.y)
  emissions_damages <- aggregate(.~Year, data = select(emissions_damages_county, -c("FIPS")), FUN = sum)
  write.csv(emissions_damages_county, paste0(results_path, "/EASIUR_total_damages_REScounty_county.csv"), row.names = FALSE)
  write.csv(emissions_damages, paste0(results_path, "/EASIUR_total_damages_REScounty_total.csv"), row.names = FALSE)
  

  # Generate the centroid of each geometric element
  emissions_electricity$geometry <- st_centroid(emissions_electricity$geometry)
  emissions_electricity <- emissions_electricity %>%
    add_column("x" = NA) %>%
    add_column("y" = NA)
  emissions_electricity$geometry <- st_transform(emissions_electricity$geometry, 4269)
  emissions_electricity$x <- trunc(st_coordinates(emissions_electricity$geometry)[,1]*1e3)/1e3
  emissions_electricity$y <- trunc(st_coordinates(emissions_electricity$geometry)[,2]*1e3)/1e3
  colnames(emissions_electricity)[8:9] <- c("Longitude", "Latitude")
  emissions_vehicles$geometry <- st_centroid(emissions_vehicles$geometry)
  emissions_vehicles <- emissions_vehicles %>%
    add_column("x" = NA) %>%
    add_column("y" = NA)
  emissions_vehicles$geometry <- st_transform(emissions_vehicles$geometry, 4269)
  emissions_vehicles$x <- trunc(st_coordinates(emissions_vehicles$geometry)[,1]*1e3)/1e3
  emissions_vehicles$y <- trunc(st_coordinates(emissions_vehicles$geometry)[,2]*1e3)/1e3
  colnames(emissions_vehicles)[8:9] <- c("Longitude", "Latitude")
  emissions_oil_processing$geometry <- st_centroid(emissions_oil_processing$geometry)
  emissions_oil_processing <- emissions_oil_processing %>%
    add_column("x" = NA) %>%
    add_column("y" = NA)
  emissions_oil_processing$geometry <- st_transform(emissions_oil_processing$geometry, 4269)
  emissions_oil_processing$x <- trunc(st_coordinates(emissions_oil_processing$geometry)[,1]*1e3)/1e3
  emissions_oil_processing$y <- trunc(st_coordinates(emissions_oil_processing$geometry)[,2]*1e3)/1e3
  colnames(emissions_oil_processing)[8:9] <- c("Longitude", "Latitude")
  
  ## Locate emissions to the EASIUR grid
  easiur_inmap_correspondence <- read.csv(paste0(getwd(), "/inputs/air_quality/", "easiur_inmap_correspondence.csv"))
  easiur_inmap_correspondence$Longitude <- trunc(easiur_inmap_correspondence$Longitude*1e3)/1e3
  easiur_inmap_correspondence$Latitude <- trunc(easiur_inmap_correspondence$Latitude*1e3)/1e3
  damages_electricity <- left_join(emissions_electricity, easiur_inmap_correspondence, by = c("Longitude", "Latitude"))
  damages_electricity$NOx <- damages_electricity$NOx*damages_electricity$NOX.Annual.Ground
  damages_electricity$SOx <- damages_electricity$SOx*damages_electricity$SO2.Annual.Ground
  damages_electricity$PM2_5 <- damages_electricity$PM2_5*damages_electricity$PM25.Annual.Ground
  damages_electricity$NH3 <- damages_electricity$NH3*damages_electricity$NH3.Annual.Ground
  damages_electricity <- select(damages_electricity, c("Year", "NOx", "SOx", "PM2_5", "NH3", "geometry"))
  damages_vehicles <- left_join(emissions_vehicles, easiur_inmap_correspondence, by = c("Longitude", "Latitude"))
  damages_vehicles$NOx <- damages_vehicles$NOx*damages_vehicles$NOX.Annual.Ground
  damages_vehicles$SOx <- damages_vehicles$SOx*damages_vehicles$SO2.Annual.Ground
  damages_vehicles$PM2_5 <- damages_vehicles$PM2_5*damages_vehicles$PM25.Annual.Ground
  damages_vehicles$NH3 <- damages_vehicles$NH3*damages_vehicles$NH3.Annual.Ground
  damages_vehicles <- select(damages_vehicles, c("Year", "NOx", "SOx", "PM2_5", "NH3", "geometry"))
  damages_oil <- left_join(emissions_oil_processing, easiur_inmap_correspondence, by = c("Longitude", "Latitude"))
  damages_oil$NOx <- damages_oil$NOx*damages_oil$NOX.Annual.Ground
  damages_oil$SOx <- damages_oil$SOx*damages_oil$SO2.Annual.Ground
  damages_oil$PM2_5 <- damages_oil$PM2_5*damages_oil$PM25.Annual.Ground
  damages_oil$NH3 <- damages_oil$NH3*damages_oil$NH3.Annual.Ground
  damages_oil <- select(damages_oil, c("Year", "NOx", "SOx", "PM2_5", "NH3", "geometry"))
  
  # Aggregate the calculated damages
  inmap_input <- read_sf(paste0(getwd(), "/inputs/air_quality/", "inmap_template.shp")) %>%
    select(c("geometry")) %>%
    add_column("Rowname_inmap" = 0)
  inmap_input$Rowname_inmap <- as.numeric(row.names(inmap_input))
  air_quality_damages <- rbind(damages_electricity, damages_oil, damages_vehicles) %>%
    add_column("Rowname" = 0)
  air_quality_damages$Rowname <- as.numeric(row.names(air_quality_damages))
  air_quality_damages$geometry <- st_transform(air_quality_damages$geometry, st_crs(inmap_input$geometry)) %>%
    st_sfc()
  inmap_interesect <- data.frame(st_intersects(air_quality_damages$geometry, inmap_input$geometry))
  colnames(inmap_interesect) <- c("Rowname", "Rowname_inmap")
  air_quality_damages <- left_join(air_quality_damages, left_join(inmap_interesect, inmap_input, by = "Rowname_inmap"), by = "Rowname") %>%
    select(-c("geometry.x", "Rowname", "Rowname_inmap"))
  colnames(air_quality_damages)[6] <- "geometry"
  write.csv(air_quality_damages, paste0(results_path, "/EASIUR_damages.csv"))
  st_write(air_quality_damages, paste0(results_path, "/EASIUR_damages.shp"))
  
  # Export the total damages
  air_quality_damages_total <- aggregate(.~Year, data = select(air_quality_damages, -c("geometry")), FUN = sum) %>%
    add_column("Total" = 0)
  air_quality_damages_total$Total <- air_quality_damages_total$NOx + air_quality_damages_total$SOx + air_quality_damages_total$PM2_5 + air_quality_damages_total$NH3
  write.csv(air_quality_damages_total, paste0(results_path, "/EASIUR_total_damages.csv"), row.names = FALSE)
}
