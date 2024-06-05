inmap_refining_emissions_f <- function(fleet_fuel_usage_tot_county, oil_gas_activity) {
  attribute_f("inmap_refining_emissions_f")
  refineries_loc <- get_input_f(input_name = 'Petroleum_Refineries')
  refineries_data <- get_input_f(input_name = 'refinery_rank_2023')
  refineries_loc$Site <- tolower(refineries_loc$Site)
  refineries_data$Site <- tolower(refineries_data$Site)
  refineries_loc$Company <- tolower(refineries_loc$Company)
  refineries_data$Company <- tolower(refineries_data$Company)
  refineries_data <- left_join(refineries_data, select(refineries_loc, c("Company", "Site", "State", "PADD", "Latitude", "Longitude")), by = c("State", "Site", "Company")) %>%
    filter(!is.na(PADD))
  
  # Calculate the share of each refinery in its PADD
  padd_production <- aggregate(.~PADD, data = select(refineries_data, c("Barrels per calendar day", "PADD")), FUN = sum)
  refineries_data <- left_join(refineries_data, padd_production, by = "PADD")
  refineries_data$'share_in_padd' <- refineries_data$'Barrels per calendar day.x'/refineries_data$'Barrels per calendar day.y'
  refineries_data <- select(refineries_data, c("State", "Site", "PADD", "Latitude", "Longitude", "share_in_padd"))
  
  # Import the oil wells dataset (Source: Oak Ridge National Laboratory)
  #oil_gas_wells <- get_input_f(input_name = 'Oil_and_Natural_Gas_Wells') %>%
  oil_gas_wells <- read.csv(paste0(getwd(), "/inputs/air_quality/", "Oil_and_Natural_Gas_Wells.csv")) %>%
    select(c("STATE", "TYPE", "STATUS", "COUNTYFIPS", "LATITUDE", "LONGITUDE", "PRODTYPE", "SURF_LAT", "SURF_LONG")) %>%
    filter(STATUS%in%c("PRODUCING WELL")) %>%
    filter(COUNTYFIPS != "NOT AVAILABLE")
  oil_gas_wells$COUNTYFIPS <- as.numeric(oil_gas_wells$COUNTYFIPS)

  
  # Import the baseline emissions from COBRA
  baseline_emissions <- get_input_f(input_name = 'COBRA_emissions_baseline')
  colnames(baseline_emissions)[3] <- "COBRA_SOURCEINDX"
  GIS_matrix <- read.csv(paste0(getwd(), "/inputs/air_quality/GIS_matching_matrix.csv"))
  refining_emissions <- aggregate(.~PADD, data = select(left_join(filter(baseline_emissions, baseline_emissions$TIER1 == 6 & baseline_emissions$TIER2 == 2 & baseline_emissions$TIER3 == 99), select(GIS_matrix, c("COBRA_SOURCEINDX", "PADD")), by = "COBRA_SOURCEINDX"), c("NO2", "SO2", "NH3", "SOA", "PM25", "VOC", "PADD")), FUN = sum)
  colnames(refining_emissions) <- c("PADD", "NOx", "SO2", "NH3", "SOA", "PM25", "VOC")
  
  # Import the refining activity changes from FLAME-AQ
  for (i in 2:length(unique(oil_gas_activity$Year))) {
    for (j in unique(oil_gas_activity$PADD)) {
      index <- which(oil_gas_activity$Year == unique(oil_gas_activity$Year)[i] & oil_gas_activity$PADD == j)
      index_previous <- which(oil_gas_activity$Year == unique(oil_gas_activity$Year)[i]-1 & oil_gas_activity$PADD == j)
      oil_gas_activity$Rel_Demand[index] <- oil_gas_activity$Rel_Demand[index]*oil_gas_activity$Rel_Demand[index_previous]
      oil_gas_activity$Rel_Refining[index] <- oil_gas_activity$Rel_Refining[index]*oil_gas_activity$Rel_Refining[index_previous]
      oil_gas_activity$Rel_Crude_oil[index] <- oil_gas_activity$Rel_Crude_oil[index]*oil_gas_activity$Rel_Crude_oil[index_previous]
      oil_gas_activity$Rel_Ethanol[index] <- oil_gas_activity$Rel_Ethanol[index]*oil_gas_activity$Rel_Ethanol[index_previous]
      oil_gas_activity$Rel_Natural_Gas[index] <- oil_gas_activity$Rel_Natural_Gas[index]*oil_gas_activity$Rel_Natural_Gas[index_previous]
    }
  }
  refining_activity <- oil_gas_activity %>%
    select(c("PADD", "Year", "Rel_Refining"))
  refineries_data <- left_join(refineries_data, refining_activity, by = "PADD", relationship = "many-to-many")
  refineries_data <- left_join(refineries_data, refining_emissions, by = "PADD")
  refineries_data[,9:14] <- refineries_data[,9:14]*refineries_data$Rel_Refining*refineries_data$share_in_padd
  
  # Distribute the emissions from extraction
  colnames(baseline_emissions)[4:5] <- c("ST_FIPS", "CY_FIPS")
  colnames(oil_gas_wells)[4] <- "FIPS"
  total_extraction_emissions <- aggregate(.~ST_FIPS+CY_FIPS, data = select(filter(baseline_emissions, baseline_emissions$TIER1 == 6 & baseline_emissions$TIER2 == 1), -c("ID", "typeindx", "COBRA_SOURCEINDX", "TIER1", "TIER2", "TIER3")), FUN = sum)
  extraction_emissions <- left_join(oil_gas_wells, left_join(select(GIS_matrix, c("ST_FIPS", "CY_FIPS", "FIPS", "PADD")), total_extraction_emissions, by = c("ST_FIPS", "CY_FIPS")), by = "FIPS", relationship = "many-to-many")
  nbr_wells_county <- add_column(oil_gas_wells, "Count" = 1)
  nbr_wells_county <- aggregate(Count~FIPS, select(nbr_wells_county, c("Count", "FIPS")), FUN = sum)
  extraction_emissions <- left_join(extraction_emissions, nbr_wells_county, by = "FIPS") %>%
    select(-c("STATE", "TYPE", "STATUS", "PRODTYPE", "SURF_LAT", "SURF_LONG"))
  colnames(extraction_emissions)[7:12] <- c("NOx", "SO2", "NH3", "SOA", "PM25", "VOC") 
  extraction_emissions[,7:12] <- extraction_emissions[,7:12]/extraction_emissions$Count
  
  # Import the inmap input
  inmap_input <- read_sf(paste0(getwd(), "/inputs/air_quality/", "inmap_template.shp")) %>%
    select(c("geometry")) %>%
    add_column("Rowname_inmap" = NA)
  inmap_input$Rowname_inmap <- as.numeric(rownames(inmap_input))
  
  # Calculate the changes in emissions from extraction over the years and convert to Inmap format
  extraction_activity <- oil_gas_activity %>%
    select(c("PADD", "Year", "Rel_Crude_oil"))
  extraction_emissions$geometry <- mapply(function(x,y) st_point(c(x,y)),extraction_emissions$LONGITUDE,extraction_emissions$LATITUDE ,SIMPLIFY = FALSE) %>%
    st_sfc()
  st_crs(extraction_emissions$geometry) <- 4326
  extraction_emissions$geometry <- st_transform(extraction_emissions$geometry, st_crs(inmap_input$geometry))
  extraction_emissions_intersection <- data.frame(st_intersects(extraction_emissions$geometry, inmap_input$geometry))
  colnames(extraction_emissions_intersection) <- c("Rowname", "Rowname_inmap")
  extraction_emissions <- add_column(extraction_emissions, "Rowname" = NA)
  extraction_emissions$Rowname <- as.numeric(rownames(extraction_emissions))
  extraction_emissions <- left_join(extraction_emissions, extraction_emissions_intersection, by = "Rowname")
  extraction_emissions <- aggregate(.~PADD + Rowname_inmap, data = select(extraction_emissions, c("PADD", "NOx", "SO2", "NH3", "PM25", "VOC", "Rowname_inmap")), FUN = sum)
  extraction_emissions <- left_join(extraction_emissions, extraction_activity, by = "PADD", relationship = "many-to-many")
  extraction_emissions[,3:7] <- extraction_emissions[,3:7]*extraction_emissions$Rel_Crude_oil
  extraction_emissions <- select(extraction_emissions, -c("PADD", "Rel_Crude_oil")) %>%
    relocate(Year, .before = "Rowname_inmap")
  
  # Convert emissions to the InMap dataset
  refineries_data$geometry <- mapply(function(x,y) st_point(c(x,y)),refineries_data$Longitude,refineries_data$Latitude,SIMPLIFY = FALSE) %>%
    st_sfc()
  st_crs(refineries_data$geometry) <- 4326
  refineries_data$geometry <- st_transform(refineries_data$geometry, st_crs(inmap_input$geometry))
  refineries_data_intersection <- data.frame(st_intersects(refineries_data$geometry, inmap_input$geometry))
  colnames(refineries_data_intersection) <- c("Rowname", "Rowname_inmap")
  refineries_data <- add_column(refineries_data, "Rowname" = NA)
  refineries_data$Rowname <- as.numeric(rownames(refineries_data))
  refineries_data <- left_join(refineries_data, refineries_data_intersection, by = "Rowname")
  refineries_data <- aggregate(.~Year + Rowname_inmap, data = select(refineries_data, c("Year", "NOx", "SO2", "NH3", "PM25", "VOC", "Rowname_inmap")), FUN = sum)
  
  # Combine the refineries emissions and the extraction emissions
  oil_processing_emissions <- rbind(extraction_emissions, refineries_data)
  oil_processing_emissions <- aggregate(.~Year + Rowname_inmap, data = oil_processing_emissions, FUN = sum) %>%
    left_join(inmap_input, by = "Rowname_inmap") %>%
    select(-c("Rowname_inmap"))
  colnames(oil_processing_emissions) <- c("Year", "NOx", "SOx", "NH3", "PM2_5", "VOC", "geometry")
  return(oil_processing_emissions)
}
