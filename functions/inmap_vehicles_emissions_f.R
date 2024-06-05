inmap_vehicles_emissions_f <- function(vehicles_emissions_county) {
  attribute_f("inmap_vehicles_emissions_f")
  GIS_matrix <- read.csv(paste0(getwd(), "/inputs/air_quality/GIS_matching_matrix.csv"))
  # Load the primary road dataset
  roads_primary <- primary_roads()
  roads_primary <- add_column(roads_primary, "Length" = 0)
  roads_primary$Length <- st_length(roads_primary$geometry)
  
  # Create intersections of the inmap polygons and the counties
  counties <- tigris::counties() %>%
    select(c("STATEFP", "COUNTYFP", "GEOID", "geometry"))
  inmap_input <- read_sf(paste0(getwd(), "/inputs/air_quality/", "inmap_template.shp")) %>%
    select(c("geometry")) %>%
    add_column("Rowname_inmap" = NA)
  inmap_input$Rowname_inmap <- as.numeric(rownames(inmap_input))
  counties <- st_transform(counties, st_crs(inmap_input$geometry))
  polygon_intersection <- st_intersection(counties, inmap_input)
  
  # Intersect roads with the new polygons
  roads_primary <- st_transform(roads_primary, st_crs(inmap_input$geometry))
  roads_intersection <- st_intersection(roads_primary, polygon_intersection)
  roads_intersection <- roads_intersection[which(st_is(roads_intersection$geometry, c("LINESTRING"))),]
  roads_intersection <- roads_intersection[which(!duplicated(roads_intersection$geometry)),]
  roads_intersection$Length <- as.numeric(st_length(roads_intersection$geometry))
  roads_intersection <- filter(roads_intersection, roads_intersection$Length != 0) %>%
    select(c("Length", "STATEFP", "COUNTYFP", "GEOID", "Rowname_inmap"))
  roads_intersection <- data.frame(cbind(roads_intersection$Length, roads_intersection$STATEFP, roads_intersection$COUNTYFP, roads_intersection$GEOID, roads_intersection$Rowname_inmap))
  colnames(roads_intersection) <- c("Length", "STATEFP", "COUNTYFP", "FIPS", "Rowname_inmap")
  roads_intersection$Length <- as.numeric(roads_intersection$Length)
  roads_intersection <- aggregate(.~STATEFP+COUNTYFP+FIPS+Rowname_inmap, data = roads_intersection, FUN = sum)
  county_road_length <- aggregate(.~STATEFP + COUNTYFP + FIPS, data = select(roads_intersection, -c("Rowname_inmap")), FUN = sum)
  roads_intersection <- left_join(roads_intersection, county_road_length, by = c("STATEFP", "COUNTYFP", "FIPS"))
  roads_intersection$Length.x <- roads_intersection$Length.x/roads_intersection$Length.y
  roads_intersection$FIPS <- as.numeric(roads_intersection$FIPS)
  
  # Import the VMT fraction
  # Load the VMT distribution by county from MOVES
  vmt_distribution <- read.csv(paste0(getwd(), "/inputs/air_quality/MOVES_VMT_allocation_county.csv"), sep = ";") %>%
    filter(activityTypeID == 1) %>%
    select(c("countyID", "roadTypeID", "activity")) %>%
    unique() %>%
    filter(countyID%in%GIS_matrix$FIPS) %>%
    pivot_wider(names_from = roadTypeID, values_from = activity) %>%
    add_column("Total" = 0)
  
  # Normalize the VMT in each type of road for each county
  vmt_distribution$Total <- vmt_distribution$'2' + vmt_distribution$'4' + vmt_distribution$'3' + vmt_distribution$'5'
  vmt_distribution[,2:5] <- vmt_distribution[,2:5]/vmt_distribution$Total
  vmt_distribution <- vmt_distribution %>%
    select(-c("Total")) %>%
    pivot_longer(cols = 2:5, names_to = "road_type", values_to = "frac")
  vmt_distribution$road_type[which(vmt_distribution$road_type == "2" | vmt_distribution$road_type == "4")] <- "Primary"
  vmt_distribution$road_type[which(vmt_distribution$road_type == "3" | vmt_distribution$road_type == "5")] <- "Secondary"
  vmt_distribution <- aggregate(.~countyID + road_type, data = vmt_distribution, FUN = sum)
  
  # Combine the road intersection dataset and the vmt distribution
  colnames(vmt_distribution)[1] <- "FIPS"
  primary_roads_distribution <- left_join(roads_intersection, select(filter(vmt_distribution, vmt_distribution$road_type == "Primary"), c("FIPS", "frac")), by = c("FIPS"))
  colnames(primary_roads_distribution) <- c("STATEFP", "COUNTYFP", "FIPS", "Rowname_inmap", "Road_frac", "County_road_length", "Road_type_frac")
  primary_roads_distribution <- select(primary_roads_distribution, -c("County_road_length"))
  
  
  # Load the secondary road dataset
  states <- unique(GIS_matrix$ST_FIPS)
  fun <- function(i) {
    primary_secondary_roads(state = i, class = "sf", )
  }
  secondary_roads_dataset <- lapply(states, fun)
  secondary_roads_dataset <- do.call(rbind, secondary_roads_dataset)
  secondary_roads_dataset$length <- st_length(secondary_roads_dataset$geometry)
  secondary_roads_dataset <- filter(secondary_roads_dataset, !secondary_roads_dataset$LINEARID%in%roads_primary$LINEARID)
  
  # Intersect secondary roads with the new polygons
  secondary_roads_dataset <- st_transform(secondary_roads_dataset, st_crs(inmap_input$geometry))
  secondary_roads_intersection <- st_intersection(secondary_roads_dataset, polygon_intersection)
  secondary_roads_intersection$length <- as.numeric(st_length(secondary_roads_intersection$geometry))
  secondary_roads_intersection <- filter(secondary_roads_intersection, secondary_roads_intersection$length != 0) %>%
    select(c("length", "STATEFP", "COUNTYFP", "GEOID", "Rowname_inmap"))
  secondary_roads_intersection <- data.frame(cbind(secondary_roads_intersection$length, secondary_roads_intersection$STATEFP, secondary_roads_intersection$COUNTYFP, secondary_roads_intersection$GEOID, secondary_roads_intersection$Rowname_inmap))
  colnames(secondary_roads_intersection) <- c("Length", "STATEFP", "COUNTYFP", "FIPS", "Rowname_inmap")
  secondary_roads_intersection$Length <- as.numeric(secondary_roads_intersection$Length)
  secondary_roads_intersection <- aggregate(.~STATEFP+COUNTYFP+FIPS+Rowname_inmap, data = secondary_roads_intersection, FUN = sum)
  county_secondary_road_length <- aggregate(.~STATEFP + COUNTYFP + FIPS, data = select(secondary_roads_intersection, -c("Rowname_inmap")), FUN = sum)
  secondary_roads_intersection <- left_join(secondary_roads_intersection, county_secondary_road_length, by = c("STATEFP", "COUNTYFP", "FIPS"))
  secondary_roads_intersection$Length.x <- secondary_roads_intersection$Length.x/secondary_roads_intersection$Length.y
  secondary_roads_intersection$FIPS <- as.numeric(secondary_roads_intersection$FIPS)
  secondary_roads_intersection$STATEFP <- as.numeric(secondary_roads_intersection$STATEFP)
  primary_roads_distribution$STATEFP <- as.numeric(primary_roads_distribution$STATEFP)
  
  # Combine the secondary road intersection dataset and the vmt distribution
  secondary_roads_distribution <- left_join(secondary_roads_intersection, select(filter(vmt_distribution, vmt_distribution$road_type == "Secondary"), c("FIPS", "frac")), by = c("FIPS"))
  colnames(secondary_roads_distribution) <- c("STATEFP", "COUNTYFP", "FIPS", "Rowname_inmap", "Road_frac", "County_road_length", "Road_type_frac")
  secondary_roads_distribution <- select(secondary_roads_distribution, -c("County_road_length"))
  
  # Populate with the on-roads emissions dataset
  ### Fill primary and secondary road distributions
  fleet_emissions_county_data <- vehicles_emissions_county
  fleet_emissions_county <- data.frame(Year = fleet_emissions_county_data$Year,
                                      STATEFP = fleet_emissions_county_data$State,
                                      FIPS = fleet_emissions_county_data$FIPS,
                                      NOx = fleet_emissions_county_data$NOx,
                                      SO2 = fleet_emissions_county_data$SO2,
                                      PM25 = fleet_emissions_county_data$Total_PM25 + fleet_emissions_county_data$Brake_PM25 + fleet_emissions_county_data$Tire_PM25,
                                      NH3 = fleet_emissions_county_data$NH3,
                                      VOC = fleet_emissions_county_data$VOC)
  fleet_emissions_county$STATEFP <- as.numeric(fleet_emissions_county$STATEFP)
  years <- unique(fleet_emissions_county$Year)
  primary_roads_distribution <- add_column(primary_roads_distribution, "Year" = NA)
  secondary_roads_distribution <- add_column(secondary_roads_distribution, "Year" = NA)
  fun <- function(i) {
    primary_roads_distribution$Year <- i
    return(primary_roads_distribution)
  }
  primary_roads_emissions <- lapply(years, fun)
  primary_roads_emissions <- do.call(rbind, primary_roads_emissions)
  fun <- function(i) {
    secondary_roads_distribution$Year <- i
    return(secondary_roads_distribution)
  }
  secondary_roads_emissions <- lapply(years, fun)
  secondary_roads_emissions <- do.call(rbind, secondary_roads_emissions)
  
  #Combine road emissions dataset with county allocation factors and emissions dataset
  primary_roads_emissions <- left_join(primary_roads_emissions, fleet_emissions_county, by = c("Year", "FIPS", "STATEFP"), relationship = "many-to-many")
  secondary_roads_emissions <- left_join(secondary_roads_emissions, fleet_emissions_county, by = c("Year", "FIPS", "STATEFP"), relationship = "many-to-many")
  
  # Calculate the emissions in each inmap cell and generate the inmap input file for the fleet emissions
  primary_roads_emissions[,8:12] <- primary_roads_emissions[,8:12]*primary_roads_emissions$Road_type_frac*primary_roads_emissions$Road_frac
  primary_roads_emissions <- select(primary_roads_emissions, c("Year", "Rowname_inmap", "NOx", "SO2", "PM25", "NH3", "VOC"))
  primary_roads_emissions <- aggregate(.~Rowname_inmap + Year, data = primary_roads_emissions, FUN = sum)
  secondary_roads_emissions[,8:12] <- secondary_roads_emissions[,8:12]*secondary_roads_emissions$Road_type_frac*secondary_roads_emissions$Road_frac
  secondary_roads_emissions <- select(secondary_roads_emissions, c("Year", "Rowname_inmap", "NOx", "SO2", "PM25", "NH3", "VOC"))
  secondary_roads_emissions <- aggregate(.~Rowname_inmap + Year, data = secondary_roads_emissions, FUN = sum)
  roads_emissions <- rbind(primary_roads_emissions, secondary_roads_emissions)
  #roads_emissions <- select(roads_emissions, c("Year", "Rowname_inmap", "NOx", "SO2", "PM25", "NH3", "VOC"))
  #roads_emissions <- aggregate(.~Rowname_inmap + Year, data = roads_emissions, FUN = sum)
  roads_emissions$Rowname_inmap <- as.numeric(roads_emissions$Rowname_inmap)
  roads_emissions <- left_join(roads_emissions, inmap_input, by = "Rowname_inmap") %>%
    select(-c("Rowname_inmap"))
  colnames(roads_emissions) <- c("Year", "NOx", "SOx", "PM2_5", "NH3", "VOC", "geometry")
  return(roads_emissions)
}
