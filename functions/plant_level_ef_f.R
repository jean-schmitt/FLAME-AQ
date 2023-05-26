plant_level_ef_f <- function() { 
  plant_emissions_dataset <- read.csv("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_FLAME-AQ/inputs/air_quality/egrid_plant_emissions.csv")
  plant_emissions_dataset <- add_column(plant_emissions_dataset, "FIPS" = NA)
  GIS_matching_matrix <- read.csv(paste0(getwd(), "/inputs/air_quality/GIS_matching_matrix.csv"))
  plant_emissions_dataset <- filter(plant_emissions_dataset, plant_emissions_dataset$Plant.FIPS.state.code%in%unique(GIS_matching_matrix$ST_FIPS))
  for (i in 1:dim(plant_emissions_dataset)[1]) {
    plant_emissions_dataset$FIPS[i] <- GIS_matching_matrix$FIPS[which(GIS_matching_matrix$ST_FIPS == plant_emissions_dataset$Plant.FIPS.state.code[i] & GIS_matching_matrix$CY_FIPS == plant_emissions_dataset$Plant.FIPS.county.code[i])]
  }
  # ef_elec contains the emission factors dfor each county
  ef_elec <- data.frame(matrix(data = NA, nrow = 0, ncol = 7))
  colnames(ef_elec) <- c("County", "eGrid_region", "Fuel", "Pollutant", "Emission_rate", "Unit", "Fraction_GEA")
  # power_generation_county contains the power generation data in each county and the weight of each county in it's FIPS power generation
  power_generation_county <- data.frame(matrix(data = NA, nrow = 0, ncol = 4))
  colnames(power_generation_county) <- c("County", "Cambium_region", "Power", "Fraction_GEA")
  #egrid_regions <- unique(GIS_matching_matrix$eGRID_subregion)
  counties <- unique(GIS_matching_matrix$FIPS)
  fuels <- c("GAS", "OIL", "COAL", "BIOMASS")
  for (i in counties) {
    egrid_region <- GIS_matching_matrix$eGRID_subregion[which(GIS_matching_matrix$FIPS == i)]
    cambium_region <- GIS_matching_matrix$CAMBIUM_GEA[which(GIS_matching_matrix$FIPS == i)]
    counties_in_region <- GIS_matching_matrix$FIPS[which(GIS_matching_matrix$CAMBIUM_GEA == cambium_region)]
    cambium_region_generation <- sum(plant_emissions_dataset$Plant.annual.net.generation..MWh.[which(plant_emissions_dataset$FIPS%in%counties_in_region)], na.rm = TRUE)
    county_generation <- sum(plant_emissions_dataset$Plant.annual.net.generation..MWh.[which(plant_emissions_dataset$FIPS == i)])
    if (cambium_region_generation != 0) {
      power_generation_county[nrow(power_generation_county)+1,] <- c(i, cambium_region, county_generation, county_generation/cambium_region_generation)
    }
    for (j in fuels) {
      total_generation <- sum(plant_emissions_dataset$Plant.annual.net.generation..MWh.[which(plant_emissions_dataset$Plant.primary.fuel.category == j & plant_emissions_dataset$FIPS == i)], na.rm = TRUE)
      total_generation_region <- sum(plant_emissions_dataset$Plant.annual.net.generation..MWh.[which(plant_emissions_dataset$Plant.primary.fuel.category == j & plant_emissions_dataset$FIPS%in%counties_in_region)], na.rm = TRUE)
      emissions_NOx <- sum(plant_emissions_dataset$Plant.annual.NOx.emissions..tons.[which(plant_emissions_dataset$FIPS == i & plant_emissions_dataset$Plant.primary.fuel.category == j)], na.rm = TRUE)
      emissions_SOx <- sum(plant_emissions_dataset$Plant.annual.SO2.emissions..tons.[which(plant_emissions_dataset$FIPS == i & plant_emissions_dataset$Plant.primary.fuel.category == j)], na.rm = TRUE)
      if (total_generation <= 0) {
        #ef_NOx <- 0
        #ef_SOx <- 0
      } else {
        ef_NOx <- emissions_NOx*1e3/total_generation
        ef_SOx <- emissions_SOx*1e3/total_generation
        ef_elec[nrow(ef_elec)+1,] <- c(i, cambium_region, j, "NOx", ef_NOx, "g/kWh", total_generation/total_generation_region)
        ef_elec[nrow(ef_elec)+1,] <- c(i, cambium_region, j, "SO2", ef_SOx, "g/kWh", total_generation/total_generation_region)
      }
    }
  }
  power_generation_county <- filter(power_generation_county, !is.na(power_generation_county$Fraction_GEA) & power_generation_county$Power >0)
  power_generation_county$Power <- as.numeric(power_generation_county$Power)
  power_generation_county$Fraction_GEA <- as.numeric(power_generation_county$Fraction_GEA)
  ef_elec$Fraction_GEA <- as.numeric(ef_elec$Fraction_GEA)
  # Fuel correspondence
  ef_elec$Fuel[which(ef_elec$Fuel == "COAL")] <- "coal"
  ef_elec$Fuel[which(ef_elec$Fuel == "OIL")] <- "oil"
  ef_elec$Fuel[which(ef_elec$Fuel == "GAS")] <- "ng"
  ef_elec$Fuel[which(ef_elec$Fuel == "BIOMASS")] <- "biomass"
}
